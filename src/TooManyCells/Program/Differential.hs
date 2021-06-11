{- TooManyCells.Program.Differential
Gregory W. Schwartz

Differential entry point for command line program.
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.Differential where

-- Remote
import BirchBeer.Load
import BirchBeer.Types
import BirchBeer.Utility
import Control.Monad (when, join)
import Control.Monad.Trans (liftIO)
import Data.Bool (bool)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Language.R as R
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as Set
import qualified H.Prelude as H
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.Program.Options
import TooManyCells.Differential.Differential
import TooManyCells.Differential.Types
import TooManyCells.MakeTree.Types
import TooManyCells.MakeTree.Utility
import TooManyCells.File.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Program.LoadMatrix
import TooManyCells.MakeTree.Load

-- | Differential path.
differentialMain :: Subcommand -> IO ()
differentialMain sub@(DifferentialCommand opts) = do
    let readOrErr err = fromMaybe (error err) . readMaybe
        delimiter'        = Delimiter
                          . (delimiter :: LoadMatrixOptions -> Char)
                          . (loadMatrixOptions :: Differential -> LoadMatrixOptions)
                          $ opts
        labelsFile' =
            fmap LabelFile . (labelsFile :: Differential -> Maybe String) $ opts
        nodes'    =
          DiffNodes . readOrErr "Cannot read --nodes." . nodes $ opts
        prior'    = PriorPath . (prior :: Differential -> String) $ opts
        topN'     = TopN . (topN :: Differential -> Int) $ opts
        features'    = fmap Feature . features $ opts
        aggregate' = Aggregate . aggregate $ opts
        separateNodes' = SeparateNodes . plotSeparateNodes $ opts
        separateLabels' = SeparateLabels . plotSeparateLabels $ opts
        violinFlag' = ViolinFlag . plotViolin $ opts
        noOutlierFlag' = NoOutlierFlag . plotNoOutlier $ opts
        updateTreeRows' = UpdateTreeRowsFlag . not . (noUpdateTreeRows :: Differential -> Bool) $ opts
        edger' = Edger . edger $ opts
        subsampleGroups' = fmap Subsample . subsampleGroups $ opts
        seed' = Seed . seed $ opts
        labels'   = fmap ( DiffLabels
                         . L.over L.both ( (\x -> bool (Just x) Nothing . Set.null $ x)
                                         . Set.fromList
                                         . fmap Label
                                         )
                         . readOrErr "Cannot read --labels."
                         )
                  . labels
                  $ opts
        (combined1, combined2) = combineNodesLabels nodes' labels'
        plotOutputR = plotOutput opts

    when (isNothing labelsFile' && isJust labels') $
      hPutStrLn stderr "Warning: labels requested with no label file, ignoring labels..."

    scRes <- loadAllSSM sub
           $ (loadMatrixOptions :: Differential -> LoadMatrixOptions) opts
    let processedSc = fmap fst scRes
        customLabelMap = join . fmap snd $ scRes

    labelMap <- if isJust labelsFile'
                  then mapM (loadLabelData delimiter') $ labelsFile'
                  else return customLabelMap

    let clInput = (FP.</> "cluster_list.json") . unPriorPath $ prior'
        treeInput = (FP.</> "cluster_tree.json") . unPriorPath $ prior'

        cr :: IO ClusterResults
        cr = loadClusterResultsFiles clInput treeInput

    gr <- treeToGraph
        . updateTreeRowBool updateTreeRows' processedSc
        . _clusterDend
      <$> cr

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
      case features' of
        [] -> do
          case nodes' of
            (DiffNodes ([], [])) -> do
              res <- H.io
                       . getAllDEGraphKruskalWallis
                           seed'
                           subsampleGroups'
                           topN'
                           labelMap
                           (maybe (DiffLabels (Nothing, Nothing)) id labels')
                           (extractSc processedSc)
                       $ gr

              H.io . B.putStrLn . getAllDEStringKruskalWallis $ res
            (DiffNodes ([], _)) -> error "Need other nodes to compare with. If every node should be compared to all other nodes using Mann-Whitney U, use \"([], [])\"."
            (DiffNodes (_, [])) -> error "Need other nodes to compare with. If every node should be compared to all other nodes using Mann-Whitney U, use \"([], [])\"."
            _ -> do
              if not $ unEdger edger'
                then do
                  res <- H.io
                       $ getDEGraphKruskalWallis
                           seed'
                           subsampleGroups'
                           topN'
                           labelMap
                           (extractSc processedSc)
                           combined1
                           combined2
                           gr

                  H.io . B.putStrLn . getDEStringKruskalWallis $ res
                else do
                  res <- getDEGraph
                          seed'
                          subsampleGroups'
                          topN'
                          labelMap
                          (extractSc processedSc)
                          combined1
                          combined2
                          gr

                  H.io . B.putStrLn . getDEString $ res
        _ -> do
          let outputCsvR = FP.replaceExtension plotOutputR ".csv"

          diffPlot <- getSingleDiff
                       seed'
                       subsampleGroups'
                       False
                       violinFlag'
                       noOutlierFlag'
                       aggregate'
                       separateNodes'
                       separateLabels'
                       labelMap
                       (extractSc processedSc)
                       combined1
                       combined2
                       features'
                       gr
          [H.r| suppressMessages(write.csv(diffPlot_hs[[2]], file = outputCsvR_hs, row.names = FALSE, quote = FALSE)) |]
          [H.r| suppressMessages(ggsave(diffPlot_hs[[1]], file = plotOutputR_hs)) |]

          let normOutputR = FP.replaceBaseName
                              plotOutputR
                             (FP.takeBaseName plotOutputR <> "_scaled")
              normOutputCsvR = FP.replaceExtension normOutputR ".csv"

          diffNormPlot <- getSingleDiff
                            seed'
                            subsampleGroups'
                            True
                            violinFlag'
                            noOutlierFlag'
                            aggregate'
                            separateNodes'
                            separateLabels'
                            labelMap
                            (extractSc processedSc)
                            combined1
                            combined2
                            features'
                            gr
          [H.r| suppressMessages(write.csv(diffNormPlot_hs[[2]], file = normOutputCsvR_hs, row.names = FALSE, quote = FALSE)) |]
          [H.r| suppressMessages(ggsave(diffNormPlot_hs[[1]], file = normOutputR_hs)) |]

          return ()
differentialMain _ = error "Wrong path in differential, contact Gregory Schwartz for this error."
