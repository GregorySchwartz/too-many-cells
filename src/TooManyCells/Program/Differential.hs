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

module TooManyCells.Program.Differential where

-- Remote
import BirchBeer.ColorMap
import BirchBeer.Interactive
import BirchBeer.Load
import BirchBeer.MainDiagram
import BirchBeer.Plot
import BirchBeer.Types
import BirchBeer.Utility
import Control.Monad (when, unless, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bool (bool)
import Data.Colour.SRGB (sRGB24read)
import Data.Matrix.MatrixMarket (readMatrix, writeMatrix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Tree (Tree (..))
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Math.Clustering.Spectral.Sparse (b1ToB2, B1 (..), B2 (..))
import Options.Generic
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe, readEither)
import TextShow (showt)
import qualified "find-clumpiness" Types as Clump
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Colour.Palette.BrewerSet as D
import qualified Data.Colour.Palette.Harmony as D
import qualified Data.Csv as CSV
import qualified Data.GraphViz as G
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified H.Prelude as H
import qualified Plots as D
import qualified System.Directory as FP
import qualified System.FilePath as FP
import qualified System.ProgressBar as Progress

-- Local
import TooManyCells.Differential.Differential
import TooManyCells.Differential.Types
import TooManyCells.Diversity.Diversity
import TooManyCells.Diversity.Load
import TooManyCells.Diversity.Plot
import TooManyCells.Diversity.Types
import TooManyCells.File.Types
import TooManyCells.MakeTree.Clumpiness
import TooManyCells.MakeTree.Cluster
import TooManyCells.MakeTree.Load
import TooManyCells.MakeTree.Plot
import TooManyCells.MakeTree.Print
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Load
import TooManyCells.Matrix.Preprocess
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Paths.Distance
import TooManyCells.Paths.Plot
import TooManyCells.Paths.Types

-- | Differential path.
differentialMain :: Options -> IO ()
differentialMain opts = do
    let readOrErr err = fromMaybe (error err) . readMaybe
        delimiter'     =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        labelsFile' =
            fmap LabelFile . unHelpful . labelsFile $ opts
        nodes'    =
          DiffNodes . readOrErr "Cannot read --nodes." . unHelpful . nodes $ opts
        prior'    = PriorPath
                  . fromMaybe (error "\nRequires a previous run to get the graph.")
                  . unHelpful
                  . prior
                  $ opts
        topN'     = TopN . fromMaybe 100 . unHelpful . topN $ opts
        features'    = fmap Feature . unHelpful . features $ opts
        aggregate' = Aggregate . unHelpful . aggregate $ opts
        labels'   = fmap ( DiffLabels
                         . L.over L.both ( (\x -> bool (Just x) Nothing . Set.null $ x)
                                         . Set.fromList
                                         . fmap Label
                                         )
                         . readOrErr "Cannot read --labels."
                         )
                  . unHelpful
                  . labels
                  $ opts
        (combined1, combined2) = combineNodesLabels nodes' labels'
        plotOutputR = fromMaybe "out.pdf" . unHelpful . plotOutput $ opts

    processedSc <- loadAllSSM opts

    labelMap <- mapM (loadLabelData delimiter') $ labelsFile'

    let clInput = (FP.</> "cluster_list.json") . unPriorPath $ prior'
        treeInput = (FP.</> "cluster_tree.json") . unPriorPath $ prior'

        cr :: IO ClusterResults
        cr = loadClusterResultsFiles clInput treeInput

    gr <- fmap (treeToGraph . _clusterDend) cr

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
      case features' of
        [] -> do
          case nodes' of
            (DiffNodes ([], [])) -> do
              let res = getAllDEGraphKruskalWallis
                          topN'
                          labelMap
                          (maybe (DiffLabels (Nothing, Nothing)) id labels')
                          (extractSc processedSc)
                      $ gr

              H.io . B.putStrLn . getAllDEStringKruskalWallis $ res
            (DiffNodes ([], _)) -> error "Need other nodes to compare with. If every node should be compared to all other nodes using Mann-Whitney U, use \"([], [])\"."
            (DiffNodes (_, [])) -> error "Need other nodes to compare with. If every node should be compared to all other nodes using Mann-Whitney U, use \"([], [])\"."
            _ -> do
              res <- getDEGraph
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
                       False
                       aggregate'
                       labelMap
                       (extractSc processedSc)
                       combined1
                       combined2
                       features'
                       gr
          [r| suppressMessages(write.csv(diffPlot_hs[[2]], file = outputCsvR_hs, row.names = FALSE, quote = FALSE)) |]
          [r| suppressMessages(ggsave(diffPlot_hs[[1]], file = plotOutputR_hs)) |]

          let normOutputR = FP.replaceBaseName
                              plotOutputR
                             (FP.takeBaseName plotOutputR <> "_scaled")
              normOutputCsvR = FP.replaceExtension normOutputR ".csv"

          diffNormPlot <- getSingleDiff
                            True
                            aggregate'
                            labelMap
                            (extractSc processedSc)
                            combined1
                            combined2
                            features'
                            gr
          [r| suppressMessages(write.csv(diffNormPlot_hs[[2]], file = normOutputCsvR_hs, row.names = FALSE, quote = FALSE)) |]
          [r| suppressMessages(ggsave(diffNormPlot_hs[[1]], file = normOutputR_hs)) |]

          return ()
