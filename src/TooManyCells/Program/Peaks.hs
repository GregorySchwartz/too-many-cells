{- TooManyCells.Program.Peaks
Gregory W. Schwartz

Peaks entry point for command line program.
-}

{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.Peaks where

-- Remote
import BirchBeer.Load (loadLabelData)
import BirchBeer.Types (Delimiter (..), LabelFile (..), Label (..))
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe, isNothing)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Control.Lens as L
import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.File.Types
import TooManyCells.MakeTree.Load
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types
import TooManyCells.Peaks.ClusterPeaks
import TooManyCells.Peaks.Types
import TooManyCells.Program.Options
import TooManyCells.Program.LoadMatrix
import TooManyCells.Program.Utility

-- | Peaks path.
peaksMain :: Subcommand -> IO ()
peaksMain (PeaksCommand opts) = do
  let readOrErr err = fromMaybe (error err) . readMaybe
      fragmentPaths' = fragmentsPath opts
      prior'    = PriorPath
                . fromMaybe (error "\nRequires a previous run to get the clusters.")
                . (prior :: Peaks -> Maybe String)
                $ opts
      genome' = GenomeFile . genome $ opts
      genomecovCommand' = GenomecovCommand . genomecovCommand $ opts
      peakCommand' = PeakCommand . peakCallCommand $ opts
      delimiter'    = Delimiter . (delimiter :: Peaks -> Char) $ opts
      labelsFile'   = fmap LabelFile . (labelsFile :: Peaks -> Maybe String) $ opts
      bedgraphFlag' = BedGraphFlag . bedgraph $ opts
      allNodesFlag' = AllNodesFlag . allNodes $ opts
      peakNodes'    = PeakNodes . ISet.fromList . peakNode $ opts
      peakNodesLabels' = PeakNodesLabels
                       . IMap.fromList
                       . fmap (L.over L._2 (Set.fromList . fmap Label))
                       . fmap (readOrErr "\nCannot read --peak-nodes-labels")
                       . peakNodeLabels
                       $ opts
      output' = OutputDirectory . (output :: Peaks -> String) $ opts
      clInput = (FP.</> "cluster_list.json") . unPriorPath $ prior'
      treeInput = (FP.</> "cluster_tree.json") . unPriorPath $ prior'

  hPutStrLn stderr ("Using --genome " <> genome opts <> " ...")

  cr <- loadClusterResultsFiles clInput treeInput :: IO ClusterResults
  labelMap <- mapM (loadLabelData delimiter') labelsFile'

  unless (skipFragments opts) $ do
    hPutStrLn stderr "Splitting fragment file by cluster ..."
    mapM_
      ( (=<<) ( saveClusterFragments
                  output'
                  bedgraphFlag'
                  genome'
                  genomecovCommand'
                  labelMap
                  allNodesFlag'
                  peakNodes'
                  peakNodesLabels'
                  cr
              )
      . getMatrixFileType
      )
      fragmentPaths'

  hPutStrLn stderr "Calling peaks ..."
  peakCallFiles peakCommand' genome' output' =<< mapM getMatrixFileType fragmentPaths'
peaksMain _ = error "Wrong path in peaks, contact Gregory Schwartz for this error."
