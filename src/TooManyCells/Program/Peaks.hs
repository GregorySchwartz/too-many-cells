{- TooManyCells.Program.Peaks
Gregory W. Schwartz

Peaks entry point for command line program.
-}

module TooManyCells.Program.Peaks where

-- Remote
import BirchBeer.Load (loadLabelData)
import BirchBeer.Types (Delimiter (..), LabelFile (..), Label (..))
import Options.Generic
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
peaksMain :: Options -> IO ()
peaksMain opts = do
  let readOrErr err = fromMaybe (error err) . readMaybe
      fragmentPaths' = unHelpful . fragmentsPath $ opts
      prior'    = PriorPath
                . fromMaybe (error "\nRequires a previous run to get the clusters.")
                . unHelpful
                . prior
                $ opts
      genome' =
        GenomeFile . fromMaybe "./human.hg38.genome" . unHelpful . genome $ opts
      genomecovCommand' = GenomecovCommand
                   . fromMaybe "bedtools genomecov -i %s -g %s -scale %f -bg > %s"
                   . unHelpful
                   . genomecovCommand
                   $ opts
      peakCommand' = PeakCommand
                   . fromMaybe "macs2 callpeak --nomodel --nolambda -p 0.001 -B -t %s -n %s --outdir %s"
                   . unHelpful
                   . peakCallCommand
                   $ opts
      delimiter'    = Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
      labelsFile'   = fmap LabelFile . unHelpful . labelsFile $ opts
      bedgraphFlag' = BedGraphFlag . unHelpful . bedgraph $ opts
      allNodesFlag' = AllNodesFlag . unHelpful . allNodes $ opts
      peakNodes'    = PeakNodes . ISet.fromList . unHelpful . peakNode $ opts
      peakNodesLabels' = PeakNodesLabels
                       . IMap.fromList
                       . fmap (L.over L._2 (Set.fromList . fmap Label))
                       . fmap (readOrErr "\nCannot read --peak-nodes-labels")
                       . unHelpful
                       . peakNodeLabels
                       $ opts
      output' = OutputDirectory . fromMaybe "out" . unHelpful . output $ opts
      clInput = (FP.</> "cluster_list.json") . unPriorPath $ prior'
      treeInput = (FP.</> "cluster_tree.json") . unPriorPath $ prior'

  when (isNothing . unHelpful . genome $ opts) $
    hPutStrLn stderr "--genome file not specified, using ./human.hg38.genome ..."

  cr <- loadClusterResultsFiles clInput treeInput :: IO ClusterResults
  labelMap <- mapM (loadLabelData delimiter') labelsFile'

  unless (unHelpful $ skipFragments opts) $ do
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
