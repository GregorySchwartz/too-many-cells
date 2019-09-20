{- TooManyCells.Program.Peaks
Gregory W. Schwartz

Peaks entry point for command line program.
-}

module TooManyCells.Program.Peaks where

-- Remote
import Options.Generic
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe, isNothing)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Data.IntSet as ISet
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.File.Types
import TooManyCells.MakeTree.Load
import TooManyCells.MakeTree.Types
import TooManyCells.Peaks.ClusterPeaks
import TooManyCells.Peaks.Types
import TooManyCells.Program.Options
import TooManyCells.Program.LoadMatrix
import TooManyCells.Program.Utility

-- | Peaks path.
peaksMain :: Options -> IO ()
peaksMain opts = do
  let readOrErr err = fromMaybe (error err) . readMaybe
      matrixPaths'      = unHelpful . matrixPath $ opts
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
                   . fromMaybe "macs2 callpeak --nomodel -B -t %s -n %s --outdir %s"
                   . unHelpful
                   . peakCallCommand
                   $ opts
      bedgraphFlag' = BedGraphFlag . unHelpful . bedgraph $ opts
      allNodesFlag' = AllNodesFlag . unHelpful . allNodes $ opts
      peakNodes'    = PeakNodes . ISet.fromList . unHelpful . peakNode $ opts
      output' = OutputDirectory . fromMaybe "out" . unHelpful . output $ opts
      clInput = (FP.</> "cluster_list.json") . unPriorPath $ prior'
      treeInput = (FP.</> "cluster_tree.json") . unPriorPath $ prior'

  when (isNothing . unHelpful . genome $ opts) $
    hPutStrLn stderr "--genome file not specified, using ./human.hg38.genome"

  cr <- loadClusterResultsFiles clInput treeInput :: IO ClusterResults

  unless (unHelpful $ skipFragments opts) $
    mapM_
      ( (=<<) ( saveClusterFragments
                  output'
                  bedgraphFlag'
                  genome'
                  genomecovCommand'
                  allNodesFlag'
                  peakNodes'
                  cr
              )
      . getMatrixFileType
      )
      matrixPaths'

  peakCallFiles peakCommand' genome' output'
