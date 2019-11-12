{- TooManyCells.Peaks.Types
Gregory W. Schwartz

Collects the peak types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Peaks.Types where

-- Remote
import BirchBeer.Types (Label)
import System.IO (Handle (..))
import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.Set as Set

-- Local

-- Basic
newtype PeakCommand  = PeakCommand { unPeakCommand :: String }
newtype GenomecovCommand  = GenomecovCommand { unGenomecovCommand :: String }
newtype GenomeFile  = GenomeFile { unGenomeFile :: String }
newtype BedGraphFlag = BedGraphFlag { unBedGraphFlag :: Bool }
newtype AllNodesFlag = AllNodesFlag { unAllNodesFlag :: Bool }
newtype ClusterHandleMap = ClusterHandleMap { unClusterHandleMap :: IMap.IntMap Handle }
newtype PeakNodes = PeakNodes { unPeakNodes :: ISet.IntSet }
newtype PeakNodesLabels =
          PeakNodesLabels { unPeakNodesLabels :: IMap.IntMap (Set.Set Label) }
