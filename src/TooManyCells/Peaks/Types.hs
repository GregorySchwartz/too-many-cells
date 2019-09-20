{- TooManyCells.Peaks.Types
Gregory W. Schwartz

Collects the peak types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Peaks.Types where

-- Remote
import qualified Data.IntSet as ISet

-- Local

-- Basic
newtype PeakCommand  = PeakCommand { unPeakCommand :: String }
newtype GenomecovCommand  = GenomecovCommand { unGenomecovCommand :: String }
newtype GenomeFile  = GenomeFile { unGenomeFile :: String }
newtype BedGraphFlag = BedGraphFlag { unBedGraphFlag :: Bool }
newtype AllNodesFlag = AllNodesFlag { unAllNodesFlag :: Bool }
newtype PeakNodes = PeakNodes { unPeakNodes :: ISet.IntSet }
