{- TooManyCells.Motifs.Types
Gregory W. Schwartz

Collects the motif types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Motifs.Types where

-- Remote
import qualified Data.Text as T
import qualified Turtle as TU

-- Local

-- Basic

newtype DiffFile = DiffFile { unDiffFile :: TU.FilePath }
newtype OutputPath = OutputPath { unOutputPath :: TU.FilePath }
newtype GenomeFile = GenomeFile { unGenomeFile :: T.Text }
newtype MotifCommand = MotifCommand { unMotifCommand :: String }
newtype TopN = TopN { unTopN :: Int }
newtype Node = Node { unNode :: Int } deriving (Eq, Ord)
