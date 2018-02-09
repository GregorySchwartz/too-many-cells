{- TooManyCells.Diversity.Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}

module TooManyCells.Diversity.Types where

-- Remote
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types


-- Basic
newtype Order       = Order { unOrder :: Double } deriving (Eq, Ord, Read, Show)
newtype Size        = Size { unSize :: Integer } deriving (Eq, Ord, Read, Show)
newtype Start       = Start { unStart :: Integer } deriving (Eq, Ord, Read, Show)
newtype Interval    = Interval { unInterval :: Integer } deriving (Eq, Ord, Read, Show)
newtype End         = End { unEnd :: Integer } deriving (Eq, Ord, Read, Show)
newtype Diversity   = Diversity
    { unDiversity :: Double
    } deriving (Eq,Ord,Read,Show)
newtype Chao1       = Chao1
    { unChao1 :: Double
    } deriving (Eq,Ord,Read,Show)
newtype Rarefaction = Rarefaction
    { unRarefaction :: [(X, Y)]
    } deriving (Eq,Ord,Read,Show)
newtype Population = Population
    { unPopulation :: Map.Map Cluster (Seq.Seq Cell)
    } deriving (Eq, Ord, Read, Show)

-- Advanced
data PopulationDiversity = PopulationDiversity
    { popLabel       :: Label
    , popDiversity   :: Diversity
    , popChao1       :: Chao1
    , popRarefaction :: Rarefaction
    }
