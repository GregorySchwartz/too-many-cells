{- TooManyCells.Diversity.Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Diversity.Types where

-- Remote
import BirchBeer.Types
import Data.List (intercalate)
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types


-- Basic
newtype Size        = Size { unSize :: Integer } deriving (Eq, Ord, Read, Show)
newtype Start       = Start { unStart :: Integer } deriving (Eq, Ord, Read, Show)
newtype Interval    = Interval { unInterval :: Integer } deriving (Eq, Ord, Read, Show)
newtype End         = End { unEnd :: Integer } deriving (Eq, Ord, Read, Show)
newtype Species     = Species { unSpecies :: T.Text } deriving (Eq, Ord, Read, Show)
newtype Diversity   = Diversity
    { unDiversity :: Double
    } deriving (Eq,Ord,Read,Show,Generic)
newtype Chao1       = Chao1
    { unChao1 :: Double
    } deriving (Eq,Ord,Read,Show,Generic)
newtype Rarefaction = Rarefaction
    { unRarefaction :: [(X, Y)]
    } deriving (Eq,Ord,Read,Show,Generic)
newtype Population = Population
    { unPopulation :: Map.Map Species (Seq.Seq Cell)
    } deriving (Eq, Ord, Read, Show, Generic)

-- Advanced
data PopulationDiversity = PopulationDiversity
    { popLabel       :: Label
    , popDiversity   :: Diversity
    , popChao1       :: Chao1
    , popRarefaction :: Rarefaction
    }
    deriving (Generic)

instance CSV.ToField Label where
  toField = B.pack . T.unpack . unLabel
instance CSV.ToField Diversity where
  toField = B.pack . show . unDiversity
instance CSV.ToField Chao1 where
  toField = B.pack . show . unChao1
instance CSV.ToField Rarefaction where
  toField = B.pack
          . intercalate "/"
          . fmap (\(X !x, Y !y) -> show x <> "-" <> show y)
          . unRarefaction

instance CSV.ToNamedRecord PopulationDiversity where
  toNamedRecord (PopulationDiversity l d _ r) =
    CSV.namedRecord
      ["label" CSV..= l, "diversity" CSV..= d, "rarefaction" CSV..= r]

instance CSV.DefaultOrdered PopulationDiversity where
  headerOrder _ = CSV.header ["label", "diversity", "rarefaction"]
