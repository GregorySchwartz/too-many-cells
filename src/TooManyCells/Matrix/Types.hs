{- TooManyCells.Matrix.Types
Gregory W. Schwartz

Collects the types used in the program concerning the matrix.
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.Matrix.Types where

-- Remote
import Data.Monoid (Monoid (..), mempty, mappend)
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Colour.SRGB (Colour (..), RGB (..), toSRGB)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Sparse (ShowB)
import Math.Clustering.Hierarchical.Spectral.Types (ClusteringTree, ClusteringVertex)
import Math.Modularity.Types (Q (..))
import qualified Data.Aeson as A
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H

-- Local

-- Basic
newtype Cell = Cell
    { unCell :: Text
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON, A.FromJSON)
newtype Cols            = Cols { unCols :: [Double] }
newtype Gene            = Gene { unGene :: Text } deriving (Eq, Ord, Read, Show)
newtype CellWhitelist = CellWhitelist
    { unCellWhitelist :: Set.Set Cell
    } deriving (Eq,Ord,Read,Show)
newtype PCAVar = PCAVar
    { unPCAVar :: Double
    } deriving (Eq,Ord,Read,Show)
newtype NoFilterFlag = NoFilterFlag
    { unNoFilterFlag :: Bool
    } deriving (Read,Show)
newtype X = X
    { unX :: Double
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
newtype Y = Y
    { unY :: Double
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
newtype RMat s          = RMat { unRMat :: R.SomeSEXP s }
newtype RMatObsRow s    = RMatObsRow { unRMatObsRow :: R.SomeSEXP s }
newtype RMatFeatRow s   = RMatFeatRow { unRMatFeatRow :: R.SomeSEXP s }
newtype RMatScaled s    = RMatScaled { unRMatScaled :: R.SomeSEXP s }
newtype Row = Row
    { unRow :: Int
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON,A.FromJSON)
newtype Rows            = Rows { unRows :: [Double] }
newtype Vals            = Vals { unVals :: [Double] }
newtype MatObsRow = MatObsRow
    { unMatObsRow :: S.SpMatrix Double
    } deriving (Show)

-- Advanced
data SingleCells = SingleCells { matrix :: MatObsRow
                               , rowNames :: Vector Cell
                               , colNames :: Vector Gene
                               , projections :: Vector (X, Y)
                               }
                     deriving (Show)

data CellInfo = CellInfo
    { barcode :: Cell
    , cellRow :: Row
    , projection :: (X, Y)
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON,A.FromJSON)

data NormType = B1Norm | WishboneNorm | NoneNorm deriving (Read, Show)

instance (Generic a) => Generic (Vector a)

instance Monoid MatObsRow where
    mempty  = MatObsRow $ S.zeroSM 0 0
    mappend (MatObsRow x) (MatObsRow y) = MatObsRow $ S.vertStackSM x y

instance Monoid SingleCells where
    mempty  = SingleCells { matrix = mempty
                          , rowNames = V.empty
                          , colNames = V.empty
                          , projections = V.empty
                          }
    mappend x y =
        SingleCells { matrix      = mappend (matrix x) (matrix y)
                    , rowNames    = (V.++) (rowNames x) (rowNames y)
                    , colNames    = colNames x
                    , projections = (V.++) (projections x) (projections y)
                    }
