{- TooManyCells.Matrix.Types
Gregory W. Schwartz

Collects the types used in the program concerning the matrix.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module TooManyCells.Matrix.Types where

-- Remote
import Control.DeepSeq (NFData (..))
import Data.Monoid (Monoid (..), mempty)
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
import TextShow (TextShow (..))
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Map as Map
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
newtype Feature            = Feature { unFeature :: Text } deriving (Eq, Ord, Read, Show, Generic)
instance NFData Feature
newtype FeatureColumn   = FeatureColumn { unFeatureColumn :: Int }
newtype BinSize = BinSize { unBinSize :: Int}
newtype CellWhitelist = CellWhitelist
    { unCellWhitelist :: Set.Set Cell
    } deriving (Eq,Ord,Read,Show)
newtype PCADim = PCADim
    { unPCADim :: Int
    } deriving (Eq,Ord,Read,Show)
newtype NoFilterFlag = NoFilterFlag
    { unNoFilterFlag :: Bool
    } deriving (Read,Show)
newtype ShiftPositiveFlag = ShiftPositiveFlag
    { unShiftPositiveFlag :: Bool
    } deriving (Read,Show)
newtype FilterThresholds = FilterThresholds
    { unFilterThresholds :: (Double, Double)
    } deriving (Read,Show)
newtype MatrixTranspose = MatrixTranspose
    { unMatrixTranspose :: Bool
    } deriving (Read,Show)
newtype X = X
    { unX :: Double
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
newtype Y = Y
    { unY :: Double
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
newtype ProjectionMap = ProjectionMap
  { unProjectionMap :: Map.Map Cell (X, Y)
  }
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
instance (Generic a) => Generic (Vector a)
instance Semigroup MatObsRow where
    (<>) (MatObsRow x) (MatObsRow y) = MatObsRow $ S.vertStackSM x y
instance Monoid MatObsRow where
    mempty = MatObsRow $ S.zeroSM 0 0

-- Advanced
data SingleCells = SingleCells { _matrix :: MatObsRow
                               , _rowNames :: Vector Cell
                               , _colNames :: Vector Feature
                               }
                     deriving (Show)
L.makeLenses ''SingleCells

instance Semigroup SingleCells where
    (<>) x y =
        SingleCells { _matrix      = mappend (_matrix x) (_matrix y)
                    , _rowNames    = (V.++) (_rowNames x) (_rowNames y)
                    , _colNames    = _colNames x
                    }
instance Monoid SingleCells where
    mempty  = SingleCells { _matrix = mempty
                          , _rowNames = V.empty
                          , _colNames = V.empty
                          }

data CellInfo = CellInfo
    { _barcode :: Cell
    , _cellRow :: Row
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON,A.FromJSON)
L.makeLenses ''CellInfo

data Range = Range {_rangeLabel :: Text, _rangeUpperBound :: Int, _rangeLowerBound :: Int }
             deriving (Eq, Ord, Read, Show)
L.makeFields ''Range
instance TextShow Range where
  showt (Range l lb ub)= mconcat [l, ":", showt lb, "-", showt ub]

data Bin = Bin { _binLabel :: Text, _binUpperBound :: Int, _binLowerBound :: Int }
             deriving (Eq, Ord, Read, Show)
L.makeFields ''Bin
instance TextShow Bin where
  showt (Bin l lb ub)= mconcat [l, ":", showt lb, "-", showt ub]

newtype BinRangeMap = BinRangeMap
  { unBinRangeMap :: Map.Map Bin Int
  }

data NormType = TfIdfNorm
              | UQNorm
              | MedNorm
              | TotalMedNorm
              | BothNorm
              | LogCPMNorm
              | NoneNorm
                deriving (Read, Show, Eq)
