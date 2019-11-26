{- TooManyCells.Matrix.Types
Gregory W. Schwartz

Collects the types used in the program concerning the matrix.
-}

{-# LANGUAGE BangPatterns #-}
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
import BirchBeer.Types
import Control.DeepSeq (NFData (..), force)
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid (..), mempty)
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Colour.SRGB (Colour (..), RGB (..), toSRGB)
import Data.Function (on)
import Data.List (sortBy)
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
import qualified Control.DeepSeq as DS
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.IntMap as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Sparse.SpMatrix as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Numeric.LinearAlgebra as H

-- Local

-- Basic
newtype Cell = Cell
    { unCell :: Text
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON, A.FromJSON)
newtype Cols            = Cols { unCols :: [Double] }
newtype FeatureColumn   = FeatureColumn { unFeatureColumn :: Int }
newtype CustomLabel = CustomLabel { unCustomLabel :: Text}
                      deriving (Eq,Ord,Read,Show)
newtype BinWidth = BinWidth { unBinWidth :: Int}
newtype NoBinarizeFlag = NoBinarizeFlag { unNoBinarizeFlag :: Bool }
newtype TransposeFlag = TransposeFlag { unTransposeFlag :: Bool }
newtype BinIdx = BinIdx { unBinIdx :: Int} deriving (Eq, Ord, Read, Show, Num, Generic)
newtype RangeIdx = RangeIdx { unRangeIdx :: Int} deriving (Eq, Ord, Read, Show, Num, Generic)
newtype CellWhitelist = CellWhitelist
    { unCellWhitelist :: HSet.HashSet Text
    } deriving (Eq,Ord,Show)
newtype ExcludeFragments = ExcludeFragments { unExcludeFragments :: T.Text }
newtype BlacklistRegions = BlacklistRegions { unBlacklistRegions :: T.Text }
newtype PCADim = PCADim
    { unPCADim :: Int
    } deriving (Eq,Ord,Read,Show)
newtype SVDDim = SVDDim
    { unSVDDim :: Int
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

newtype LabelMat = LabelMat { unLabelMat :: (SingleCells, Maybe LabelMap) }
instance Semigroup LabelMat where
  (<>) (LabelMat (!m1, !l1)) (LabelMat (!m2, !l2)) =
    LabelMat (mappend m1 m2, mappend l1 l2)
instance Monoid LabelMat where
  mempty = LabelMat (mempty, mempty)

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
  (<>) x y
    | (null . L.view colNames $ x)
   || (null . L.view colNames $ y)
   || (L.view colNames x == L.view colNames y) =  -- If either is null or they share features, then do a normal append
        SingleCells { _matrix      = mappend (L.view matrix x) (L.view matrix y)
                    , _rowNames    = (V.++) (L.view rowNames x) (L.view rowNames y)
                    , _colNames    = _colNames x
                    }
    | otherwise = mergeFeaturesSingleCells x y

-- | Join two SingleCells with different features.
mergeFeaturesSingleCells :: SingleCells -> SingleCells -> SingleCells
mergeFeaturesSingleCells sc1 sc2 =
  SingleCells { _matrix      = MatObsRow
                             . force
                             $ (newMat id sc1)
                         S.^+^ (newMat (+ sc1NRows) sc2)
              , _rowNames    = (V.++) (L.view rowNames sc1) (L.view rowNames sc2)
              , _colNames    = V.fromList newCols
              }
  where
    newMat rowF sc = S.modifyKeysSM
                      rowF
                      (lookupCol (fmap unFeature $ L.view colNames sc))
                   . S.SM (newNRows, HMap.size newColMap)
                   . S.immSM
                   $ getMatrix sc
    newNRows = (V.length . L.view rowNames $ sc1)
             + (V.length . L.view rowNames $ sc2)
    sc1NRows = S.nrows . getMatrix $ sc1
    lookupCol :: V.Vector T.Text -> Int -> Int
    lookupCol olds x = fromMaybe (error $ "mergeFeaturesSingleCells: No new index when joining cols: " <> show x)
                     . (>>=) (olds V.!? x)
                     $ flip HMap.lookup newColMap
    newColMap :: HMap.HashMap T.Text Int
    newColMap = HMap.fromList . flip zip [0..] . fmap unFeature $ newCols
    newCols = Set.toAscList . Set.union (colToSet sc1) $ colToSet sc2
    colToSet = Set.fromList . V.toList . L.view colNames

instance Monoid SingleCells where
    mempty  = SingleCells { _matrix = mempty
                          , _rowNames = V.empty
                          , _colNames = V.empty
                          }

instance MatrixLike SingleCells where
    getMatrix   = unMatObsRow . _matrix
    getRowNames = fmap unCell . _rowNames
    getColNames = fmap unFeature . _colNames


data CellInfo = CellInfo
    { _barcode :: Cell
    , _cellRow :: Row
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON,A.FromJSON)
L.makeLenses ''CellInfo

-- | A range with a label, most ranges can vary with each other.
data Range = Range { rangeIdx :: Maybe RangeIdx
                   , _rangeLabel :: Text
                   , _rangeLowerBound :: Int
                   , _rangeUpperBound :: Int
                   }
             deriving (Eq, Ord, Read, Show)
L.makeFields ''Range
instance TextShow Range where
  showt (Range _ l lb ub)= mconcat [l, ":", showt lb, "-", showt ub]

-- | A bin with a label, bins are standard across all observations.
data Bin = Bin { binIdx :: Maybe BinIdx, _binLabel :: Text, _binLowerBound :: Int, _binUpperBound :: Int }
             deriving (Eq, Ord, Read, Show)
L.makeFields ''Bin
instance TextShow Bin where
  showt (Bin _ l lb ub)= mconcat [l, ":", showt lb, "-", showt ub]

-- | Map of ranges to their bins.
newtype RangeBinMap = RangeBinMap
  { unRangeBinMap :: IMap.IntMap BinIdx
  }

data NormType = TfIdfNorm
              | UQNorm
              | MedNorm
              | TotalMedNorm
              | BothNorm
              | LogCPMNorm
              | NoneNorm
                deriving (Read, Show, Eq)

-- | Map of bins to their idxs in unsorted and un-uniqued list.
newtype BinIdxMap = BinIdxMap
  { unBinIdxMap :: Map.Map Bin BinIdx
  }

instance Generic Feature
instance NFData Feature where rnf x = seq x ()

instance Generic Double

instance Generic a => Generic (S.SpMatrix a)
instance (Generic a, NFData a) => NFData (S.SpMatrix a) where
  rnf all@(S.SM (!x, !y) !m) = seq all ()
