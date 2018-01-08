{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Types where

-- Remote
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Language.R as R
import Language.R.QQ (r)
import qualified Data.Clustering.Hierarchical as HC
import qualified Numeric.LinearAlgebra as H
import qualified Data.Sparse.Common as S

-- Local

-- Basic
newtype Label = Label
    { unLabel :: Text
    } deriving (Eq,Ord,Read,Show)
newtype Cell            = Cell { unCell :: Text } deriving (Eq, Ord, Read, Show)
newtype Cluster         = Cluster { unCluster :: Int } deriving (Read, Show)
newtype Cols            = Cols { unCols :: [Double] }
newtype Delimiter       = Delimiter { unDelimiter :: Char }
newtype Gene            = Gene { unGene :: Text } deriving (Read, Show)
newtype CellFile        = CellFile { unCellFile :: FilePath }
newtype GeneFile        = GeneFile { unGeneFile :: FilePath }
newtype MatrixFile = MatrixFile
    { unMatrixFile :: FilePath
    } deriving (Read,Show)
newtype ProjectionFile  = ProjectionFile { unProjectionFile :: FilePath }
newtype LabelFile       = LabelFile { unLabelFile :: FilePath }
newtype DendrogramFile  = DendrogramFile { unDendrogramFile :: FilePath }
newtype X               = X { unX :: Double } deriving (Eq, Ord, Read, Show, Num)
newtype Y               = Y { unY :: Double } deriving (Eq, Ord, Read, Show, Num)
newtype RMat s          = RMat { unRMat :: R.SomeSEXP s }
newtype RMatObsRow s    = RMatObsRow { unRMatObsRow :: R.SomeSEXP s }
newtype RMatFeatRow s   = RMatFeatRow { unRMatFeatRow :: R.SomeSEXP s }
newtype RMatObsRowImportant s = RMatObsRowImportant
    { unRMatObsRowImportant :: R.SomeSEXP s
    }
newtype RMatScaled s    = RMatScaled { unRMatScaled :: R.SomeSEXP s }
newtype Rows            = Rows { unRows :: [Double] }
newtype Vals            = Vals { unVals :: [Double] }
newtype AdjacencyMat = AdjacencyMat
    { unAdjacencyMat :: H.Matrix H.R
    } deriving (Read,Show)
newtype MatObsRow = MatObsRow
    { unMatObsRow :: S.SpMatrix Double
    } deriving (Show)
newtype MatObsRowImportant = MatObsRowImportant
    { unMatObsRowImportant :: S.SpMatrix Double
    } deriving (Show)
newtype LabelMap = LabelMap
    { unLabelMap :: Map Cell Label
    } deriving (Read,Show)
newtype ColorMap = ColorMap
    { unColorMap :: Map Label Kolor
    } deriving (Read,Show)

-- Advanced
data SingleCells a = SingleCells { matrix :: a
                                 , rowNames :: Vector Cell
                                 , colNames :: Vector Gene
                                 , projections :: Vector (X, Y)
                                 }
                     deriving (Read, Show)

data CellInfo = CellInfo
    { barcode :: Cell
    , features :: [(Int, Double)]
    , projection :: (X, Y)
    } deriving (Eq,Ord,Read,Show)

data ClusterResults = ClusterResults { clusterList :: [(CellInfo, Cluster)]
                                     , clusterDend :: HC.Dendrogram (Vector Cell)
                                     }
                      deriving (Read, Show)

deriving instance (Read a) => Read (HC.Dendrogram a)
