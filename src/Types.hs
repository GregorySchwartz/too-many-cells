{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Types where

-- Remote
import Math.Clustering.Hierarchical.Spectral.Types (ClusteringTree, ClusteringVertex)
import Math.Clustering.Hierarchical.Spectral.Sparse (ShowB)
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Map.Strict (Map)
import Math.Modularity.Types (Q (..))
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Language.R as R
import Language.R.QQ (r)
import qualified Data.Aeson as A
import qualified Data.Clustering.Hierarchical as HC
import qualified Numeric.LinearAlgebra as H
import qualified Data.Sparse.Common as S

-- Local

-- Basic
newtype Label = Label
    { unLabel :: Text
    } deriving (Eq,Ord,Read,Show)
newtype Cell = Cell
    { unCell :: Text
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON, A.FromJSON)
newtype Cluster = Cluster
    { unCluster :: Int
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
newtype MinClusterSize = MinClusterSize
    { unMinClusterSize :: Int
    } deriving (Read,Show)
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
newtype X = X
    { unX :: Double
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
newtype Y = Y
    { unY :: Double
    } deriving (Eq,Ord,Read,Show,Num,Generic,A.ToJSON,A.FromJSON)
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
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON,A.FromJSON)

data ClusterResults = ClusterResults
    { clusterList :: [(CellInfo, Cluster)]
    , clusterDend :: HC.Dendrogram (Vector CellInfo)
    , clusterTree :: Either (HC.Dendrogram (Vector CellInfo)) (ClusteringTree CellInfo ShowB)
    } deriving (Read,Show,Generic,A.ToJSON,A.FromJSON)

deriving instance (Read a) => Read (HC.Dendrogram a)
deriving instance (Generic a) => Generic (HC.Dendrogram a)

instance (Generic a) => Generic (Vector a)

instance A.ToJSON Q where
    toEncoding = A.genericToEncoding A.defaultOptions
instance A.FromJSON Q

instance (A.ToJSON a, Generic a) => A.ToJSON (HC.Dendrogram a) where
    toEncoding = A.genericToEncoding A.defaultOptions
instance (A.FromJSON a, Generic a) => A.FromJSON (HC.Dendrogram a)

instance (A.ToJSON a, A.ToJSON b, Generic a, Generic b) => A.ToJSON (ClusteringVertex a b) where
    toEncoding = A.genericToEncoding A.defaultOptions
instance (A.FromJSON a, A.FromJSON b, Generic a, Generic b) => A.FromJSON (ClusteringVertex a b)
