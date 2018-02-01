{- TooManyCells.MakeTree.Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.MakeTree.Types where

-- Remote
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
import qualified Data.Sparse.Common as S
import qualified Numeric.LinearAlgebra as H

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
newtype PreNormalization = PreNormalization
    { unPreNormalization :: Bool
    } deriving (Read,Show)
newtype DrawNodeNumber = DrawNodeNumber
    { unDrawNodeNumber :: Bool
    } deriving (Read,Show)
newtype IsLeaf = IsLeaf {unIsLeaf :: Bool} deriving (Eq, Ord, Read, Show)
newtype Cols            = Cols { unCols :: [Double] }
newtype Delimiter       = Delimiter { unDelimiter :: Char }
newtype Gene            = Gene { unGene :: Text } deriving (Eq, Ord, Read, Show)
newtype CellFile        = CellFile { unCellFile :: FilePath }
newtype GeneFile        = GeneFile { unGeneFile :: FilePath }
newtype MatrixFile = MatrixFile
    { unMatrixFile :: FilePath
    } deriving (Read,Show)
newtype ProjectionFile  = ProjectionFile { unProjectionFile :: FilePath }
newtype LabelFile       = LabelFile { unLabelFile :: FilePath }
newtype PriorPath   = PriorPath
    { unPriorPath :: FilePath
    } deriving (Eq,Ord,Read,Show)
newtype OutputDirectory  = OutputDirectory { unOutputDirectory :: FilePath }
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
newtype Row = Row
    { unRow :: Int
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON,A.FromJSON)
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
newtype LabelColorMap = LabelColorMap
    { unLabelColorMap :: Map Label Kolor
    } deriving (Read,Show)
newtype CellColorMap = CellColorMap
    { unCellColorMap :: Map Cell Kolor
    } deriving (Read,Show)
newtype CellGraph = CellGraph
    { unCellGraph :: G.Gr (G.Node, Maybe (Seq.Seq CellInfo)) HC.Distance
    } deriving (Read, Show)

-- Advanced
data DrawCellType = DrawLabel | DrawExpression Text deriving (Read, Show)
data DrawLeaf = DrawCell DrawCellType | DrawText deriving (Read, Show)

data SingleCells a = SingleCells { matrix :: a
                                 , rowNames :: Vector Cell
                                 , colNames :: Vector Gene
                                 , projections :: Vector (X, Y)
                                 }
                     deriving (Read, Show)

data CellInfo = CellInfo
    { barcode :: Cell
    , cellRow :: Row
    , projection :: (X, Y)
    } deriving (Eq,Ord,Read,Show,Generic,A.ToJSON,A.FromJSON)

data ClusterResults = ClusterResults
    { clusterList :: [(CellInfo, [Cluster])] -- Thanks to hierarchical clustering, we can have multiple clusters per cell.
    , clusterDend :: HC.Dendrogram (Vector CellInfo)
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

instance (A.ToJSON a, Generic a) => A.ToJSON (ClusteringVertex a) where
    toEncoding = A.genericToEncoding A.defaultOptions
instance (A.FromJSON a, Generic a) => A.FromJSON (ClusteringVertex a)

instance (Ord a, Floating a) => Ord (Colour a) where
    compare x y = ((\a -> (channelRed a, channelGreen a, channelBlue a)) . toSRGB $ y)
        `compare` ((\a -> (channelRed a, channelGreen a, channelBlue a)) . toSRGB $ x)
