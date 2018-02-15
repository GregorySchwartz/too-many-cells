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
import TooManyCells.Matrix.Types

-- Basic
newtype Label = Label
    { unLabel :: Text
    } deriving (Eq,Ord,Read,Show)
newtype Id = Id
    { unId :: Text
    } deriving (Eq,Ord,Read,Show)
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
newtype AdjacencyMat = AdjacencyMat
    { unAdjacencyMat :: H.Matrix H.R
    } deriving (Read,Show)
newtype LabelMap = LabelMap
    { unLabelMap :: Map Id Label
    } deriving (Read,Show)
newtype LabelColorMap = LabelColorMap
    { unLabelColorMap :: Map Label Kolor
    } deriving (Read,Show)
newtype ItemColorMap = ItemColorMap
    { unItemColorMap :: Map Id Kolor
    } deriving (Read,Show)
newtype ClusterGraph a = ClusterGraph
    { unClusterGraph :: G.Gr (G.Node, Maybe (Seq.Seq a)) HC.Distance
    } deriving (Read, Show)

-- Advanced
data DrawItemType = DrawLabel | DrawExpression Text deriving (Read, Show)
data DrawLeaf = DrawItem DrawItemType | DrawText deriving (Read, Show)
data DrawPie  = PieRing | PieChart | PieNone deriving (Read, Show)

data DrawConfig = DrawConfig
    { _drawLeaf :: DrawLeaf
    , _drawPie :: DrawPie
    , _drawNodeNumber :: DrawNodeNumber
    } deriving (Read,Show)

data ClusterResults = ClusterResults
    { clusterList :: [(CellInfo, [Cluster])] -- Thanks to hierarchical clustering, we can have multiple clusters per cell.
    , clusterDend :: HC.Dendrogram (Vector CellInfo)
    } deriving (Read,Show,Generic,A.ToJSON,A.FromJSON)

class TreeItem a where
    getId :: a -> Id

instance TreeItem CellInfo where
    getId = Id . unCell . barcode

deriving instance (Read a) => Read (HC.Dendrogram a)
deriving instance (Generic a) => Generic (HC.Dendrogram a)

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
