{- TooManyCells.MakeTree.Print
Gregory W. Schwartz

Collects the functions pertaining to the printing of information for the graph.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.MakeTree.Print
    ( printClusterDiversity
    , printClusterInfo
    , printNodeInfo
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves, getGraphLeavesWithParents)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Safe (headMay)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Sequence as Seq
import qualified Data.Text as T

-- Local
import TooManyCells.Diversity.Types
import TooManyCells.MakeTree.Types
import TooManyCells.MakeTree.Cluster

-- | Print the diversity of each leaf cluster.
printClusterDiversity :: [(Cluster, Diversity, Size)] -> B.ByteString
printClusterDiversity =
    (<>) "cluster,diversity,size\n" . CSV.encode
        . fmap ( L.over L._3 unSize
               . L.over L._2 unDiversity
               . L.over L._1 unCluster
               )

-- | Get the size and modularity path of each leaf cluster path. Modularity
-- starts from the parent of the cluster to the root for modularity.
clusterInfo :: (TreeItem a) => ClusterGraph a -> [(Cluster, [Double], [Int])]
clusterInfo (ClusterGraph gr) =
    F.toList
        . fmap (\ !x -> (Cluster . fst . snd $ x, getQs x, getSizes x))
        . getGraphLeavesWithParents gr
        $ 0
  where
    getSizes :: ([G.Node], a) -> [Int]
    getSizes = fmap getSize . fst
    getSize :: G.Node -> Int
    getSize = sum . fmap (maybe 0 Seq.length . snd) . getGraphLeaves gr
    getQs :: ([G.Node], a) -> [Double]
    getQs = mapMaybe getQ . fst
    getQ :: G.Node -> Maybe Double
    getQ  = fmap snd . headMay . G.lsuc gr

-- | Get the information of each leaf cluster path. Modularity
-- starts from the parent of the cluster to the root for modularity.
printClusterInfo :: (TreeItem a) => ClusterGraph a -> B.ByteString
printClusterInfo =
    (<>) "cluster,modularity,size\n" . CSV.encode
        . fmap ( L.over L._3 (T.intercalate "/" . fmap showt)
               . L.over L._2 (T.intercalate "/" . fmap showt)
               . L.over L._1 unCluster
               )
        . clusterInfo

-- | Get various properties about the nodes in the ClusterGraph.
nodeInfo :: (TreeItem a) => ClusterGraph a -> [NodeInfo]
nodeInfo (ClusterGraph gr) = fmap getNodeInfo . G.nodes $ gr
  where
    getNodeInfo x = NodeInfo x (getSize x) (getProportion x) (getQ x)
    getSize :: G.Node -> Int
    getSize = sum . fmap (maybe 0 Seq.length . snd) . getGraphLeaves gr
    getQ :: G.Node -> Maybe Double
    getQ  = fmap snd . headMay . G.lsuc gr
    getProportion :: G.Node -> Maybe Double
    getProportion n =
        case fmap fst $ G.lsuc gr n of
            [x, y] -> Just $ fromIntegral (getSize x) / fromIntegral (getSize y)
            []     -> Nothing

-- | Print the node information to a string.
printNodeInfo :: (TreeItem a) => ClusterGraph a -> B.ByteString
printNodeInfo =
    (<>) "node,size,proportion,modularity\n"
        . CSV.encode
        . fmap (\(NodeInfo n s p m) -> (n, s, maybe "" show p, maybe "" show m))
        . nodeInfo
