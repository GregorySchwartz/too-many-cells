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
    , printLabelMap
    , saveFragments
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves, getGraphLeavesWithParents)
import Control.Monad (join)
import Data.List (genericLength, intercalate, foldl')
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust)
import Data.Monoid ((<>))
import Data.Streaming.Zlib (WindowBits (..))
import Safe (headMay)
import TextShow (showt)
import Turtle hiding (Size)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Filesystem.Path as FP
import qualified Turtle.Bytes as TB

-- Local
import TooManyCells.Diversity.Types
import TooManyCells.File.Types
import TooManyCells.MakeTree.Types
import TooManyCells.MakeTree.Cluster
import TooManyCells.Matrix.Types

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
    getSize = foldl' (+) 0 . fmap (maybe 0 Seq.length . snd) . getGraphLeaves gr
    getQs :: ([G.Node], a) -> [Double]
    getQs = mapMaybe getQ . fst
    getQ :: G.Node -> Maybe Double
    getQ  = join . fmap (L.view (L._2. edgeDistance)) . headMay . G.lsuc gr

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

-- | Get the counts and frequencies of all cell labels in a node.
getComposition ::
     (TreeItem a) => LabelMap -> ClusterGraph a -> G.Node -> LabelCompositions
getComposition (LabelMap lm) (ClusterGraph gr) n =
    LabelCompositions
        . Map.elems
        . Map.mapWithKey
            (\ !k !v -> LabelComposition k v (fromIntegral v / total))
        $ counts
  where
    counts = Map.fromListWith (+) . flip zip [1,1..] $ labels
    total  = genericLength labels
    labels = catMaybes
           . fmap (flip Map.lookup lm . getId)
           . concatMap (maybe [] F.toList . snd)
           . F.toList
           $ getGraphLeaves gr n

-- | Get various properties about the nodes in the ClusterGraph.
nodeInfo :: (TreeItem a) => Maybe LabelMap -> ClusterGraph a -> [NodeInfo]
nodeInfo lm (ClusterGraph gr) = fmap getNodeInfo . G.nodes $ gr
  where
    getNodeInfo x = NodeInfo
                        x
                        (getSize x)
                        (getProportion x)
                        (getQ x)
                        (getSignificance x)
                        (fmap (\a -> getComposition a (ClusterGraph gr) x) lm)
                        (getNodeChildren gr x)
    getSize :: G.Node -> Int
    getSize = foldl' (+) 0 . fmap (maybe 0 Seq.length . snd) . getGraphLeaves gr
    getQ :: G.Node -> Maybe Double
    getQ  = join . fmap (L.view edgeDistance . snd) . headMay . G.lsuc gr
    getSignificance :: G.Node -> Maybe Double
    getSignificance =
      join . fmap (L.view edgeSignificance . snd) . headMay . G.lsuc gr
    getProportion :: G.Node -> Maybe Double
    getProportion n =
        case fmap fst $ G.lsuc gr n of
            [x, y] -> Just $ fromIntegral (getSize x) / fromIntegral (getSize y)
            []     -> Nothing
    getNodeChildren gr = Set.toAscList
                       . Set.fromList
                       . concatMap fst
                       . F.toList
                       . getGraphLeavesWithParents gr

-- | Print the node information to a string.
printNodeInfo ::
     (TreeItem a) => Maybe LabelMap -> ClusterGraph a -> B.ByteString
printNodeInfo lm =
    (<>) "node,size,proportion,modularity,significance,composition,subtree\n"
        . CSV.encode
        . fmap (\ (NodeInfo n s p m sig c ch)
               -> ( n
                  , s
                  , maybe "" show p
                  , maybe "" show m
                  , maybe "" show sig
                  , maybe "" show c
                  , intercalate "/" . fmap show $ ch
                  )
               )
        . nodeInfo lm

-- | Print the label map to a string.
printLabelMap :: LabelMap -> B.ByteString
printLabelMap = (<>) "item,label\n" . CSV.encode 
              . fmap ( L.over L._2 unLabel
                     . L.over L._1 unId
                     )
              . Map.toAscList
              . unLabelMap

-- | Print the fragments to a file. Will reassign original barcodes from the
-- original file to those in the single cell matrix by splitting the barcode on
-- "BARCODE-LABEL". Mainly for use with --custom-label.
saveFragments :: OutputDirectory
              -> SingleCells
              -> [Either MatrixFileType MatrixFileType]
              -> IO ()
saveFragments (OutputDirectory output') sc files = sh $ do
  let out = (fromText . T.pack $ output') FP.</> "fragments.tsv.gz"
      newBarcodes = HMap.fromList
                  . fmap (\ (Cell !x) -> (fst . T.breakOn "-" $ x, x))
                  . V.toList
                  . L.view rowNames
                  $ sc
      assignRow :: T.Text -> Maybe T.Text
      assignRow = fmap ((<> "\n") . T.intercalate "\t")
                . ( (L.ix 3)
              L.%%~ (flip HMap.lookup newBarcodes . fst . T.breakOn "-")
                  )
                . T.splitOn "\t"
      getFile (Left (CompressedFragments (FragmentsFile file))) = file
      getFile x = error $ "saveFragments: Not a fragments.tsv.gz file: "
               <> show x
      loadFile =
        TB.decompress (WindowBits 31) . TB.input . fromText . T.pack . getFile

  TB.output out
    . TB.compress 6 (WindowBits 31)
    . fmap (maybe "" T.encodeUtf8)
    . mfilter isJust
    . fmap (assignRow . lineToText)
    . toLines
    . TB.toUTF8
    . msum
    . fmap loadFile
    $ files
