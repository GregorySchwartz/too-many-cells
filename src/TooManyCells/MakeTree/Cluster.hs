{- TooManyCells.MakeTree.Cluster
Gregory W. Schwartz

Collects the functions pertaining to the clustering of columns.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.MakeTree.Cluster
    ( hdbscan
    , clustersToClusterList
    , hClust
    , hSpecClust
    , assignClusters
    , dendrogramToClusterList
    , clusterDiversity
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves, getGraphLeavesWithParents, dendrogramToGraph)
import Control.Monad (join)
import Data.Function (on)
import Data.List (sortBy, groupBy, zip4, genericLength)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Sparse (hierarchicalSpectralCluster, B (..))
import Math.Clustering.Hierarchical.Spectral.Types (clusteringTreeToDendrogram, getClusterItemsDend, EigenGroup (..))
import Math.Diversity.Diversity (diversity)
import Statistics.Quantile (continuousBy, s)
import System.IO (hPutStrLn, stderr)
import Safe (headMay)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Numeric.LinearAlgebra as H

-- Local
import TooManyCells.MakeTree.Adjacency
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types
import TooManyCells.Diversity.Types

-- | Cluster cLanguage.R.QQ (r)olumns of a sparse matrix using HDBSCAN.
hdbscan :: RMatObsRow s -> R s (R.SomeSEXP s)
hdbscan (RMatObsRow mat) = do
    [r| library(dbscan) |]

    clustering  <- [r| hdbscan(mat_hs, minPts = 5) |]

    return clustering

-- | Hierarchical clustering.
hClust :: SingleCells -> ClusterResults
hClust sc =
    ClusterResults { _clusterList = clustering
                   , _clusterDend = cDend
                   }
  where
    cDend = fmap ( V.singleton
                 . (\ (!w, _, !y, !z)
                   -> CellInfo { _barcode = w, _cellRow = y, _projection = z }
                   )
                 )
            dend
    clustering = assignClusters
               . fmap ( fmap ((\(!w, _, !y, !z) -> CellInfo w y z))
                      . HC.elements
                      )
               . flip HC.cutAt (findCut dend)
               $ dend
    dend = HC.dendrogram HC.CLINK items euclDist
    euclDist x y =
        sqrt . sum . fmap (** 2) $ S.liftU2 (-) (L.view L._2 y) (L.view L._2 x)
    items = (\ fs
            -> zip4
                   (V.toList $ _rowNames sc)
                   fs
                   (fmap Row . take (V.length . _rowNames $ sc) . iterate (+ 1) $ 0)
                   (V.toList $ _projections sc)
            )
          . S.toRowsL
          . unMatObsRow
          . _matrix
          $ sc

-- | Assign clusters to values. Thanks to hierarchical clustering, we can have
-- a cell belong to multiple clusters.
assignClusters :: [[a]] -> [(a, [Cluster])]
assignClusters =
    concat . zipWith (\c -> flip zip (repeat c)) (fmap ((:[]) . Cluster) [1..])

-- | Find cut value.
findCut :: HC.Dendrogram a -> HC.Distance
findCut = continuousBy s 9 10 . VU.fromList . F.toList . flattenDist
  where
    flattenDist (HC.Leaf _)          = Seq.empty
    flattenDist (HC.Branch !d !l !r) =
        (Seq.<|) d . (Seq.><) (flattenDist l) . flattenDist $ r

-- | Convert the cluster object from hdbscan to a cluster list.
clustersToClusterList :: SingleCells
                      -> R.SomeSEXP s
                      -> R s [(Cell, Cluster)]
clustersToClusterList sc clustering = do
    io . hPutStrLn stderr $ "Calculating clusters."
    clusters <- [r| clustering_hs$cluster |]
    return
        . zip (V.toList . _rowNames $ sc)
        . fmap (Cluster . fromIntegral)
        $ (R.fromSomeSEXP clusters :: [Int32])

-- | Hierarchical spectral clustering.
hSpecClust :: EigenGroup
           -> NormType
           -> SingleCells
           -> (ClusterResults, ClusterGraph CellInfo)
hSpecClust eigenGroup norm sc =
    ( ClusterResults { _clusterList = clustering
                     , _clusterDend = dend
                     }
    , gr
    )
  where
    clustering :: [(CellInfo, [Cluster])]
    clustering =
        concatMap (\ (!ns, (_, !xs))
                  -> zip (maybe [] F.toList xs) . repeat . fmap Cluster $ ns
                  )
            . F.toList
            . flip getGraphLeavesWithParents 0
            . unClusterGraph
            $ gr
    gr         = dendrogramToGraph dend
    dend       = clusteringTreeToDendrogram tree
    tree       = hSpecCommand norm
               . Left
               . unMatObsRow
               . _matrix
               $ sc
    items      = V.zipWith3
                    (\x y z -> CellInfo x y z)
                    (_rowNames sc)
                    (fmap Row . flip V.generate id . V.length . _rowNames $ sc)
                    (_projections sc)
    hSpecCommand B1Norm   =
        hierarchicalSpectralCluster eigenGroup True Nothing Nothing items
    hSpecCommand BothNorm =
        hierarchicalSpectralCluster eigenGroup True Nothing Nothing items
    hSpecCommand _        =
        hierarchicalSpectralCluster eigenGroup False Nothing Nothing items

dendrogramToClusterList :: HC.Dendrogram (V.Vector CellInfo)
                        -> [(CellInfo, [Cluster])]
dendrogramToClusterList =
    concatMap (\ (!ns, (_, !xs))
                -> zip (maybe [] F.toList xs) . repeat . fmap Cluster $ ns
                )
        . F.toList
        . flip getGraphLeavesWithParents 0
        . unClusterGraph
        . dendrogramToGraph

-- | Find the diversity of each leaf cluster.
clusterDiversity :: Order
                 -> LabelMap
                 -> ClusterResults
                 -> Either String [(Cluster, Diversity, Size)]
clusterDiversity (Order order) (LabelMap lm) = do
    let getDiversityOfCluster :: [(CellInfo, [Cluster])]
                              -> Either String [(Cluster, Diversity, Size)]
        getDiversityOfCluster =
            join
                . fmap ( sequence
                       . fmap
                            (\ (!c, !xs)
                            -> do
                                diversities <- fmap (Diversity . diversity order)
                                                . sequence
                                                . fmap cellInfoToLabel
                                                $ xs
                                return (c, diversities, Size $ genericLength xs)
                            )
                       )
                . groupCellsByCluster
        cellInfoToLabel :: CellInfo -> Either String Label
        cellInfoToLabel =
            flip (Map.findWithDefault (Left "\nCell missing a label.")) (fmap Right lm)
                . Id
                . unCell
                . _barcode
        groupCellsByCluster :: [(CellInfo, [Cluster])]
                            -> Either String [(Cluster, [CellInfo])]
        groupCellsByCluster = sequence
                            . fmap assignCluster
                            . groupBy ((==) `on` (headMay . snd))
                            . sortBy (compare `on` (headMay . snd))
        assignCluster :: [(CellInfo, [Cluster])] -> Either String (Cluster, [CellInfo])
        assignCluster [] = Left "\nEmpty cluster."
        assignCluster all@(x:_) = do
            cluster <- fromMaybe (Left "\nNo cluster for cell.")
                    . fmap Right
                    . headMay
                    . snd
                    $ x
            return (cluster, fmap fst all)

    getDiversityOfCluster . _clusterList
