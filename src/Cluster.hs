{- Cluster
Gregory W. Schwartz

Collects the functions pertaining to the clustering of columns.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Cluster
    ( hdbscan
    , clustersToClusterList
    , hClust
    , hSpecClust
    ) where

-- Remote
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral
       (hierarchicalSpectralCluster, getClusterItems)
import Statistics.Quantile (continuousBy, s)
import System.IO (hPutStrLn, stderr)
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Numeric.LinearAlgebra as H

-- Local
import Types
import Utility
import Adjacency

-- | Cluster cLanguage.R.QQ (r)olumns of a sparse matrix using HDBSCAN.
hdbscan :: RMatObsRowImportant s -> R s (R.SomeSEXP s)
hdbscan (RMatObsRowImportant mat) = do
    [r| library(dbscan) |]

    clustering  <- [r| hdbscan(mat_hs, minPts = 5) |]

    return clustering

-- | Hierarchical clustering.
hClust :: SingleCells MatObsRowImportant -> ClusterResults
hClust sc =
    ClusterResults { clusterList = clustering
                   , clusterDend = fmap (V.singleton . fst) dend
                   }
  where
    clustering =
        assignClusters . fmap HC.elements . flip HC.cutAt (findCut dend) $ dend
    dend = HC.dendrogram HC.CLINK items euclDist
    euclDist x y = H.norm_2 $ snd y - snd x
    items = zip (V.toList . rowNames $ sc)
          . H.toRows
          . unMatObsRowImportant
          . matrix
          $ sc

-- | Assign clusters to values.
assignClusters :: [[a]] -> [(a, Cluster)]
assignClusters =
    concat . zipWith (\c -> flip zip (repeat c)) (fmap Cluster [1..])

-- | Find cut value.
findCut :: HC.Dendrogram a -> HC.Distance
findCut = continuousBy s 9 10 . VU.fromList . toList . flattenDist
  where
    flattenDist (HC.Leaf _)          = Seq.empty
    flattenDist (HC.Branch !d !l !r) =
        (Seq.<|) d . (Seq.><) (flattenDist l) . flattenDist $ r

-- | Convert the cluster object from hdbscan to a cluster list.
clustersToClusterList :: SingleCells MatObsRowImportant
                      -> R.SomeSEXP s
                      -> R s [(Cell, Cluster)]
clustersToClusterList sc clustering = do
    io . hPutStrLn stderr $ "Calculating clusters."
    clusters <- [r| clustering_hs$cluster |]
    return
        . zip (V.toList . rowNames $ sc)
        . fmap (Cluster . fromIntegral)
        $ (R.fromSomeSEXP clusters :: [Int32])

-- | Hierarchical spectral clustering
hSpecClust :: SingleCells MatObsRow -> ClusterResults
hSpecClust sc = ClusterResults { clusterList = clustering
                               , clusterDend = fmap (fmap fst . fst) dend
                               }
  where
    clustering = assignClusters . fmap V.toList . getClusterItems $ dend
    dend       = hierarchicalSpectralCluster items . unAdjacencyMat $ adjMat
    adjMat     =
        getAdjacencyMat . matrix $ sc
    items      = V.fromList
               . zip (V.toList . rowNames $ sc)
               . H.toRows
               . unMatObsRow
               . matrix
               $ sc
