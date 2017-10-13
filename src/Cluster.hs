{- Cluster
Gregory W. Schwartz

Collects the functions pertaining to the clustering of columns.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}

module Cluster
    ( hdbscan
    , clustersToClusterList
    ) where

-- Remote
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import System.IO (hPutStrLn, stderr)
import Data.Foldable (foldl')
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Statistics.Quantile (continuousBy, s)
import qualified Numeric.LinearAlgebra as H
import qualified Data.Clustering.Hierarchical as HC

-- Local
import Types
import Utility

-- | Cluster cLanguage.R.QQ (r)olumns of a sparse matrix using HDBSCAN.
hdbscan :: RMatObsRowImportant s -> R s (R.SomeSEXP s)
hdbscan (RMatObsRowImportant mat) = do
    [r| library(dbscan) |]

    clustering  <- [r| hdbscan(mat_hs, minPts = 5) |]

    return clustering

-- | Hierarchical clustering.
hClust :: SingleCells -> [(Cell, Cluster)]
hClust sc = clustering
  where
    clustering = assignClusters . fmap (fmap fst . elements findCut) $ dend
    dend = HC.dendrogram HC.CLINK euclDist items
    euclDist x y = norm_2 $ snd y - snd x
    items = zip (V.toList . rowNames $ sc) . toRows . matrix $ sc

-- | Assign clusters to values.
assignClusters :: [[Cell]] -> [(Cell, Cluster)]
assignClusters = zipWith (\c -> zip (repeat c)) (fmap Cluster [1..])

-- | Find cut value.
findCut :: HC.Dendrogram (H.Vector R) -> HC.Distance
findCut = continuousBy s 3 4 . VU.fromList . foldl' foldFunc
  where
    foldFunc !acc (HC.Branch !d _ _) = (Just d) : acc
    foldFunc _ (HC.Leaf _)           = Nothing
    
-- | Convert the cluster object from hdbscan to a cluster list.
clustersToClusterList :: SingleCells -> R.SomeSEXP s -> R s [(Cell, Cluster)]
clustersToClusterList sc clustering = do
    io . hPutStrLn stderr $ "Calculating clusters."
    clusterList <- [r| clustering_hs$cluster |]
    return
        . zip (V.toList . colNames $ sc)
        . fmap Cluster
        $ (R.fromSomeSEXP clusterList :: [Double])
