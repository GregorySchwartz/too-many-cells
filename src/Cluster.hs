{- Cluster
Gregory W. Schwartz

Collects the functions pertaining to the clustering of columns.
-}

{-# LANGUAGE QuasiQuotes #-}

module Cluster
    ( hdbscan
    , clustersToClusterList
    ) where

-- Remote
import qualified Data.Vector as V
import Language.R as R
import Language.R.QQ (r)

-- Local
import Types
import Utility

-- | Cluster columns of a sparse matrix using HDBSCAN.
hdbscan :: RMatObsRowImportant s -> R s (R.SomeSEXP s)
hdbscan (RMatObsRowImportant mat) = do
    [r| library(dbscan) |]

    clustering  <- [r| hdbscan(mat_hs, minPts = 5) |]

    return clustering

-- | Convert the cluster object from hdbscan to a cluster list.
clustersToClusterList :: SingleCells -> R.SomeSEXP s -> R s [(Cell, Cluster)]
clustersToClusterList sc clustering = do
    clusterList <- [r| clustering_hs$cluster |]
    return
        . zip (V.toList . colNames $ sc)
        . fmap Cluster
        $ (R.fromSomeSEXP clusterList :: [Double])
