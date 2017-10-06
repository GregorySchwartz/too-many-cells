{- Cluster
Gregory W. Schwartz

Collects the functions pertaining to the clustering of columns.
-}

{-# LANGUAGE QuasiQuotes #-}

module Cluster
    ( featureSelectionRandomForest
    , removeCorrelated
    , hdbscan
    , clustersToClusterList
    ) where

-- Remote
import qualified Data.Vector as V
import Language.R as R
import Language.R.QQ (r)

-- Local
import Types
import Utility

-- | Perform feature selection on a scaled matrix.
featureSelectionRandomForest :: RMatScaled s -> R s (RMatImportant s)
featureSelectionRandomForest (RMatScaled mat) = do
    [r| suppressPackageStartupMessages(library(randomForest)) |]

    importance   <- [r| randomForest(mat_hs, na.action = na.omit)$importance |]
    importantMat <- [r| mat_hs[,importance_hs > sort(importance_hs, decreasing = TRUE)[10]] |]

    return . RMatImportant $ importantMat
    
-- | Remove highly correlated (> 0.6) variables in a matrix.
removeCorrelated :: RMatScaled s -> R s (RMatImportant s)
removeCorrelated (RMatScaled mat) = do
    [r| suppressPackageStartupMessages(library(caret)) |]

    cor   <- [r| cor(mat_hs) |]
    importantMat <- [r| mat_hs[,-findCorrelation(cor_hs, cutoff = 0.6, exact = FALSE)] |]

    return . RMatImportant $ importantMat

-- | Cluster columns of a sparse matrix using HDBSCAN.
hdbscan :: RMatImportant s -> R s (R.SomeSEXP s)
hdbscan (RMatImportant mat) = do
    [r| library(Matrix) |]
    [r| library(dbscan) |]

    clustering  <- [r| hdbscan(as.matrix(mat_hs), minPts = 5) |]

    return clustering

-- | Convert the cluster object from hdbscan to a cluster list.
clustersToClusterList :: SingleCells -> R.SomeSEXP s -> R s [(Cell, Cluster)]
clustersToClusterList sc clustering = do
    clusterList <- [r| clustering_hs$cluster |]
    return
        . zip (V.toList . colNames $ sc)
        . fmap Cluster
        $ (R.fromSomeSEXP clusterList :: [Double])
