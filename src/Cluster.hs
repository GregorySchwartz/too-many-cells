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
    ) where

-- Remote
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
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

-- | Cluster cLanguage.R.QQ (r)olumns of a sparse matrix using HDBSCAN.
hdbscan :: RMatObsRowImportant s -> R s (R.SomeSEXP s)
hdbscan (RMatObsRowImportant mat) = do
    [r| library(dbscan) |]

    clustering  <- [r| hdbscan(mat_hs, minPts = 5) |]

    return clustering

-- | Hierarchical clustering.
hClust
    :: SingleCells MatObsRowImportant
    -> ([((Cell, H.Vector H.R), Cluster)], HC.Dendrogram Cell)
hClust sc = (clustering, fmap fst dend)
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
    clusterList <- [r| clustering_hs$cluster |]
    return
        . zip (V.toList . rowNames $ sc)
        . fmap Cluster
        $ (R.fromSomeSEXP clusterList :: [Double])
