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
    , treeToClusterList
    , clusterDiversity
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves, getGraphLeavesWithParents, dendrogramToGraph, dendToTree, clusteringTreeToTree, treeToGraph)
import Control.Monad (join)
import Data.Function (on)
import Data.List (sortBy, groupBy, zip4, genericLength)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Math.Modularity.Types (Q (..))
import Data.Monoid ((<>))
import Data.Tree (Tree (..))
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
import qualified Math.Clustering.Hierarchical.Spectral.Dense as HSD
import qualified Numeric.LinearAlgebra as H

-- Local
import TooManyCells.MakeTree.Adjacency
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
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
                   , _clusterDend = dendToTree cDend
                   }
  where
    cDend = fmap ( V.singleton
                 . (\ (!w, _, !y)
                   -> CellInfo { _barcode = w, _cellRow = y }
                   )
                 )
            dend
    clustering = assignClusters
               . fmap ( fmap ((\(!w, _, !y) -> CellInfo w y))
                      . HC.elements
                      )
               . flip HC.cutAt (findCut dend)
               $ dend
    dend = HC.dendrogram HC.CLINK items euclDist
    euclDist x y =
        sqrt . sum . fmap (** 2) $ S.liftU2 (-) (L.view L._2 y) (L.view L._2 x)
    items = (\ fs
            -> zip3
                   (V.toList $ _rowNames sc)
                   fs
                   (fmap Row . take (V.length . _rowNames $ sc) . iterate (+ 1) $ 0)
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
hSpecClust :: DenseFlag
           -> EigenGroup
           -> NormType
           -> Maybe NumEigen
           -> Maybe Q
           -> Maybe NumRuns
           -> SingleCells
           -> IO (ClusterResults, ClusterGraph CellInfo)
hSpecClust (DenseFlag isDense) eigenGroup norm numEigen minModMay runsMay sc = do
  let items      = V.zipWith
                      (\x y -> CellInfo x y)
                      (_rowNames sc)
                      (fmap Row . flip V.generate id . V.length . _rowNames $ sc)
      hSpecCommand TfIdfNorm False =
          hierarchicalSpectralCluster
            eigenGroup
            True
            (fmap unNumEigen numEigen)
            Nothing
            minModMay
            (fmap unNumRuns runsMay)
            items
          . Left
      hSpecCommand BothNorm False =
          hierarchicalSpectralCluster
            eigenGroup
            True
            (fmap unNumEigen numEigen)
            Nothing
            minModMay
            (fmap unNumRuns runsMay)
            items
          . Left
      hSpecCommand _ False =
          hierarchicalSpectralCluster
            eigenGroup
            False
            (fmap unNumEigen numEigen)
            Nothing
            minModMay
            (fmap unNumRuns runsMay)
            items
          . Left
      hSpecCommand TfIdfNorm True =
          HSD.hierarchicalSpectralCluster
            eigenGroup
            True
            (fmap unNumEigen numEigen)
            Nothing
            minModMay
            (fmap unNumRuns runsMay)
            items
          . Left
          . sparseToHMat
      hSpecCommand BothNorm True =
          HSD.hierarchicalSpectralCluster
            eigenGroup
            True
            (fmap unNumEigen numEigen)
            Nothing
            minModMay
            (fmap unNumRuns runsMay)
            items
          . Left
          . sparseToHMat
      hSpecCommand _ True =
          HSD.hierarchicalSpectralCluster
            eigenGroup
            False
            (fmap unNumEigen numEigen)
            Nothing
            minModMay
            (fmap unNumRuns runsMay)
            items
          . Left
          . sparseToHMat

  tree <- hSpecCommand norm isDense . unMatObsRow . _matrix $ sc

  let clustering :: [(CellInfo, [Cluster])]
      clustering =
        concatMap (\ (!ns, (_, !xs))
                  -> zip (maybe [] F.toList xs) . repeat . fmap Cluster $ ns
                  )
            . F.toList
            . flip getGraphLeavesWithParents 0
            . unClusterGraph
            $ gr
      dend = clusteringTreeToTree tree
      gr = treeToGraph dend

  return ( ClusterResults { _clusterList = clustering
                          , _clusterDend = dend
                          }
         , gr
         )

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

treeToClusterList :: Tree (TreeNode (V.Vector CellInfo))
                        -> [(CellInfo, [Cluster])]
treeToClusterList =
    concatMap (\ (!ns, (_, !xs))
                -> zip (maybe [] F.toList xs) . repeat . fmap Cluster $ ns
                )
        . F.toList
        . flip getGraphLeavesWithParents 0
        . unClusterGraph
        . treeToGraph

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
