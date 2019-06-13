{- TooManyCells.Differential.Differential
Gregory W. Schwartz

Functions for finding the differential between groups of cells.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.Differential.Differential
    ( scToTwoD
    , getDEGraph
    , getDEString
    , getSingleDiff
    , combineNodesLabels
    , getAllDEGraphKruskalWallis
    , getAllDEStringKruskalWallis
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves, getGraphLeafItems)
import Control.Monad (join, mfilter)
import Data.Function (on)
import Data.List (sort, sortBy, genericLength)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Monoid ((<>))
import Language.R as R
import Language.R.QQ (r)
import TextShow (showt)
import Control.Parallel.Strategies (parMap, withStrategy, parBuffer, rdeepseq)
import qualified "differential" Differential as Diff
import qualified "differential" Plot as Diff
import qualified "differential" Types as Diff
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.FilePath as FP

-- Local
import TooManyCells.Differential.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Convert a single cell matrix to a two dimensional matrix.
scToTwoD :: [(Int, Cell, (Int, Diff.Status))] -> SingleCells -> Diff.TwoDMat
scToTwoD cellGroups sc =
    Diff.TwoDMat rNames cNames statuses nRows nCols . S.toListSM $ filteredMat
  where
    rNames = fmap (Diff.Name . unGene) . V.toList . _colNames $ sc
    cNames = fmap (Diff.Name . unCell . L.view L._2) cellGroups -- We flip row and column because cells are columns here
    statuses = fmap (Diff.Status . showt . L.view (L._3 . L._1)) cellGroups
    nRows    = S.nrows filteredMat
    nCols    = S.ncols filteredMat
    filteredMat = S.fromColsL -- Here the columns should be observations.
                . fmap (S.extractRow (unMatObsRow . _matrix $ sc) . L.view L._1)
                $ cellGroups

-- | Get the indices and statuses for two lists of nodes.
getStatuses
    :: Maybe LabelMap
    -> ([G.Node], Maybe (Set.Set Label))
    -> ([G.Node], Maybe (Set.Set Label))
    -> ClusterGraph CellInfo
    -> [(Int, Cell, (Int, Diff.Status))]
getStatuses lm (v1, l1) (v2, l2) (ClusterGraph gr) =
    sort
        . F.toList
        $ mappend (collapseStatus (1 :: Int) v1 l1) (collapseStatus (2 :: Int) v2 l2)
  where
    collapseStatus s vs ls =
        fmap (\ !x -> (unRow . _cellRow $ x, _barcode x, (s, Diff.Status $ statusName vs ls)))
            . mfilter (validCellInfo lm ls)
            . join
            . mconcat
            . fmap (fmap (fromMaybe mempty . snd) . getGraphLeaves gr)
            $ vs
    statusName vs Nothing = showt vs
    statusName vs (Just ls) =
      (T.intercalate " " . fmap unLabel . Set.toAscList $ ls) <> " " <> showt vs

-- | Filter barcodes by labels.
validCellInfo :: Maybe LabelMap -> Maybe (Set.Set Label) -> CellInfo -> Bool
validCellInfo Nothing _ = const True
validCellInfo _ Nothing = const True
validCellInfo (Just (LabelMap lm)) (Just ls) =
  maybe False (flip Set.member ls)
    . flip Map.lookup lm
    . Id
    . unCell
    . L.view barcode

-- | Filter barcodes by labels.
validCell :: Maybe LabelMap -> Maybe (Set.Set Label) -> Cell -> Bool
validCell Nothing _ = const True
validCell _ Nothing = const True
validCell (Just (LabelMap lm)) (Just ls) =
  maybe False (flip Set.member ls)
    . flip Map.lookup lm
    . Id
    . unCell

-- | Get the differential expression of two sets of cells, filtered by labels.
getDEGraph :: TopN
           -> Maybe LabelMap
           -> SingleCells
           -> ([G.Node], Maybe (Set.Set Label))
           -> ([G.Node], Maybe (Set.Set Label))
           -> ClusterGraph CellInfo
           -> R.R s [(Diff.Name, Double, Diff.PValue, Diff.FDR)]
getDEGraph (TopN topN) lm sc v1 v2 gr = do
    let cellGroups = getStatuses lm v1 v2 gr
        mat        = scToTwoD cellGroups sc

    Diff.edgeR topN mat

-- | Get the differential expression of each cluster to each other cluster using
-- KruskalWallis.
getAllDEGraphKruskalWallis :: TopN
                    -> Maybe LabelMap
                    -> DiffLabels
                    -> SingleCells
                    -> ClusterGraph CellInfo
                    -> [(G.Node, Gene, Diff.Log2Diff, Maybe Diff.PValue, Maybe Diff.FDR)]
getAllDEGraphKruskalWallis topN lm ls sc gr =
  mconcat
    . catMaybes
    . parMap rdeepseq (\n -> compareClusterToOthersKruskalWallis n topN lm ls sc mat gr)
    $ nodes
  where
    nodes = filter (/= 0) . G.nodes . unClusterGraph $ gr -- Don't want to look at root.
    mat = S.transpose . unMatObsRow . L.view matrix $ sc

-- | Get the differential expression of a cluster (n) to all other clusters (ns)
-- using KruskalWallis such that n / ns.
compareClusterToOthersKruskalWallis
  :: G.Node
  -> TopN
  -> Maybe LabelMap
  -> DiffLabels
  -> SingleCells
  -> S.SpMatrix Double
  -> ClusterGraph CellInfo
  -> Maybe [(G.Node, Gene, Diff.Log2Diff, Maybe Diff.PValue, Maybe Diff.FDR)]
compareClusterToOthersKruskalWallis n (TopN topN) lm (DiffLabels (ls1, ls2)) sc mat gr
  | fastFiveCheck nCells || fastFiveCheck nsCells = Nothing
  | otherwise = Just . take topN . sortBy (compare `on` (L.view L._4)) $ res
  where
    fastFiveCheck = (< 5) . length . take 5
    nCells' = F.toList $ getGraphLeafItems gr n
    nCellsSet = Set.fromList . fmap (L.view barcode) $ nCells'
    nCells = fmap (unRow . L.view cellRow)
           . filter (validCellInfo lm ls2)
           $ nCells' -- All cells from node and labels
    nsCells =
      fmap fst
        . filter (\ (_, !x) -> validCell lm ls1 x && not (Set.member x nCellsSet)) -- All cells outside of node and from labels
        . zip [0..]
        . V.toList
        . L.view rowNames
        $ sc
    res = filter (isJust . L.view L._4)
        . ( zipWith
              (\name (!x, !y, !z) -> (n, name, x, y, z))
              (V.toList . L.view colNames $ sc)
          )
        $ Diff.differentialMatrixFeatRow nsCells nCells mat -- Here the matrix rows are features

-- | Get the differential expression of two sets of cells.
getDEString :: [(Diff.Name, Double, Diff.PValue, Diff.FDR)]
            -> B.ByteString
getDEString xs = header <> "\n" <> body
  where
    header = "gene,logFC,pVal,FDR"
    body   = CSV.encode
           . fmap ( L.over L._1 Diff.unName
                  . L.over L._3 Diff.unPValue
                  . L.over L._4 Diff.unFDR
                  )
           $ xs

-- | Get the differential expression of each node to all other nodes using
-- KruskalWallis.
getAllDEStringKruskalWallis
  :: [(G.Node, Gene, Diff.Log2Diff, Maybe Diff.PValue, Maybe Diff.FDR)] -> B.ByteString
getAllDEStringKruskalWallis xs = header <> "\n" <> body
  where
    header = "node,gene,log2FC,pVal,FDR"
    body   = CSV.encode
           . fmap ( L.over L._5 (maybe "NA" (showt . Diff.unFDR))
                  . L.over L._4 (maybe "NA" (showt . Diff.unPValue))
                  . L.over L._3 Diff.unLog2Diff
                  . L.over L._2 unGene
                  )
           $ xs

-- | Convert a single cell matrix to a list of Entities with the specified
-- features. Also aggregates genes by average value or not.
scToEntities :: Aggregate
             -> [Gene]
             -> [(Int, Cell, (Int, Diff.Status))]
             -> SingleCells
             -> [Diff.Entity]
scToEntities aggregate genes cellGroups sc =
    concatMap (\x -> toEntity aggregate x geneIdxs) cellGroups
  where
    mat = getMatrix sc
    toEntity (Aggregate False) (cellIdx, (Cell b), (_, status)) =
      fmap (\ (Gene gene, idx) -> Diff.Entity (Diff.Name gene) status (Diff.Id b)
                                $ S.lookupWD_SM mat (cellIdx, idx)
           )
    toEntity (Aggregate True) (cellIdx, (Cell b), (_, status)) =
      (:[])
        . Diff.Entity
            (Diff.Name . T.intercalate " " . fmap (unGene . fst) $ geneIdxs)
            status
            (Diff.Id b)
        . (/ n)
        . sum
        . fmap (\(_, idx) -> S.lookupWD_SM mat (cellIdx, idx))
    n = genericLength geneIdxs
    geneIdxs :: [(Gene, Int)]
    geneIdxs = fmap (\ !x -> ( x
                             , fromMaybe (err x)
                             $ V.elemIndex (unGene x) (getColNames sc)
                             )
                    ) genes
    err x = error $ "Feature " <> show x <> " not found for differential."

-- | Get the differential expression plot of features (or aggregate of features
-- by average) over statuses, filtered by labels.
getSingleDiff :: Bool
              -> Aggregate
              -> Maybe LabelMap
              -> SingleCells
              -> ([G.Node], Maybe (Set.Set Label))
              -> ([G.Node], Maybe (Set.Set Label))
              -> [Gene]
              -> ClusterGraph CellInfo
              -> R.R s (R.SomeSEXP s)
getSingleDiff normalize aggregate lm sc v1 v2 genes gr = do
  let cellGroups = getStatuses lm v1 v2 gr
      entities = scToEntities aggregate genes cellGroups sc

  Diff.plotSingleDiff normalize entities

-- | Combine nodes and labels.
combineNodesLabels
    :: DiffNodes
    -> Maybe DiffLabels
    -> (([G.Node], Maybe (Set.Set Label)), ([G.Node], Maybe (Set.Set Label)))
combineNodesLabels (DiffNodes (v1, v2)) Nothing = ((v1, Nothing), (v2, Nothing))
combineNodesLabels (DiffNodes (v1, v2)) (Just (DiffLabels (l1, l2))) =
  ((v1, l1), (v2, l2))
