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
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves)
import Control.Monad (join, mfilter)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Language.R as R
import Language.R.QQ (r)
import TextShow (showt)
import qualified "differential" Differential as Diff
import qualified "differential" Plot as Diff
import qualified "differential" Types as Diff
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
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
        fmap (\ !x -> (unRow . _cellRow $ x, _barcode x, (s, Diff.Status . showt $ vs)))
            . mfilter (validCell lm ls)
            . join
            . mconcat
            . fmap (fmap (fromMaybe mempty . snd) . getGraphLeaves gr)
            $ vs

-- | Filter barcodes by labels.
validCell :: Maybe LabelMap -> Maybe (Set.Set Label) -> CellInfo -> Bool
validCell Nothing _ _ = True
validCell _ Nothing _ = True
validCell (Just (LabelMap lm)) (Just ls) cell =
  maybe False (flip Set.member ls)
    . flip Map.lookup lm
    . Id
    . unCell
    . L.view barcode
    $ cell

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

-- | Get the differential expression of two sets of cells.
getDEString :: [(Diff.Name, Double, Diff.PValue, Diff.FDR)]
            -> String
getDEString xs = header <> "\n" <> body
  where
    header = "gene,logFC,pVal,FDR"
    body   = B.unpack
           . CSV.encode
           . fmap ( L.over L._1 Diff.unName
                  . L.over L._3 Diff.unPValue
                  . L.over L._4 Diff.unFDR
                  )
           $ xs

-- | Convert a single cell matrix to a list of Entities with the specified
-- features.
scToEntities :: [Gene]
             -> [(Int, Cell, (Int, Diff.Status))]
             -> SingleCells
             -> [Diff.Entity]
scToEntities genes cellGroups sc =
    concatMap (\x -> fmap (toEntity x) geneIdxs) cellGroups
  where
    mat = getMatrix sc
    toEntity (cellIdx, _, (_, status)) (Gene gene, idx) =
      Diff.Entity (Diff.Name gene) status $ S.lookupWD_SM mat (cellIdx, idx)
    geneIdxs :: [(Gene, Int)]
    geneIdxs = fmap (\ !x -> ( x
                             , fromMaybe (err x)
                             $ V.elemIndex (unGene x) (getColNames sc)
                             )
                    ) genes
    err x = error $ "Feature " <> show x <> " not found for differential."

-- | Get the differential expression plot of features over statuses, filtered by labels.
getSingleDiff :: Maybe LabelMap
              -> SingleCells
              -> ([G.Node], Maybe (Set.Set Label))
              -> ([G.Node], Maybe (Set.Set Label))
              -> [Gene]
              -> ClusterGraph CellInfo
              -> R.R s (R.SomeSEXP s)
getSingleDiff lm sc v1 v2 genes gr = do
  let cellGroups = getStatuses lm v1 v2 gr
      entities = scToEntities genes cellGroups sc

  Diff.plotSingleDiff entities

-- | Combine nodes and labels.
combineNodesLabels :: DiffNodes
                   -> Maybe DiffLabels
                   -> (([G.Node], Maybe (Set.Set Label)), ([G.Node], Maybe (Set.Set Label)))
combineNodesLabels (DiffNodes (v1, v2)) Nothing = ((v1, Nothing), (v2, Nothing))
combineNodesLabels (DiffNodes (v1, v2)) (Just (DiffLabels (l1, l2))) =
  ((v1, Just . Set.fromList $ l1), (v2, Just . Set.fromList $ l2))
