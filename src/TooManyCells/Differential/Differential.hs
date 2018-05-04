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
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves)
import Control.Monad (join)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Language.R as R
import Language.R.QQ (r)
import TextShow (showt)
import qualified "differential" Differential as Diff
import qualified "differential" Types as Diff
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Sparse.Common as S
import qualified Data.Vector as V

-- Local
import TooManyCells.Differential.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Convert a single cell matrix to a two dimensional matrix.
scToTwoD :: [(Int, Cell, Diff.Status)] -> SingleCells -> Diff.TwoDMat
scToTwoD cellGroups sc =
    Diff.TwoDMat rNames cNames statuses nRows nCols . S.toListSM $ filteredMat
  where
    rNames = fmap (Diff.Name . unGene) . V.toList . _colNames $ sc
    cNames = fmap (Diff.Name . unCell . L.view L._2) cellGroups -- We flip row and column because cells are columns here
    statuses = fmap (L.view L._3) cellGroups
    nRows    = S.nrows filteredMat
    nCols    = S.ncols filteredMat
    filteredMat = S.fromColsL -- Here the columns should be observations.
                . fmap (S.extractRow (unMatObsRow . _matrix $ sc) . L.view L._1)
                $ cellGroups

-- | Get the indices and statuses for two lists of nodes.
getStatuses
    :: [G.Node] -> [G.Node] -> ClusterGraph CellInfo -> [(Int, Cell, Diff.Status)]
getStatuses v1 v2 (ClusterGraph gr) =
    sort
        . F.toList
        $ mappend (collapseStatus (1 :: Int) v1) (collapseStatus (2 :: Int) v2)
  where
    collapseStatus s =
        fmap (\ !x -> (unRow . _cellRow $ x, _barcode x, Diff.Status . showt $ s))
            . join
            . mconcat
            . fmap (fmap (fromMaybe mempty . snd) . getGraphLeaves gr)

-- | Get the differential expression of two sets of cells.
getDEGraph :: TopN
           -> SingleCells
           -> [G.Node]
           -> [G.Node]
           -> ClusterGraph CellInfo
           -> R.R s [(Diff.Name, Double, Diff.PValue, Diff.FDR)]
getDEGraph (TopN topN) sc v1 v2 gr = do
    let cellGroups = getStatuses v1 v2 gr
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

