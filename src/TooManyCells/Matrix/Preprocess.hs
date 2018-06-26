{- TooManyCells.MakeTree.Preprocess
Gregory W. Schwartz

Collects functions pertaining to preprocessing the data.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.Matrix.Preprocess
    ( scaleRMat
    , scaleDenseMat
    , scaleSparseMat
    , filterRMat
    , filterDenseMat
    , filterNumSparseMat
    , filterWhitelistSparseMat
    , getCellWhitelist
    , featureSelectionRandomForest
    , removeCorrelated
    , pcaRMat
    , pcaDenseMat
    ) where

-- Remote
import Data.List (sort)
import Data.Maybe (fromMaybe)
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import MachineLearning.PCA (getDimReducer_rv)
import Statistics.Quantile (continuousBy, s)
import qualified Control.Lens as L
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as H

-- Local
import TooManyCells.File.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility

-- | Scale a matrix.
scaleRMat :: RMatObsRow s -> R s (RMatObsRow s)
scaleRMat (RMatObsRow mat) = do
    fmap
        RMatObsRow
        [r| mat = scale(t(mat_hs));
            t(mat[,colSums(!is.na(mat)) > 0])
        |]

-- | Scale a matrix based on the library size.
scaleDenseMat :: MatObsRow -> MatObsRow
scaleDenseMat (MatObsRow mat) = MatObsRow
                              . hToSparseMat
                              . H.fromColumns
                              . fmap scaleDenseMol
                              . H.toColumns
                              . H.fromRows
                              . fmap scaleDenseCell
                              . H.toRows
                              . sparseToHMat
                              $ mat

-- | Scale a matrix based on the library size.
scaleSparseMat :: MatObsRow -> MatObsRow
scaleSparseMat (MatObsRow mat) = MatObsRow
                               . S.sparsifySM
                               . S.fromColsL
                               . fmap scaleSparseMol
                               . S.toRowsL -- A bit confusing, but this returns the original columns due to the earlier fromColsL . toRowsL.
                               . S.fromColsL
                               . fmap scaleSparseCell
                               . S.toRowsL
                               $ mat

-- | Scale a cell by the library size.
scaleDenseCell :: H.Vector H.R -> H.Vector H.R
scaleDenseCell xs = H.cmap (/ total) xs
  where
    total = H.sumElements xs

-- | Scale a cell by the library size.
scaleSparseCell :: S.SpVector Double -> S.SpVector Double
scaleSparseCell xs = fmap (/ total) xs
  where
    total = sum xs

-- | Median scale molecules across cells.
scaleDenseMol :: H.Vector H.R -> H.Vector H.R
scaleDenseMol xs = H.cmap (/ med) xs
  where
    med = continuousBy s 2 4 . VS.filter (> 0) $ xs

-- | Median scale molecules across cells.
scaleSparseMol :: S.SpVector Double -> S.SpVector Double
scaleSparseMol xs = fmap (/ med) xs
  where
    med = continuousBy s 2 4
        . VS.filter (> 0)
        . VS.fromList
        . S.toDenseListSV
        $ xs

-- | Filter a matrix to remove low count cells and genes.
filterDenseMat :: SingleCells -> SingleCells
filterDenseMat sc =
    SingleCells { _matrix   = m
                , _rowNames = r
                , _colNames = c
                , _projections = p
                }
  where
    m = MatObsRow . hToSparseMat $ colFilteredMat
    rowFilter = (>= 250) . H.sumElements
    colFilter = (> 0) . H.sumElements
    mat            = sparseToHMat . unMatObsRow . _matrix $ sc
    rowFilteredMat = H.fromRows
                   . filter rowFilter
                   . H.toRows
                   $ mat
    colFilteredMat = H.fromColumns
                   . filter colFilter
                   . H.toColumns
                   $ rowFilteredMat
    r = V.ifilter (\i _ -> rowFilter . (H.!) mat $ i)
      . _rowNames
      $ sc
    c = V.ifilter (\i _ -> colFilter . H.flatten . (H.¿) mat $ [i])
      . _colNames
      $ sc
    p = V.ifilter (\i _ -> colFilter . H.flatten . (H.¿) mat $ [i])
      . _projections
      $ sc

-- | Filter a matrix to remove low count cells and genes.
filterNumSparseMat :: FilterThresholds -> SingleCells -> SingleCells
filterNumSparseMat (FilterThresholds (rowThresh, colThresh)) sc =
    SingleCells { _matrix   = m
                , _rowNames = r
                , _colNames = c
                , _projections = p
                }
  where
    m = MatObsRow colFilteredMat
    rowFilter = (>= rowThresh) . sum
    colFilter = (>= colThresh) . sum
    mat            = unMatObsRow . _matrix $ sc
    mat'           = S.transposeSM mat
    rowFilteredMat = S.transposeSM
                   . S.fromColsL
                   . filter rowFilter
                   . S.toRowsL
                   $ mat
    colFilteredMat = S.fromColsL
                   . filter colFilter
                   . S.toRowsL -- Rows of transpose are faster.
                   . S.transposeSM
                   $ rowFilteredMat
    r = V.ifilter (\i _ -> rowFilter . S.extractRow mat $ i)
      . _rowNames
      $ sc
    c = V.ifilter (\i _ -> colFilter . S.extractRow mat' $ i) -- Rows of transpose are faster.
      . _colNames
      $ sc
    p = V.ifilter (\i _ -> rowFilter . S.extractRow mat $ i)
      . _projections
      $ sc

-- | Filter a matrix to keep whitelist cells.
filterWhitelistSparseMat :: CellWhitelist
                         -> SingleCells
                         -> SingleCells
filterWhitelistSparseMat (CellWhitelist wl) sc =
    sc { _matrix   = m
       , _rowNames = r
       , _projections = p
       }
  where
    m = MatObsRow rowFilteredMat
    mat            = unMatObsRow . _matrix $ sc
    validIdx       = sort
                   . fmap fst
                   . filter (\(_, !c) -> Set.member c wl)
                   . zip [0..]
                   . V.toList
                   . _rowNames
                   $ sc
    rowFilteredMat = S.transposeSM
                   . S.fromColsL
                   . fmap (S.extractRow mat)
                   $ validIdx
    r = V.fromList
      . fmap ( fromMaybe (error "\nWhitelist row index out of bounds (do the whitelist barcodes match the data?).")
             . (V.!?) (_rowNames sc)
             )
      $ validIdx
    p = V.fromList
      . fmap ( (error "\nWhitelist projection index out of bounds (do the whitelist barcodes match the data?).")
             . (V.!?) (_projections sc)
             )
      $ validIdx

-- | Get a cell white list from a file.
getCellWhitelist :: CellWhitelistFile -> IO CellWhitelist
getCellWhitelist (CellWhitelistFile file) = do
    contents <- T.readFile file

    let whiteList = CellWhitelist
                  . Set.fromList
                  . fmap Cell
                  . filter (not . T.null)
                  . T.lines
                  $ contents

    return whiteList

-- | Filter a matrix to remove low count cells. R version.
filterRMat :: RMatObsRow s -> R s (RMatObsRow s)
filterRMat (RMatObsRow mat) =
    fmap RMatObsRow [r| mat = mat_hs[,colSums(mat_hs) >= 250] |]

-- | Perform feature selection on a matrix.
featureSelectionRandomForest :: RMatObsRow s -> R s (RMatObsRow s)
featureSelectionRandomForest (RMatObsRow mat) = do
    [r| suppressPackageStartupMessages(library(randomForest)) |]

    importance   <- [r| randomForest(mat_hs, na.action = na.omit)$importance |]
    importantMat <- [r| mat_hs[,importance_hs > sort(importance_hs, decreasing = TRUE)[10]] |]

    return . RMatObsRow $ importantMat

-- | Remove highly correlated (> 0.6) variables in a matrix.
removeCorrelated :: RMatObsRow s -> R s (RMatObsRow s)
removeCorrelated (RMatObsRow mat) = do
    [r| suppressPackageStartupMessages(library(caret)) |]

    cor   <- [r| cor(mat_hs) |]
    importantMat <- [r| mat_hs[,-findCorrelation(cor_hs, cutoff = 0.6, exact = FALSE)] |]

    return . RMatObsRow $ importantMat

-- | Conduct PCA on a matrix, using components > 5% of variance.
pcaRMat :: RMatObsRow s -> R s (RMatObsRow s)
pcaRMat (RMatObsRow mat) = do
    fmap
        RMatObsRow
        [r| mat = prcomp(t(mat_hs), tol = 0.95)$rotation
        |]

-- | Conduct PCA on a matrix, retaining a percentage of variance.
pcaDenseMat :: PCAVar -> MatObsRow -> MatObsRow
pcaDenseMat (PCAVar pcaVar) (MatObsRow mat) = do
    MatObsRow
        . hToSparseMat
        . L.view L._3
        . getDimReducer_rv (sparseToHMat mat)
        $ pcaVar
