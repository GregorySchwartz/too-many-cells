{- TooManyCells.MakeTree.Preprocess
Gregory W. Schwartz

Collects functions pertaining to preprocessing the data.
-}

{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.Matrix.Preprocess
    ( scaleRMat
    , scaleDenseMat
    , scaleSparseMat
    , filterRMat
    , filterDenseMat
    , filterSparseMat
    , filterSparseMatTest
    , featureSelectionRandomForest
    , removeCorrelated
    , pcaRMat
    , pcaDenseMat
    ) where

-- Remote
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import MachineLearning.PCA (getDimReducer_rv)
import Statistics.Quantile (continuousBy, s)
import qualified Control.Lens as L
import qualified Data.Sparse.Common as S
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as H

-- Local
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
filterDenseMat :: SingleCells MatObsRow -> SingleCells MatObsRow
filterDenseMat sc =
    SingleCells { matrix   = m
                , rowNames = r
                , colNames = c
                , projections = p
                }
  where
    m = MatObsRow . hToSparseMat $ colFilteredMat
    rowFilter = (>= 250) . H.sumElements
    colFilter = (> 0) . H.sumElements
    mat            = sparseToHMat . unMatObsRow . matrix $ sc
    rowFilteredMat = H.fromRows
                   . filter rowFilter
                   . H.toRows
                   $ mat
    colFilteredMat = H.fromColumns
                   . filter colFilter
                   . H.toColumns
                   $ rowFilteredMat
    r = V.ifilter (\i _ -> rowFilter . (H.!) mat $ i)
      . rowNames
      $ sc
    c = V.ifilter (\i _ -> colFilter . H.flatten . (H.¿) mat $ [i])
      . colNames
      $ sc
    p = V.ifilter (\i _ -> colFilter . H.flatten . (H.¿) mat $ [i])
      . projections
      $ sc

-- | Filter a matrix to remove low count cells and genes.
filterSparseMat :: SingleCells MatObsRow -> SingleCells MatObsRow
filterSparseMat sc = SingleCells { matrix   = m
                                 , rowNames = r
                                 , colNames = c
                                 , projections = p
                                 }
  where
    m = MatObsRow colFilteredMat
    rowFilter = (>= 250) . sum
    colFilter = (> 0) . sum
    mat            = unMatObsRow . matrix $ sc
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
      . rowNames
      $ sc
    c = V.ifilter (\i _ -> colFilter . S.extractRow mat' $ i) -- Rows of transpose are faster.
      . colNames
      $ sc
    p = V.ifilter (\i _ -> rowFilter . S.extractRow mat $ i)
      . projections
      $ sc

-- | Filter a matrix to remove low count cells and genes.
filterSparseMatTest :: SingleCells MatObsRow -> SingleCells MatObsRow
filterSparseMatTest sc = SingleCells { matrix   = m
                                 , rowNames = r
                                 , colNames = c
                                 , projections = p
                                 }
  where
    m = MatObsRow colFilteredMat
    mat            = unMatObsRow . matrix $ sc
    rowFilteredMat = S.takeRows 100
                   $ mat
    colFilteredMat = S.takeCols 1000
                   $ rowFilteredMat
    r = V.take 100
      . rowNames
      $ sc
    c = V.take 1000
      . colNames
      $ sc
    p = V.take 100
      . projections
      $ sc

-- | Filter a matrix to remove low count cells. R version.
filterRMat :: RMatObsRow s -> R s (RMatObsRow s)
filterRMat (RMatObsRow mat) =
    fmap RMatObsRow [r| mat = mat_hs[,colSums(mat_hs) >= 250] |]

-- | Perform feature selection on a matrix.
featureSelectionRandomForest :: RMatObsRow s -> R s (RMatObsRowImportant s)
featureSelectionRandomForest (RMatObsRow mat) = do
    [r| suppressPackageStartupMessages(library(randomForest)) |]

    importance   <- [r| randomForest(mat_hs, na.action = na.omit)$importance |]
    importantMat <- [r| mat_hs[,importance_hs > sort(importance_hs, decreasing = TRUE)[10]] |]

    return . RMatObsRowImportant $ importantMat

-- | Remove highly correlated (> 0.6) variables in a matrix.
removeCorrelated :: RMatObsRow s -> R s (RMatObsRowImportant s)
removeCorrelated (RMatObsRow mat) = do
    [r| suppressPackageStartupMessages(library(caret)) |]

    cor   <- [r| cor(mat_hs) |]
    importantMat <- [r| mat_hs[,-findCorrelation(cor_hs, cutoff = 0.6, exact = FALSE)] |]

    return . RMatObsRowImportant $ importantMat

-- | Conduct PCA on a matrix, using components > 5% of variance.
pcaRMat :: RMatObsRow s -> R s (RMatObsRowImportant s)
pcaRMat (RMatObsRow mat) = do
    fmap
        RMatObsRowImportant
        [r| mat = prcomp(t(mat_hs), tol = 0.95)$rotation
        |]

-- | Conduct PCA on a matrix, retaining 80% of variance.
pcaDenseMat :: MatObsRow -> IO MatObsRowImportant
pcaDenseMat (MatObsRow mat) = do
    return
        . MatObsRowImportant
        . hToSparseMat
        . L.view L._3
        . getDimReducer_rv (sparseToHMat mat)
        $ 0.8
