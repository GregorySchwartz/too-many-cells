{- Preprocess
Gregory W. Schwartz

Collects functions pertaining to preprocessing the data.
-}

{-# LANGUAGE QuasiQuotes #-}

module Preprocess
    ( scaleRMat
    , scaleMat
    , filterRMat
    , filterMat
    , featureSelectionRandomForest
    , removeCorrelated
    , pcaRMat
    , pcaMat
    ) where

-- Remote
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import MachineLearning.PCA (getDimReducer_rv)
import Statistics.Quantile (continuousBy, s)
import System.IO (hPutStrLn, stderr)
import qualified Control.Lens as L
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as H

-- Local
import Types

-- | Scale a matrix.
scaleRMat :: RMatObsRow s -> R s (RMatObsRow s)
scaleRMat (RMatObsRow mat) = do
    io $ hPutStrLn stderr "Scaling matrix."

    fmap
        RMatObsRow
        [r| mat = scale(t(mat_hs));
            t(mat[,colSums(!is.na(mat)) > 0])
        |]

-- | Scale a matrix based on the library size.
scaleMat :: MatObsRow -> IO MatObsRow
scaleMat (MatObsRow mat) = do
    hPutStrLn stderr "Scaling matrix."

    return
        . MatObsRow
        . H.fromColumns
        . fmap scaleMol
        . H.toColumns
        . H.fromRows
        . fmap scaleCell
        . H.toRows
        $ mat

-- | Scale a cell by the library size.
scaleCell :: H.Vector H.R -> H.Vector H.R
scaleCell xs = H.cmap (/ total) xs
  where
    total = H.sumElements xs

-- | Median scale molecules across cells.
scaleMol :: H.Vector H.R -> H.Vector H.R
scaleMol xs = H.cmap (/ med) xs
  where
    med = continuousBy s 2 4 . VS.filter (> 0) $ xs

-- | Filter a matrix to remove low count cells and genes.
filterMat :: SingleCells MatObsRow -> IO (SingleCells MatObsRow)
filterMat sc = do
    hPutStrLn stderr "Filtering matrix."

    let m = MatObsRow colFilteredMat
        rowFilter = (>= 250) . H.sumElements
        colFilter = (> 0) . H.sumElements
        mat            = unMatObsRow . matrix $ sc
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
        c = V.ifilter (\i _ -> colFilter . (H.!) (H.tr mat) $ i)
          . colNames
          $ sc

    return $ SingleCells { matrix   = m
                         , rowNames = r
                         , colNames = c
                         }

-- | Filter a matrix to remove low count cells. R version.
filterRMat :: RMatObsRow s -> R s (RMatObsRow s)
filterRMat (RMatObsRow mat) = do
    io $ hPutStrLn stderr "Filtering matrix."

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
    io $ hPutStrLn stderr "Calculating PCA."

    fmap
        RMatObsRowImportant
        [r| mat = prcomp(t(mat_hs), tol = 0.95)$rotation
        |]

-- | Conduct PCA on a matrix, retaining 80% of variance.
pcaMat :: MatObsRow -> IO MatObsRowImportant
pcaMat (MatObsRow mat) = do
    hPutStrLn stderr "Calculating PCA."

    return
        . MatObsRowImportant
        . L.view L._3
        . getDimReducer_rv mat
        $ 0.8
