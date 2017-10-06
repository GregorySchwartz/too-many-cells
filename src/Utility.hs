{- Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE QuasiQuotes #-}

module Utility
    ( matToRMat
    , scToRMat
    , scaleRMat
    ) where

-- Remote
import Language.R as R
import Language.R.QQ (r)

-- Local
import Types

-- | Convert a mat to an RMatrix.
matToRMat :: Rows -> Cols -> Vals -> R s (RMat s)
matToRMat (Rows rows) (Cols cols) (Vals vals) = do
    [r| library(Matrix) |]
    -- We want rows as observations and columns as features.
    mat <- [r| t(sparseMatrix(i = rows_hs, j = cols_hs, x = vals_hs)) |]
    return . RMat $ mat

-- | Convert a sc structure to an RMatrix.
scToRMat :: SingleCells -> R s (RMat s)
scToRMat sc = do
    [r| library(Matrix) |]

    let sparseMat = matrix sc
        rows = Rows $ fmap (!! 0) sparseMat
        cols = Cols $ fmap (!! 1) sparseMat
        vals = Vals $ fmap (!! 2) sparseMat

    matToRMat rows cols vals

-- | Scale a matrix (z-score normalization).
scaleRMat :: RMat s -> R s (RMatScaled s)
scaleRMat (RMat mat) =
    fmap
        RMatScaled
        [r| mat = scale(mat_hs);
            mat[,colSums(!is.na(mat)) > 0]
        |]
