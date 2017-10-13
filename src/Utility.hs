{- Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE QuasiQuotes #-}

module Utility
    ( matToRMat
    , scToRMat
    , getSCMatrix
    ) where

-- Remote
import qualified Data.Vector as V
import qualified Data.Text as T
import Language.R as R
import Language.R.QQ (r)
import qualified Numeric.LinearAlgebra as H

-- Local
import Types

-- | Convert a mat to an RMatrix.
matToRMat :: SCMatrix -> R s (RMatObsRow s)
matToRMat m = do
    [r| library(jsonlite) |]

    let mString = show . H.toLists . getSCMatrix $ m

    -- We want rows as observations and columns as features.
    mat <- [r| as.matrix(fromJSON(mString_hs)) |]
    return . RMatObsRow $ mat

-- | Convert a sc structure to an RMatrix.
scToRMat :: SingleCells -> R s (RMatObsRowImportant s)
scToRMat sc = do
    [r| library(Matrix) |]

    let rowNamesR = fmap (T.unpack . unGene) . V.toList . rowNames $ sc
        colNamesR = fmap (T.unpack . unCell) . V.toList . colNames $ sc

    mat <- fmap unRMatObsRow . matToRMat . matrix $ sc

    -- Switched row and column names because of transpose.
    -- namedMat <- [r| rownames(mat_hs) = colNamesR_hs
    --                 colnames(mat_hs) = rowNamesR_hs
    --             |]

    return . RMatObsRowImportant $ mat

-- | Get the matrix from an SCMatrix.
getSCMatrix :: SCMatrix -> H.Matrix H.R
getSCMatrix (MatObsRow x)          = x
getSCMatrix (MatObsRowImportant x) = x
