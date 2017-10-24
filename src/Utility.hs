{- Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE QuasiQuotes #-}

module Utility
    ( matToRMat
    , scToRMat
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
matToRMat :: MatObsRow -> R s (RMatObsRow s)
matToRMat (MatObsRow m) = do
    [r| library(jsonlite) |]

    let mString = show . H.toLists $ m

    -- We want rows as observations and columns as features.
    mat <- [r| as.matrix(fromJSON(mString_hs)) |]
    return . RMatObsRow $ mat

-- | Convert a sc structure to an RMatrix.
scToRMat :: SingleCells MatObsRow -> R s (RMatObsRow s)
scToRMat sc = do
    [r| library(Matrix) |]

    let rowNamesR = fmap (T.unpack . unCell) . V.toList . rowNames $ sc
        colNamesR = fmap (T.unpack . unGene) . V.toList . colNames $ sc

    mat <- fmap unRMatObsRow . matToRMat . matrix $ sc

    -- namedMat <- [r| rownames(mat_hs) = rowNamesR_hs
    --                 colnames(mat_hs) = colNamesR_hs
    --             |]

    return . RMatObsRow $ mat
