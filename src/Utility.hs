{- Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE QuasiQuotes #-}

module Utility
    ( matToRMat
    , scToRMat
    , getMostFrequent
    ) where

-- Remote
import Data.Function (on)
import Data.List (maximumBy)
import Language.R as R
import Language.R.QQ (r)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
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

-- | Get the most frequent element of a list.
getMostFrequent :: (Eq a, Ord a) => [a] -> a
getMostFrequent = fst
                . maximumBy (compare `on` snd)
                . Map.toAscList
                . Map.fromListWith (+)
                . flip zip ([1,1..] :: [Double])
