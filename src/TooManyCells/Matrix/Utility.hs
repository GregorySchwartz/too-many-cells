{- TooManyCells.Matrix.Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.Matrix.Utility
    ( matToRMat
    , scToRMat
    , sparseToHMat
    , hToSparseMat
    , matToHMat
    , matToSpMat
    , spMatToMat
    ) where

-- Remote
import Control.Monad.State (MonadState (..), State (..), evalState, execState, modify)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Matrix.MatrixMarket (Matrix(RMatrix, IntMatrix), Structure (..))
import Data.Scientific (toRealFloat, Scientific)
import Language.R as R
import Language.R.QQ (r)
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Convert a mat to an RMatrix.
matToRMat :: MatObsRow -> R s (RMatObsRow s)
matToRMat (MatObsRow m) = do
    [r| library(jsonlite) |]

    let mString = show . H.toLists . sparseToHMat $ m

    -- We want rows as observations and columns as features.
    mat <- [r| as.matrix(fromJSON(mString_hs)) |]
    return . RMatObsRow $ mat

-- | Convert a sc structure to an RMatrix.
scToRMat :: SingleCells -> R s (RMatObsRow s)
scToRMat sc = do
    [r| library(Matrix) |]

    let rowNamesR = fmap (T.unpack . unCell) . V.toList . _rowNames $ sc
        colNamesR = fmap (T.unpack . unGene) . V.toList . _colNames $ sc

    mat <- fmap unRMatObsRow . matToRMat . _matrix $ sc

    -- namedMat <- [r| rownames(mat_hs) = rowNamesR_hs
    --                 colnames(mat_hs) = colNamesR_hs
    --             |]

    return . RMatObsRow $ mat

-- | Convert a sparse matrix to an hmatrix.
sparseToHMat :: S.SpMatrix Double -> H.Matrix H.R
sparseToHMat mat = H.assoc (S.dimSM mat) 0
                 . fmap (\(!x, !y, !z) -> ((x, y), z))
                 . S.toDenseListSM
                 $ mat

-- | Convert a sparse matrix to an hmatrix.
hToSparseMat :: H.Matrix H.R -> S.SpMatrix Double
hToSparseMat =
    S.transposeSM . S.sparsifySM . S.fromColsL . fmap S.vr . H.toLists

-- | Convert a Matrix to an hmatrix Matrix. Assumes matrix market is 1 indexed.
matToHMat :: Matrix Scientific -> H.Matrix H.R
matToHMat (RMatrix size _ _ xs) =
    H.assoc size 0
        . fmap (\(!x, !y, !z) -> ((fromIntegral x - 1, fromIntegral y - 1), toRealFloat z))
        $ xs
matToHMat (IntMatrix size _ _ xs) =
    H.assoc size 0
        . fmap (\(!x, !y, !z) -> ((fromIntegral x - 1, fromIntegral y - 1), fromIntegral z))
        $ xs
matToHMat _ = error "Input matrix is not a Real matrix."

-- | Convert a Matrix to a sparse matrix.
matToSpMat :: Matrix Scientific -> S.SpMatrix Double
matToSpMat (RMatrix size _ _ xs) =
    S.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, toRealFloat z))
        $ xs
matToSpMat (IntMatrix size _ _ xs) =
    S.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, fromIntegral z))
        $ xs
matToSpMat _ = error "Input matrix is not a Real matrix."

-- | Convert a sparse matrix to a Matrix.
spMatToMat :: S.SpMatrix Double -> Matrix Double
spMatToMat mat = RMatrix (S.dimSM mat) (S.nzSM mat) General
               . fmap (\(!x, !y, !z) -> (x + 1, y + 1, z)) . S.toListSM
               $ mat
