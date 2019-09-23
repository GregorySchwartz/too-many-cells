{- TooManyCells.MakeTree.Preprocess
Gregory W. Schwartz

Collects functions pertaining to preprocessing the data.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Matrix.Preprocess
    ( scaleRMat
    , scaleDenseMat
    , scaleSparseMat
    , logCPMSparseMat
    , uqScaleSparseMat
    , medScaleSparseMat
    , filterRMat
    , filterDenseMat
    , filterNumSparseMat
    , filterWhitelistSparseMat
    , getCellWhitelist
    , featureSelectionRandomForest
    , removeCorrelated
    , pcaRMat
    , pcaDenseSc
    , shiftPositiveSc
    ) where

-- Remote
import Data.Bool (bool)
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import Statistics.Quantile (continuousBy, s)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Radix as V
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as H
import qualified Numeric.LinearAlgebra.HMatrix as H

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

-- | Scale a matrix based on the upper quartile.
uqScaleSparseMat :: MatObsRow -> MatObsRow
uqScaleSparseMat (MatObsRow mat) = MatObsRow
                                 . S.sparsifySM
                                 . S.transposeSM
                                 . S.fromColsL
                                 . fmap uqScaleSparseCell
                                 . S.toRowsL
                                 $ mat

-- | Scale a matrix based on log(CPM + 1).
logCPMSparseMat :: MatObsRow -> MatObsRow
logCPMSparseMat (MatObsRow mat) = MatObsRow
                                . S.sparsifySM
                                . S.transposeSM
                                . S.fromColsL
                                . fmap logCPMSparseCell
                                . S.toRowsL
                                $ mat

-- | Scale a matrix based on the median.
medScaleSparseMat :: MatObsRow -> MatObsRow
medScaleSparseMat (MatObsRow mat) = MatObsRow
                                 . S.sparsifySM
                                 . S.transposeSM
                                 . S.fromColsL
                                 . fmap medScaleSparseCell
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

-- | log(CPM + 1) normalization for cell.
logCPMSparseCell :: S.SpVector Double -> S.SpVector Double
logCPMSparseCell xs = fmap (logBase 2 . (+ 1) . cpm) xs
  where
    cpm x = x / tpm
    tpm = sum xs / 1000000

-- | Upper quartile scale cells.
uqScaleSparseCell :: S.SpVector Double -> S.SpVector Double
uqScaleSparseCell xs = fmap (/ uq) xs
  where
    uq = continuousBy s 3 4
       . VS.fromList
       . filter (/= 0)
       . fmap snd
       . S.toListSV
       $ xs

-- | Median scale cells.
medScaleSparseCell :: S.SpVector Double -> S.SpVector Double
medScaleSparseCell xs = fmap (/ med) xs
  where
    med = continuousBy s 2 4
        . VS.fromList
        . filter (/= 0)
        . fmap snd
        . S.toListSV
        $ xs

-- | Median scale molecules across cells.
scaleDenseMol :: H.Vector H.R -> H.Vector H.R
scaleDenseMol xs = H.cmap (/ med) xs
  where
    med = continuousBy s 2 4 . VS.filter (/= 0) $ xs

-- | Median scale molecules across cells.
scaleSparseMol :: S.SpVector Double -> S.SpVector Double
scaleSparseMol xs = fmap (/ med) xs
  where
    med = continuousBy s 2 4
        . VS.fromList
        . filter (/= 0)
        . fmap snd
        . S.toListSV
        $ xs

-- | Filter a matrix to remove low count cells and genes.
filterDenseMat :: FilterThresholds -> SingleCells -> SingleCells
filterDenseMat (FilterThresholds (rowThresh, colThresh)) sc =
    SingleCells { _matrix   = m
                , _rowNames = r
                , _colNames = c
                }
  where
    m = MatObsRow . hToSparseMat $ filteredMat
    rowFilter = (>= rowThresh) . H.sumElements
    colFilter = (>= colThresh) . H.sumElements
    mat            = sparseToHMat . unMatObsRow . _matrix $ sc
    validRows = Set.fromList
              . fmap fst
              . filter (rowFilter . snd)
              . zip [0..]
              . H.toRows
              $ mat
    validCols = Set.fromList
              . fmap fst
              . filter (colFilter . snd)
              . zip [0..]
              . H.toColumns
              $ mat
    filteredMat = mat
             H.?? ( H.Pos (H.idxs (Set.toAscList validRows))
                  , H.Pos (H.idxs (Set.toAscList validCols))
                  )
    r = V.ifilter (\i _ -> Set.member i validRows)
      . _rowNames
      $ sc
    c = V.ifilter (\i _ -> Set.member i validCols)
      . _colNames
      $ sc

-- | Filter a matrix to remove low count cells and genes.
filterNumSparseMat :: FilterThresholds -> SingleCells -> SingleCells
filterNumSparseMat (FilterThresholds (rowThresh, colThresh)) sc =
    SingleCells { _matrix   = m
                , _rowNames = r
                , _colNames = c
                }
  where
    m = MatObsRow colFilteredMat
    rowFilter = (>= rowThresh) . sum
    colFilter = (>= colThresh) . sum
    mat            = unMatObsRow . _matrix $ sc
    mat'           = S.transposeSM mat
    validRows = Set.fromList
              . fmap fst
              . filter (rowFilter . snd)
              . zip [0..]
              . S.toRowsL
              $ mat
    validCols = Set.fromList
              . fmap fst
              . filter (colFilter . snd)
              . zip [0..]
              . S.toRowsL
              . S.transposeSM -- toRowsL is much faster.
              $ mat
    rowFilteredMat = S.transposeSM
                   . S.fromColsL -- fromRowsL still broken.
                   . fmap snd
                   . filter (flip Set.member validRows . fst)
                   . zip [0..]
                   . S.toRowsL
                   $ mat
    colFilteredMat = S.fromColsL
                   . fmap snd
                   . filter (flip Set.member validCols . fst)
                   . zip [0..]
                   . S.toRowsL -- Rows of transpose are faster.
                   . S.transposeSM
                   $ rowFilteredMat
    r = V.ifilter (\i _ -> Set.member i validRows)
      . _rowNames
      $ sc
    c = V.ifilter (\i _ -> Set.member i validCols)
      . _colNames
      $ sc

-- | Filter a matrix to keep whitelist cells.
filterWhitelistSparseMat :: CellWhitelist
                         -> SingleCells
                         -> SingleCells
filterWhitelistSparseMat (CellWhitelist wl) sc =
    sc { _matrix   = m
       , _rowNames = r
       }
  where
    m = MatObsRow rowFilteredMat
    mat            = unMatObsRow . _matrix $ sc
    validIdx       = V.modify V.sort
                   . V.map fst
                   . V.filter (\(_, !c) -> Set.member c wl)
                   . V.imap (\i v -> (i, v))
                   . _rowNames
                   $ sc
    rowFilteredMat = S.transposeSM
                   . S.fromColsL -- fromRowsL still broken.
                   . fmap (S.extractRow mat)
                   . V.toList
                   $ validIdx
    r = V.map (\x -> fromMaybe (error $ "\nWhitelist row index out of bounds (do the whitelist barcodes match the data?): " <> show x <> " out of " <> (show . length . _rowNames $ sc))
                   . (V.!?) (_rowNames sc)
                   $ x
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
        [r| mat = prcomp(t(mat_hs), tol = 0.95)$x
        |]

-- | Conduct PCA on a matrix, taking the first principal components.
pcaDenseMat :: PCADim -> MatObsRow -> MatObsRow
pcaDenseMat (PCADim pcaDim) (MatObsRow matObs) =
  MatObsRow
    . hToSparseMat
    . H.takeColumns pcaDim
    . H.mul mat
    . (\(u, _, _) -> u)
    . H.svd
    . H.unSym
    . snd
    . H.meanCov
    $ mat
  where
    mat = sparseToHMat matObs

-- | Conduct PCA on a SingleCells, taking the first principal components.
pcaDenseSc :: PCADim -> SingleCells -> SingleCells
pcaDenseSc p@(PCADim n) =
  L.set colNames (V.fromList . fmap (\x -> Gene $ "PCA " <> showt x) $ [1..n])
    . L.over matrix (pcaDenseMat p)

-- | Shift features to positive values.
shiftPositiveMat :: MatObsRow -> MatObsRow
shiftPositiveMat = MatObsRow
              . S.fromColsL
              . fmap (\ xs -> bool xs (shift . S.toDenseListSV $ xs)
                            . any (<= 0)
                            . S.toDenseListSV
                            $ xs
                     )
              . S.toColsL
              . unMatObsRow
  where
    shift xs = S.sparsifySV . S.vr . fmap (\x -> x + minimum xs + 1) $ xs

-- | Shift features to positive values for SingleCells.
shiftPositiveSc :: SingleCells -> SingleCells
shiftPositiveSc = L.over matrix shiftPositiveMat
