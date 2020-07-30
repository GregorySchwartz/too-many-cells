{- TooManyCells.MakeTree.Preprocess
Gregory W. Schwartz

Collects functions pertaining to preprocessing the data.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Matrix.Preprocess
    ( scaleRMat
    , scaleDenseMat
    , scaleSparseMat
    , totalScaleSparseMat
    , logCPMSparseMat
    , uqScaleSparseMat
    , medScaleSparseMat
    , quantileScaleSparseMat
    , centerScaleSparseCell
    , tfidfScaleSparseMat
    , filterRMat
    , filterDenseMat
    , filterNumSparseMat
    , filterWhitelistSparseMat
    , getCellWhitelist
    , featureSelectionRandomForest
    , removeCorrelated
    , pcaRMat
    , pcaDenseSc
    , pcaSparseSc
    , lsaSparseSc
    , svdSparseSc
    , shiftPositiveSc
    , pcaDenseMat
    , pcaSparseMat
    , lsaSparseMat
    , svdSparseMat
    , shiftPositiveMat
    , emptyMatErr
    , labelRows
    , labelCols
    , fastBinJoinRows
    , fastBinJoinCols
    , transformChrRegions
    ) where

-- Remote
import BirchBeer.Types (LabelMap (..), Id (..), Label (..), Feature (..))
import Data.Bool (bool)
import Data.List (sort, foldl', transpose)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, isJust)
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Spectral.Sparse (B1 (..), B2 (..), b1ToB2)
import Statistics.Quantile (quantile, s)
import Statistics.Sample (mean, stdDev)
import TextShow (showt)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet
import qualified Data.IntervalMap.Interval as IntervalMap
import qualified Data.IntervalMap.Strict as IntervalMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Radix as V
import qualified Data.Vector.Storable as VS
import qualified Math.Clustering.Spectral.Sparse as S
import qualified Numeric.LinearAlgebra as H
import qualified Numeric.LinearAlgebra.Devel as H
import qualified Numeric.LinearAlgebra.HMatrix as H
import qualified Numeric.LinearAlgebra.SVD.SVDLIBC as SVD

-- Local
import TooManyCells.File.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility

-- | Empty matrix error.
emptyMatErr :: String -> String
emptyMatErr checkType = "Matrix is empty in " <> checkType <> ". Check --filter-thresholds, --normalization, or the input matrix for over filtering or incorrect input format."

-- | Check if valid features or cells are empty, error if so.
emptyMatCheckErr :: String -> [a] -> [a]
emptyMatCheckErr checkType xs = bool xs (error $ emptyMatErr checkType) . null $ xs

-- | Scale a matrix.
scaleRMat :: RMatObsRow s -> R s (RMatObsRow s)
scaleRMat (RMatObsRow mat) = do
    fmap
        RMatObsRow
        [r| mat = scale(t(mat_hs));
            t(mat[,colSums(!is.na(mat)) > 0])
        |]

-- | Scale a matrix based on the library size for each cell and median feature
-- size.
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

-- | Scale a matrix based on the library size for each cell and median feature
-- size.
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

-- | Scale a matrix based on the library size.
totalScaleSparseMat :: MatObsRow -> MatObsRow
totalScaleSparseMat = MatObsRow
                    . S.sparsifySM
                    . S.fromRowsL
                    . fmap scaleSparseCell
                    . S.toRowsL
                    . unMatObsRow

-- | Scale a matrix based on the upper quartile.
uqScaleSparseMat :: MatObsRow -> MatObsRow
uqScaleSparseMat (MatObsRow mat) = MatObsRow
                                 . S.sparsifySM
                                 . S.fromRowsL
                                 . fmap uqScaleSparseCell
                                 . S.toRowsL
                                 $ mat

-- | Scale a matrix based on log(CPM + 1).
logCPMSparseMat :: Double -> MatObsRow -> MatObsRow
logCPMSparseMat b (MatObsRow mat) = MatObsRow
                                  . S.sparsifySM
                                  . S.fromRowsL
                                  . fmap (logCPMSparseCell b)
                                  . S.toRowsL
                                  $ mat

-- | Scale a matrix based on the median.
medScaleSparseMat :: MatObsRow -> MatObsRow
medScaleSparseMat (MatObsRow mat) = MatObsRow
                                 . S.sparsifySM
                                 . S.fromRowsL
                                 . fmap medScaleSparseCell
                                 . S.toRowsL
                                 $ mat

-- | Scale a matrix based quantile normalization. Will ignore 0s if missing from
-- input.
quantileScaleSparseMat :: MatObsRow -> MatObsRow
quantileScaleSparseMat (MatObsRow mat) =
  MatObsRow
    . S.sparsifySM
    . fmap (flip (IMap.findWithDefault 0) totalRankMap)
    $ rankMat
  where
    totalRankMap = IMap.fromList
                 . zip [1,2..]
                 . fmap (Fold.fold avg)
                 . transpose
                 . fmap (sort . fmap snd . S.toListSV)
                 . S.toRowsL
                 $ mat
    avg = (/) <$> Fold.sum <*> Fold.genericLength
    rankMat :: S.SpMatrix Int
    rankMat = S.fromRowsL . fmap rankTransform . S.toRowsL $ mat
    rankTransform :: S.SpVector Double -> S.SpVector Int
    rankTransform xs = fmap (flip (Map.findWithDefault 0) rankMap) xs
      where
        rankMap :: Map.Map Double Int
        rankMap = Map.fromList
                . flip zip [1,2..]
                . Set.toList
                . Set.fromList
                . sort
                . fmap snd
                . S.toListSV
                $ xs

-- | TF-IDF normalization.
tfidfScaleSparseMat :: MatObsRow -> MatObsRow
tfidfScaleSparseMat = MatObsRow . unB2 . b1ToB2 . B1 . unMatObsRow

-- | Scale a cell by the library size.
scaleDenseCell :: H.Vector H.R -> H.Vector H.R
scaleDenseCell xs = H.cmap (/ total) xs
  where
    total = H.sumElements xs

-- | Scale a cell by the library size.
scaleSparseCell :: S.SpVector Double -> S.SpVector Double
scaleSparseCell xs = fmap (/ total) xs
  where
    total = foldl' (+) 0 xs

-- | log(CPM + 1) normalization for cell.
logCPMSparseCell :: Double -> S.SpVector Double -> S.SpVector Double
logCPMSparseCell b xs = fmap (logBase b . (+ 1) . cpm) xs
  where
    cpm x = x / tpm
    tpm = foldl' (+) 0 xs / 1000000

-- | Upper quartile scale cells.
uqScaleSparseCell :: S.SpVector Double -> S.SpVector Double
uqScaleSparseCell xs = fmap (/ uq) xs
  where
    uq = quantile s 3 4
       . VS.fromList
       . filter (/= 0)
       . fmap snd
       . S.toListSV
       $ xs

-- | Median scale cells.
medScaleSparseCell :: S.SpVector Double -> S.SpVector Double
medScaleSparseCell xs = fmap (/ med) xs
  where
    med = quantile s 2 4
        . VS.fromList
        . filter (/= 0)
        . fmap snd
        . S.toListSV
        $ xs

-- | Center and scale a cell.
centerScaleSparseCell :: S.SpVector Double -> S.SpVector Double
centerScaleSparseCell xs = fmap standard xs
  where
    standard x
      | sigma == 0 = 0
      | otherwise  = (x - mu) / sigma
    mu = mean v
    sigma = stdDev v
    v = S.toVector xs

-- | Center a sparse vector.
centerSparseVector :: S.SpVector Double -> S.SpVector Double
centerSparseVector xs = fmap center xs
  where
    center x = x - mu
    mu = mean $ S.toVector xs

-- | Median scale molecules across cells.
scaleDenseMol :: H.Vector H.R -> H.Vector H.R
scaleDenseMol xs = H.cmap (/ med) xs
  where
    med = quantile s 2 4 . VS.filter (/= 0) $ xs

-- | Median scale molecules across cells.
scaleSparseMol :: S.SpVector Double -> S.SpVector Double
scaleSparseMol xs = fmap (/ med) xs
  where
    med = quantile s 2 4
        . VS.fromList
        . filter (/= 0)
        . fmap snd
        . S.toListSV
        $ xs

-- | Filter a matrix to remove low count cells and features.
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
    validRows = ISet.fromList
              . emptyMatCheckErr "cells"
              . fmap fst
              . filter (rowFilter . snd)
              . zip [0..]
              . H.toRows
              $ mat
    validCols = ISet.fromList
              . emptyMatCheckErr "features"
              . fmap fst
              . filter (colFilter . snd)
              . zip [0..]
              . H.toColumns
              $ mat
    filteredMat = mat
             H.?? ( H.Pos (H.idxs (ISet.toAscList validRows))
                  , H.Pos (H.idxs (ISet.toAscList validCols))
                  )
    r = V.ifilter (\i _ -> ISet.member i validRows)
      . _rowNames
      $ sc
    c = V.ifilter (\i _ -> ISet.member i validCols)
      . _colNames
      $ sc

-- | Filter a matrix to remove low count cells and features.
filterNumSparseMat :: FilterThresholds -> SingleCells -> SingleCells
filterNumSparseMat (FilterThresholds (rowThresh, colThresh)) sc =
    SingleCells { _matrix   = m
                , _rowNames = r
                , _colNames = c
                }
  where
    m = MatObsRow colFilteredMat
    rowFilter = (>= rowThresh) . foldl' (+) 0
    colFilter = (>= colThresh) . foldl' (+) 0
    mat            = unMatObsRow . _matrix $ sc
    validRows = ISet.fromList
              . emptyMatCheckErr "cells"
              . fmap fst
              . filter (rowFilter . snd)
              . zip [0..]
              . S.toRowsL
              $ mat
    validCols = ISet.fromList
              . emptyMatCheckErr "features"
              . fmap fst
              . filter (colFilter . snd)
              . zip [0..]
              . S.toRowsL
              . S.transposeSM -- toRowsL is much faster.
              $ mat
    rowFilteredMat = S.fromRowsL -- fromRowsL fixed.
                   . fmap snd
                   . filter (flip ISet.member validRows . fst)
                   . zip [0..]
                   . S.toRowsL
                   $ mat
    colFilteredMat = S.fromColsL
                   . fmap snd
                   . filter (flip ISet.member validCols . fst)
                   . zip [0..]
                   . S.toRowsL -- Rows of transpose are faster.
                   . S.transposeSM
                   $ rowFilteredMat
    r = V.ifilter (\i _ -> ISet.member i validRows)
      . _rowNames
      $ sc
    c = V.ifilter (\i _ -> ISet.member i validCols)
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
                   . V.filter (\(_, (Cell !c)) -> HSet.member c wl)
                   . V.imap (\i v -> (i, v))
                   . _rowNames
                   $ sc
    rowFilteredMat = S.fromRowsL -- fromRowsL fixed.
                   . fmap (S.extractRow mat)
                   . emptyMatCheckErr "cells"
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
                  . HSet.fromList
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

-- | Conduct PCA on a SingleCells, taking the first principal components.
pcaDenseSc :: DropDimensionFlag -> PCADim -> SingleCells -> SingleCells
pcaDenseSc df p@(PCADim n) sc =
  L.set colNames (V.fromList . fmap (\x -> Feature $ "PCA " <> showt x) $ [begin..end])
    . L.over matrix (pcaDenseMat df p)
    $ sc
  where
    begin = bool 1 (n + 1) . unDropDimensionFlag $ df
    end   = bool n ((S.ncols . unMatObsRow . L.view matrix $ sc) - n + 1)
          . unDropDimensionFlag
          $ df

-- | Conduct PCA on a SingleCells, taking the first principal components.
pcaSparseSc :: DropDimensionFlag -> PCADim -> SingleCells -> SingleCells
pcaSparseSc df p@(PCADim n) sc =
  L.set colNames (V.fromList . fmap (\x -> Feature $ "PCA " <> showt x) $ [begin..end])
    . L.over matrix (pcaSparseMat df p)
    $ sc
  where
    begin = bool 1 (n + 1) . unDropDimensionFlag $ df
    end   = bool n ((S.ncols . unMatObsRow . L.view matrix $ sc) - n + 1)
          . unDropDimensionFlag
          $ df

-- | Conduct LSA on a SingleCells, taking the first singular values.
lsaSparseSc :: DropDimensionFlag -> LSADim -> SingleCells -> SingleCells
lsaSparseSc df l@(LSADim n) sc =
  L.set colNames (V.fromList . fmap (\x -> Feature $ "LSA " <> showt x) $ [begin..end])
    . L.over matrix (lsaSparseMat df l)
    $ sc
  where
    begin = bool 1 (n + 1) . unDropDimensionFlag $ df
    end   = bool n ((S.ncols . unMatObsRow . L.view matrix $ sc) - n + 1)
          . unDropDimensionFlag
          $ df

-- | Conduct SVD on a SingleCells, taking the first singular values.
svdSparseSc :: DropDimensionFlag -> SVDDim -> SingleCells -> SingleCells
svdSparseSc df s@(SVDDim n) sc =
  L.set colNames (V.fromList . fmap (\x -> Feature $ "SVD " <> showt x) $ [begin..end])
    . L.over matrix (svdSparseMat df s)
    $ sc
  where
    begin = bool 1 (n + 1) . unDropDimensionFlag $ df
    end   = bool n ((S.ncols . unMatObsRow . L.view matrix $ sc) - n + 1)
          . unDropDimensionFlag
          $ df

-- | Obtain the right singular vectors from N to E on of a sparse
-- matrix.
sparseRightSVD :: Int -> Int -> S.SpMatrix Double -> [S.SpVector Double]
sparseRightSVD n e =
  fmap (S.sparsifySV . S.fromListDenseSV e . drop (n - 1) . H.toList)
    . H.toColumns
    . (\(_, _, !z) -> z)
    . SVD.sparseSvd (e + (n - 1))
    . H.mkCSR
    . fmap (\(!i, !j, !x) -> ((i, j), x))
    . S.toListSM

-- | Conduct PCA on a matrix, retaining the specified number of dimensions.
pcaDenseMat :: DropDimensionFlag -> PCADim -> MatObsRow -> MatObsRow
pcaDenseMat df (PCADim pcaDim) (MatObsRow matObs) =
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
    begin = bool 1 (pcaDim + 1) . unDropDimensionFlag $ df
    end   = bool pcaDim (S.ncols matObs - pcaDim)
          . unDropDimensionFlag
          $ df

-- | Conduct SVD on a matrix, retaining the specified number of dimensions.
pcaSparseMat :: DropDimensionFlag -> PCADim -> MatObsRow -> MatObsRow
pcaSparseMat df (PCADim pcaDim) (MatObsRow mat) =
  MatObsRow
    . (S.#~#) scaled
    . S.fromRowsL
    . sparseRightSVD begin end
    $ scaled
  where
    scaled = S.fromColsL
           . fmap centerScaleSparseCell
           . S.toRowsL -- Scale features for PCA
           . S.transpose
           $ mat
    begin = bool 1 (pcaDim + 1) . unDropDimensionFlag $ df
    end   = bool pcaDim (S.ncols mat - pcaDim) . unDropDimensionFlag $ df

-- | Conduct LSA on a matrix, retaining the specified number of dimensions.
lsaSparseMat :: DropDimensionFlag -> LSADim -> MatObsRow -> MatObsRow
lsaSparseMat df (LSADim lsaDim) (MatObsRow mat) =
  MatObsRow
    . S.fromRowsL
    . S.secondLeft begin end
    . unB2
    . b1ToB2
    . B1
    $ mat
  where
    begin = bool 1 (lsaDim + 1) . unDropDimensionFlag $ df
    end   = bool lsaDim (S.ncols mat - lsaDim) . unDropDimensionFlag $ df

-- | Conduct SVD on a matrix, retaining the specified number of dimensions.
svdSparseMat :: DropDimensionFlag -> SVDDim -> MatObsRow -> MatObsRow
svdSparseMat df (SVDDim svdDim) (MatObsRow mat) =
  MatObsRow
    . S.fromRowsL
    . S.secondLeft begin end
    . S.fromRowsL
    . fmap centerScaleSparseCell
    . S.toRowsL
    $ mat
  where
    begin = bool 1 (svdDim + 1) . unDropDimensionFlag $ df
    end   = bool svdDim (S.ncols mat - svdDim) . unDropDimensionFlag $ df


-- | Shift features to positive values.
shiftPositiveMat :: MatObsRow -> MatObsRow
shiftPositiveMat = MatObsRow
              . S.fromColsL
              . fmap (\ xs -> bool xs (shift . S.toDenseListSV $ xs)
                            . any (< 0)
                            . S.toDenseListSV
                            $ xs
                     )
              . S.toRowsL -- toRowsL much faster than toColsL
              . S.transpose
              . unMatObsRow
  where
    shift xs = S.sparsifySV . S.vr . fmap (+ (abs $ minimum xs)) $ xs

-- | Shift features to positive values for SingleCells.
shiftPositiveSc :: SingleCells -> SingleCells
shiftPositiveSc = L.over matrix shiftPositiveMat

-- | Optionally give the filepath as a label to the rows.
labelRows :: Maybe CustomLabel -> SingleCells -> LabelMat
labelRows Nothing sc = LabelMat (sc, Nothing)
labelRows (Just (CustomLabel l)) sc =
  LabelMat (L.set rowNames newRowNames sc, Just labelMap)
  where
    labelMap = LabelMap
             . Map.fromList
             . flip zip (repeat (Label l))
             . fmap (Id . unCell)
             . V.toList
             $ newRowNames
    newRowNames = fmap labelRow . L.view rowNames $ sc
    labelRow (Cell x) = Cell $ x <> "-" <> l

-- | Optionally give the filepath as a label to the columns when transposing
-- matrix.
labelCols :: Maybe CustomLabel -> SingleCells -> LabelMat
labelCols Nothing sc = LabelMat (sc, Nothing)
labelCols (Just (CustomLabel l)) sc =
  LabelMat (L.set colNames newColNames sc, Just labelMap)
  where
    labelMap = LabelMap
             . Map.fromList
             . flip zip (repeat (Label l))
             . fmap (Id . unFeature)
             . V.toList
             $ newColNames
    newColNames = fmap labelCol . L.view colNames $ sc
    labelCol (Feature x) = Feature $ x <> "-" <> l

-- | Add or remove a single character to speed up joining if region data is
-- binned across rows before transposition.
fastBinJoinRows :: Maybe BinWidth -> Bool -> SingleCells -> SingleCells
fastBinJoinRows binWidth joining =
  bool id (L.over rowNames (fmap (Cell . j . unCell))) $ isJust binWidth
    where
      j = if joining then T.cons 'B' else T.drop 1

-- | Add or remove a single character to speed up joining if region data is
-- binned across columns.
fastBinJoinCols :: Maybe BinWidth -> Bool -> SingleCells -> SingleCells
fastBinJoinCols binWidth joining =
  bool id (L.over colNames (fmap (Feature . j . unFeature)))
    $ isJust binWidth
    where
      j = if joining then T.cons 'B' else T.drop 1

-- | Transform a list of chromosome region features to a list of custom features.
transformChrRegions :: CustomRegions -> SingleCells -> SingleCells
transformChrRegions (CustomRegions customRegions) sc =
  L.set matrix mat . L.set colNames features $ sc
  where
    features = V.fromList . fmap (Feature . showt) $ customRegions
    knownFeatureMap = foldl' (HMap.unionWith IntervalMap.union) HMap.empty
                    . fmap (\ (!i, (!c, !interval))
                           -> HMap.singleton c
                            $ IntervalMap.singleton interval i
                           )
                    . zip ([0..] :: [Int])
                    . fmap unChrRegion
                    $ customRegions
    findErrKnown :: T.Text -> [Int]
    findErrKnown x = fromMaybe []
                   . (\ (!c, !i) -> HMap.lookup c knownFeatureMap
                                >>= Just
                                  . IntervalMap.elems
                                  . flip IntervalMap.intersecting i
                     )
                   . either error unChrRegion
                   . parseChrRegion
                   $ x
    mat = MatObsRow
        . foldl' addToMat init
        . concatMap (\ (!i, !j, !v)
                    -> fmap (i, , v)
                     . findErrKnown
                     . unFeature
                     . fromMaybe (error $ "transformChrRegions/mat: No indices found: " <> show j)
                     $ (V.!?) (L.view colNames sc) j
                    )
        . S.toListSM
        $ oldMat
    init = S.zeroSM (S.nrows oldMat) $ V.length features
    addToMat !m x = m S.^+^ S.fromListSM (S.dim init) [x]
    oldMat = unMatObsRow . L.view matrix $ sc
