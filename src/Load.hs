{- Load
Gregory W. Schwartz

Collects the functions pertaining to loading the single cell data output from
cellranger.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Load
    ( loadCellrangerData
    , loadHMatrixData
    , loadSparseMatrixData
    , loadLabelData
    ) where

-- Remote
import Data.Char (ord)
import Data.Matrix.MatrixMarket (readMatrix, Matrix(RMatrix, IntMatrix))
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Scientific (toRealFloat, Scientific)
import Data.Vector (Vector)
import Safe
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Map as Map
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H

-- Local
import Types
import Utility

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

-- | Load output of cellranger.
loadCellrangerData
    :: Maybe ProjectionFile
    -> GeneFile
    -> CellFile
    -> MatrixFile
    -> IO (SingleCells MatObsRow)
loadCellrangerData pf gf cf mf = do
    let csvOptsTabs = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord '\t') }

    m <- fmap (MatObsRow . S.transposeSM . matToSpMat)  -- We want observations as rows
       . readMatrix
       . unMatrixFile
       $ mf
    g <- fmap (\ x -> either error (fmap (Gene . fst)) ( CSV.decodeWith csvOptsTabs CSV.NoHeader x
                                       :: Either String (Vector (T.Text, T.Text))
                                        )
              )
       . B.readFile
       . unGeneFile
       $ gf
    c <- fmap (\ x -> either error (fmap (Cell . head)) ( CSV.decodeWith csvOptsTabs CSV.NoHeader x
                                       :: Either String (Vector [T.Text])
                                        )
              )
       . B.readFile
       . unCellFile
       $ cf
    p <- maybe (return . projectMatrix $ m) loadProjectionFile $ pf

    return $
        SingleCells { matrix   = m -- We want observations as rows.
                    , rowNames = c
                    , colNames = g
                    , projections = p
                    }

-- | Load an H Matrix in CSV format (rows were features) with row names and
-- column names.
loadHMatrixData :: Delimiter
                -> Maybe ProjectionFile
                -> MatrixFile
                -> IO (SingleCells MatObsRow)
loadHMatrixData (Delimiter delim) pf mf = do
    let csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord delim) }

    all <- fmap (\ x -> either error id ( CSV.decodeWith csvOpts CSV.NoHeader x
                                       :: Either String (Vector (Vector T.Text))
                                        )
                )
         . B.readFile
         . unMatrixFile
         $ mf

    let c = fmap Cell . V.drop 1 . V.head $ all
        g = fmap (Gene . V.head) . V.drop 1 $ all
        m = MatObsRow
          . hToSparseMat
          . H.tr -- We want observations as rows
          . H.fromLists
          . fmap (fmap (either error fst . T.double) . drop 1 . V.toList)
          . drop 1
          . V.toList
          $ all

    p <- maybe (return . projectMatrix $ m) loadProjectionFile $ pf

    return $
        SingleCells { matrix   = m
                    , rowNames = c
                    , colNames = g
                    , projections = p
                    }

-- | Load a sparse matrix in CSV format (rows were features) with row names and
-- column names.
loadSparseMatrixData :: Delimiter
                     -> Maybe ProjectionFile
                     -> MatrixFile
                     -> IO (SingleCells MatObsRow)
loadSparseMatrixData (Delimiter delim) pf mf = do
    let csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord delim) }

    all <- fmap (\ x -> either error id ( CSV.decodeWith csvOpts CSV.NoHeader x
                                       :: Either String (Vector (Vector T.Text))
                                        )
                )
         . B.readFile
         . unMatrixFile
         $ mf

    let c = fmap Cell . V.drop 1 . V.head $ all
        g = fmap (Gene . V.head) . V.drop 1 $ all
        nrows = V.length all - 1
        ncols = V.length (all V.! 1) - 1
        m = MatObsRow
          . S.sparsifySM
          . S.fromColsV -- We want observations as rows
          . fmap (S.vr . fmap (either error fst . T.double) . drop 1 . V.toList)
          . V.drop 1
          $ all

    p <- maybe (return . projectMatrix $ m) loadProjectionFile pf

    return $
        SingleCells { matrix   = m
                    , rowNames = c
                    , colNames = g
                    , projections = p
                    }

-- | Load a CSV containing the label of each cell.
loadLabelData :: Delimiter -> LabelFile -> IO LabelMap
loadLabelData (Delimiter delim) (LabelFile file) = do
    let csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord delim) }

    rows <- fmap (\ x -> either error snd ( CSV.decodeByNameWith csvOpts x
                                        :: Either String (CSV.Header, Vector (Map.Map T.Text T.Text))
                                         )
                 )
          . B.readFile
          $ file

    let toLabelMap :: Map.Map T.Text T.Text -> Map.Map Cell Label
        toLabelMap m =
            Map.singleton
                (Cell $ Map.findWithDefault (error "No \"cell\" column in label file.") "cell" m)
                (Label $ Map.findWithDefault (error "No \"label\" column in label file.") "label" m)

    return . LabelMap . Map.unions . fmap toLabelMap . V.toList $ rows

-- | Load a projection file to get a vector of projections.
loadProjectionFile :: ProjectionFile -> IO (Vector (X, Y))
loadProjectionFile =
    fmap (\ x -> either
                                error
                                (fmap getProjection . V.drop 1)
                                (CSV.decode CSV.NoHeader x :: Either String (Vector [T.Text]))
         )
        . B.readFile
        . unProjectionFile
  where
    getProjection [_, x, y] = ( X . either error fst . T.double $ x
                              , Y . either error fst . T.double $ y
                              )
    getProjection xs        =
        error $ "Unrecognized projection row: " <> show xs

-- | Decide to use first two values as the projection.
toPoint :: S.SpVector Double -> (X, Y)
toPoint = (\[!x, !y] -> (X x, Y y)) . S.toDenseListSV . S.takeSV 2

-- | Project a matrix to first two dimensions.
projectMatrix :: MatObsRow -> Vector (X, Y)
projectMatrix = V.fromList . fmap toPoint . S.toRowsL . unMatObsRow
