{- Load
Gregory W. Schwartz

Collects the functions pertaining to loading the single cell data output from
cellranger.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Load
    ( matToSpMat
    , spMatToMat
    , loadCellrangerData
    , loadHMatrixData
    , loadSparseMatrixData
    , loadSparseMatrixDataStream
    , loadLabelData
    ) where

-- Remote
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Except (runExceptT)
import Data.Char (ord)
import Data.Matrix.MatrixMarket (readMatrix, Matrix(RMatrix, IntMatrix), Structure (..))
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Scientific (toRealFloat, Scientific)
import Data.Vector (Vector)
import Safe
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sparse.Common as HS
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H
import qualified Streaming as S
import qualified Streaming.Cassava as S
import qualified Streaming.Prelude as S

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
matToSpMat :: Matrix Scientific -> HS.SpMatrix Double
matToSpMat (RMatrix size _ _ xs) =
    HS.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, toRealFloat z))
        $ xs
matToSpMat (IntMatrix size _ _ xs) =
    HS.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, fromIntegral z))
        $ xs
matToSpMat _ = error "Input matrix is not a Real matrix."

-- | Convert a sparse matrix to a Matrix.
spMatToMat :: HS.SpMatrix Double -> Matrix Double
spMatToMat mat = RMatrix (HS.dimSM mat) (HS.nzSM mat) General
               . fmap (\(!x, !y, !z) -> (x + 1, y + 1, z)) . HS.toListSM
               $ mat

-- | Load output of cellranger.
loadCellrangerData
    :: Maybe ProjectionFile
    -> GeneFile
    -> CellFile
    -> MatrixFile
    -> IO (SingleCells MatObsRow)
loadCellrangerData pf gf cf mf = do
    let csvOptsTabs = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord '\t') }

    m <- fmap (MatObsRow . HS.transposeSM . matToSpMat)  -- We want observations as rows
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
    let csvOpts = CSV.defaultDecodeOptions
                    { CSV.decDelimiter = fromIntegral (ord delim) }
        strictRead path = (evaluate . force) =<< B.readFile path

    all <- fmap (\x -> either error id $ (CSV.decodeWith csvOpts CSV.NoHeader x :: Either String (V.Vector [T.Text])))
         . strictRead
         . unMatrixFile
         $ mf

    let c = V.fromList . fmap Cell . drop 1 . V.head $ all
        g = fmap (Gene . head) . V.drop 1 $ all
        m = MatObsRow
          . HS.sparsifySM
          . HS.fromColsV -- We want observations as rows
          . fmap (HS.vr . fmap (either error fst . T.double) . drop 1)
          . V.drop 1
          $ all

    p <- maybe (return . projectMatrix $ m) loadProjectionFile pf

    return $
        SingleCells { matrix   = m
                    , rowNames = c
                    , colNames = g
                    , projections = p
                    }

-- | Load a sparse matrix streaming in CSV format (rows were features) with row
-- names and column names.
loadSparseMatrixDataStream :: Delimiter
                           -> Maybe ProjectionFile
                           -> MatrixFile
                           -> IO (SingleCells MatObsRow)
loadSparseMatrixDataStream (Delimiter delim) pf mf = do
    let csvOpts = S.defaultDecodeOptions
                    { S.decDelimiter = fromIntegral (ord delim) }
        cS = S.toList . S.map (fmap Cell . drop 1) . S.take 1
        gS = S.toList . S.map (Gene . head) . S.drop 1
        mS = S.toList
           . S.map (HS.vr . fmap (either error fst . T.double) . drop 1)
           . S.drop 1
           
    (c S.:> _) <- fmap (either (error . show) id)
                . S.runResourceT
                . runExceptT
                . cS
                . S.decode S.NoHeader
                . BS.readFile
                . unMatrixFile
                $ mf

    (g S.:> m S.:> _) <- fmap (either (error . show) id)
                       . S.runResourceT
                       . runExceptT
                       $ gS
                       $ mS
                       $ S.copy
                       . S.decode S.NoHeader
                       . BS.readFile
                       . unMatrixFile
                       $ mf

    let finalMat = MatObsRow . HS.sparsifySM . HS.fromColsL $ m -- We want observations as rows

    p <- maybe (return . projectMatrix $ finalMat) loadProjectionFile pf

    return $
        SingleCells { matrix   = finalMat
                    , rowNames = V.fromList . head $ c
                    , colNames = V.fromList g
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
toPoint :: HS.SpVector Double -> (X, Y)
toPoint = (\[!x, !y] -> (X x, Y y)) . HS.toDenseListSV . HS.takeSV 2

-- | Project a matrix to first two dimensions.
projectMatrix :: MatObsRow -> Vector (X, Y)
projectMatrix = V.fromList . fmap toPoint . HS.toRowsL . unMatObsRow
