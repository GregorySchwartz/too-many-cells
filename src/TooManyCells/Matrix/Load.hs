{- TooManyCells.Matrix.Load
Gregory W. Schwartz

Collects the functions pertaining to loading the single cell data output from
cellranger.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Matrix.Load
    ( matToSpMat
    , spMatToMat
    , loadCellrangerData
    , loadHMatrixData
    , loadSparseMatrixData
    , loadSparseMatrixDataStream
    ) where

-- Remote
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Except (runExceptT, ExceptT (..))
import Control.Monad.Managed (with, liftIO, Managed (..))
import Data.Char (ord)
import Data.Matrix.MatrixMarket (readMatrix)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
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
import qualified Streaming.With.Lifted as SW

-- Local
import TooManyCells.File.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility

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
        cS = fmap (S.first (fmap Cell . drop 1 . fromMaybe (error "No header.")))
           . S.head
        gS = S.toList . S.map (Gene . head) . S.drop 1
        mS = S.toList
           . S.map (HS.sparsifySV . HS.vr . fmap (either error fst . T.double) . drop 1)
           . S.drop 1

    res <- flip with return $ do

        contents <- SW.withBinaryFileContents . unMatrixFile $ mf

        (c S.:> g S.:> m S.:> _) <-
            fmap (either (error . show) id)
                . runExceptT
                . cS
                . (S.store gS)
                . (S.store mS)
                . S.decode S.NoHeader
                $ (contents :: BS.ByteString (ExceptT S.CsvParseException Managed) ())

        let finalMat = MatObsRow . HS.sparsifySM . HS.fromColsL $ m -- We want observations as rows

        p <- liftIO
           $ maybe (return . projectMatrix $ finalMat) loadProjectionFile pf

        return $
            SingleCells { matrix   = finalMat
                        , rowNames = V.fromList c
                        , colNames = V.fromList g
                        , projections = p
                        }

    return res

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
