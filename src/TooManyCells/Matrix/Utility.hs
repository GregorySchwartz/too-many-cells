{- TooManyCells.Matrix.Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE OverloadedStrings #-}
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
    , loadMatrixMarket
    , extractSc
    , writeMatrixLike
    , isCsvFile
    , getMatrixOutputType
    , matrixValidity
    , hashNub
    , decompressStreamAll
    , transposeSc
    , extractCellSc
    , extractCellsSc
    , removeCellsSc
    , extractCellV
    , aggSc
    ) where

-- Remote
import BirchBeer.Types
import Codec.Compression.GZip (compress)
import Control.Monad.Managed (runManaged)
import Control.Monad.State (MonadState (..), State (..), evalState, execState, modify)
import Control.Monad.IO.Class (MonadIO)
import Data.Bool (bool)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (maximumBy, isInfixOf, foldl')
import Data.Maybe (fromMaybe)
import Data.Matrix.MatrixMarket (Matrix(RMatrix, IntMatrix), Structure (..), writeMatrix')
import Data.Scientific (toRealFloat, Scientific)
import Data.Streaming.Zlib (WindowBits)
import Language.R as R
import Language.R.QQ (r)
import System.FilePath ((</>))
import TextShow (showt)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.HashSet as HSet
import qualified Data.IntMap.Strict as IMap
import qualified Data.IntervalMap.Strict as IntervalMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H
import qualified Streaming as Stream
import qualified Streaming.Cassava as Stream
import qualified Streaming.Prelude as Stream
import qualified Streaming.With.Lifted as SW
import qualified Streaming.Zip as S
import qualified System.Directory as FP

-- Local
import TooManyCells.File.Types
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
        colNamesR = fmap (T.unpack . unFeature) . V.toList . _colNames $ sc

    mat <- fmap unRMatObsRow . matToRMat . _matrix $ sc

    -- namedMat <- [r| rownames(mat_hs) = rowNamesR_hs
    --                 colnames(mat_hs) = colNamesR_hs
    --             |]

    return . RMatObsRow $ mat

-- | Convert a sparse matrix to an hmatrix.
sparseToHMat :: S.SpMatrix Double -> H.Matrix H.R
sparseToHMat mat = H.assoc (S.dimSM mat) 0
                 . fmap (\(!x, !y, !z) -> ((x, y), z))
                 . S.toListSM
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
matToHMat _ = error "\nInput matrix is not a Real matrix."

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
matToSpMat _ = error "\nInput matrix is not a Real matrix."

-- | Convert a sparse matrix to a Matrix.
spMatToMat :: S.SpMatrix Double -> Matrix Double
spMatToMat mat = RMatrix (S.dimSM mat) (S.nzSM mat) General
               . fmap (\(!x, !y, !z) -> (x + 1, y + 1, z)) . S.toListSM
               $ mat

-- | Load a matrix market format.
loadMatrixMarket :: MatrixFileFolder -> IO (S.SpMatrix Double)
loadMatrixMarket (MatrixFile file) = do
    let toDouble [r, c, v] =
            ( either error fst . TL.decimal $ r
            , either error fst . TL.decimal $ c
            , either error fst . TL.double $ v
            )
        toDouble _ = error "Matrix market contains non-standard coordinate."

    (rows, cols, _):cs <- fmap
                            ( fmap (toDouble . TL.words)
                            . dropWhile ((== '%') . TL.head)
                            . TL.lines
                            )
                        . TL.readFile
                        $ file

    return
        . S.fromListSM (rows, cols)
        . fmap (\(!r, !c, !v) -> (r - 1, c - 1, v))
        $ cs

-- | Determine presence of matrix.
extractSc :: Maybe SingleCells -> SingleCells
extractSc = fromMaybe (error "Need to provide matrix in --matrix-path for this functionality.")

-- | Write a matrix to a file.
writeSparseMatrixLike :: MatrixLike a => MatrixTranspose -> MatrixFileFolder -> a -> IO ()
writeSparseMatrixLike (MatrixTranspose mt) (MatrixFolder folder) mat = do
  -- Where to place output files.
  FP.createDirectoryIfMissing True folder

  let writeCompressedFile x = BL.writeFile x . compress

  (=<<) (writeCompressedFile (folder </> "matrix.mtx.gz"))
    . writeMatrix'
    . spMatToMat
    . (bool S.transposeSM id mt) -- whether to transpose
    . getMatrix
    $ mat
  writeCompressedFile (folder </> "features.tsv.gz")
    . TL.encodeUtf8
    . TL.fromStrict
    . T.unlines
    . fmap (\x -> T.intercalate "\t" [x, x, "NA"])
    . V.toList
    . getColNames
    $ mat
  writeCompressedFile (folder </> "barcodes.tsv.gz")
    . TL.encodeUtf8
    . TL.fromStrict
    . T.unlines
    . V.toList
    . getRowNames
    $ mat

  return ()

-- | Print a dense matrix to a streaming string.
printDenseMatrixLike :: (MatrixLike a, Monad m)
                     => MatrixTranspose
                     -> a
                     -> BS.ByteString m ()
printDenseMatrixLike (MatrixTranspose mt) mat =
  Stream.encode (Just $ Stream.header header)
    . Stream.zipWith (:) rowN
    . Stream.each
    . fmap (fmap showt . S.toDenseListSV)
    . S.toRowsL
    . (bool S.transposeSM id mt) -- To have cells as columns
    . getMatrix
    $ mat
  where
    header = (B.empty :)
           . fmap T.encodeUtf8
           . V.toList
           . getRowNames -- To have rows as columns
           $ mat
    rowN = Stream.each . V.toList . getColNames $ mat -- To have columns as rows

-- | Write a matrix to a dense file.
writeDenseMatrixLike :: (MatrixLike a)
                     => MatrixTranspose
                     -> MatrixFileFolder
                     -> a
                     -> IO ()
writeDenseMatrixLike mt (MatrixFile file) =
  runManaged . SW.writeBinaryFile file . printDenseMatrixLike mt

-- | Write a MatrixLike to a file (dense) or folder (sparse).
writeMatrixLike :: (MatrixLike a)
                => MatrixTranspose
                -> MatrixFileFolder
                -> a
                -> IO ()
writeMatrixLike mt o@(MatrixFolder _) = writeSparseMatrixLike mt o
writeMatrixLike mt o@(MatrixFile _) = writeDenseMatrixLike mt o

-- | Check if a name ends with .csv
isCsvFile :: FilePath -> Bool
isCsvFile = (== ".CSV") . fmap toUpper . reverse . take 4 . reverse

-- | Get matrix output format from input name.
getMatrixOutputType :: FilePath -> MatrixFileFolder
getMatrixOutputType x = bool (MatrixFolder x) (MatrixFile x) . isCsvFile $ x

-- | Check validity of matrix.
matrixValidity :: (MatrixLike a) => a -> Maybe String
matrixValidity mat
  | rows /= numCells || cols /= numFeatures =
      Just $ "Warning: mismatch in number of (features, cells) ("
                           <> show numFeatures
                           <> ","
                           <> show numCells
                           <> ") with matrix (rows, columns) ("
                           <> show cols
                           <> ","
                           <> show rows
                           <> "), will probably result in error."
  | otherwise = Nothing
  where
    (rows, cols) = S.dimSM . getMatrix $ mat
    numCells = V.length . getRowNames $ mat
    numFeatures = V.length . getColNames $ mat

-- | Same as Control.Foldl.nub but using HashSet.
hashNub :: (Hashable a, Eq a) => Fold.Fold a [a]
hashNub = Fold.Fold step (HSet.empty, id) fin
  where
    step (!s, !r) a =
      if HSet.member a s
        then (s, r)
        else (HSet.insert a s, r . (a :))
    fin (_, !r) = r []
{-# INLINABLE hashNub #-}

-- | Keep decompressing a compressed bytestream until exhaused.
decompressStreamAll :: (MonadIO m) => WindowBits -> BS.ByteString m r -> BS.ByteString m r
decompressStreamAll w bs = S.decompress' w bs >>= go
  where
    go (Left bs) = S.decompress' w bs >>= go
    go (Right r) = return r
{-# INLINABLE decompressStreamAll #-}

-- | Transpose a SingleCells type.
transposeSc :: SingleCells -> SingleCells
transposeSc (SingleCells (MatObsRow mat) rows cols) =
  SingleCells
    (MatObsRow . S.transposeSM $ mat)
    (fmap (Cell . unFeature) cols)
    (fmap (Feature . unCell) rows)

-- | Get a single cell from a SingleCells type.
extractCellSc :: Cell -> SingleCells -> SingleCells
extractCellSc cell sc = L.set rowNames (V.singleton cell)
                    . L.set matrix newMatrix
                    $ sc
  where
    newMatrix = MatObsRow
              . S.fromRowsL
              . (:[])
              . flip S.extractRow (getCellIdx sc cell)
              . unMatObsRow
              . L.view matrix
              $ sc

-- | Get a list of single cells for each single cell from a SingleCells type.
extractCellsSc :: SingleCells -> [SingleCells]
extractCellsSc sc = zipWith
                      (\ v c -> sc { _rowNames = V.singleton c
                                   , _matrix = MatObsRow $ S.fromRowsL [v]
                                   }
                      )
                      (S.toRowsL . unMatObsRow . L.view matrix $ sc)
                  . V.toList
                  . L.view rowNames
                  $ sc

-- | Get a single cell vector from a SingleCells type.
extractCellV :: Cell -> SingleCells -> S.SpVector Double
extractCellV cell sc =
  flip S.extractRow (getCellIdx sc cell) . unMatObsRow . L.view matrix $ sc

-- | Remove single cells from a SingleCells type.
removeCellsSc :: [Cell] -> SingleCells -> SingleCells
removeCellsSc cells sc = L.set rowNames newRowNames
                     . L.set matrix newMatrix
                     $ sc
  where
    blacklistCells = Set.fromList cells
    blacklistIdxs = Set.fromList . fmap (getCellIdx sc) $ cells
    whitelistIdxs = filter
                      (not . flip Set.member blacklistIdxs)
                      [0..(V.length $ L.view rowNames sc) - 1]
    newRowNames =
      V.filter (not . flip Set.member blacklistCells) . L.view rowNames $ sc
    newMatrix = MatObsRow
              . S.fromRowsL
              . fmap (\x -> flip S.extractRow x . unMatObsRow . L.view matrix $ sc)
              $ whitelistIdxs

-- | Get the index of a cell in a SingleCells type.
getCellIdx :: SingleCells -> Cell -> Int
getCellIdx sc x =
  fromMaybe (error $ "extractCellIdx: cell not found in matrix: " <> show x)
    . V.findIndex (== x)
    . L.view rowNames
    $ sc

-- | Aggregate SingleCells using average.
aggSc :: SingleCells -> AggSingleCells
aggSc sc = AggSingleCells
         . L.over matrix (MatObsRow . avgMat . unMatObsRow)
         . L.over rowNames ( fmap (Cell . (<> " Aggregate") . unCell)
                           . V.take 1
                           )
         $ sc
  where
    avgMat = S.fromListDenseSM numRows
           . fmap ((/ fromIntegral numCols) . foldl' (+) 0)
           . S.toRowsL
    (numRows, numCols) = S.dim . unMatObsRow . L.view matrix $ sc
