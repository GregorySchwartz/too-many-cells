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
    , loadCellrangerDataFeatures
    , loadHMatrixData
    , loadSparseMatrixData
    , loadSparseMatrixDataStream
    , loadFragments
    , loadProjectionMap
    , loadBdgBW
    ) where

-- Remote
import BirchBeer.Types
import Codec.Compression.GZip (decompress)
import Control.DeepSeq (force, deepseq)
import Control.Exception (evaluate)
import Control.Monad.Except (runExceptT, ExceptT (..))
import Control.Monad.Managed (with, liftIO, Managed (..))
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Char (ord)
import Data.Function (on)
import Data.List (sortBy, sort, foldl')
import Data.Matrix.MatrixMarket (readMatrix, readMatrix')
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Streaming.Zlib (WindowBits (..))
import Data.Vector (Vector)
import Safe (atMay, headMay)
import System.IO.Temp (withSystemTempFile)
import TextShow (showt)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.IntMap.Strict as IMap
import qualified Data.IntervalMap.Interval as I
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sparse.Common as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H
import qualified Streaming as S
import qualified Streaming.Cassava as S
import qualified Streaming.Prelude as S
import qualified Streaming.With.Lifted as S
import qualified Streaming.Zip as S
import qualified System.IO as IO
import qualified Turtle as Turtle
import qualified Turtle.Bytes as TB
import qualified Turtle.Line as Turtle

-- Local
import TooManyCells.File.Types
import TooManyCells.Matrix.AtacSeq
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility

-- | Get feature from feature file row with informative error.
getFeature :: FeatureColumn -> [a] -> a
getFeature (FeatureColumn fc) =
  fromMaybe (error "Feature column selected not present in feature file (too few columns in feature file.)")
    . flip atMay (fc - 1) -- Convert to 0-index.

-- | Load output of cellranger.
loadCellrangerData
    :: FeatureColumn
    -> FeatureFile
    -> CellFile
    -> MatrixFileFolder
    -> IO SingleCells
loadCellrangerData _ _ _ (MatrixFolder mf) = error "Expected matrix.mtx, impossible error."
loadCellrangerData fc gf cf (MatrixFile mf) = do
    let csvOptsTabs = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord '\t') }

    m <- fmap (MatObsRow . HS.transposeSM . matToSpMat)  -- We want observations as rows
       . readMatrix
       $ mf
    g <- fmap (\ x -> either error (fmap (Feature . getFeature fc)) ( CSV.decodeWith csvOptsTabs CSV.NoHeader x
                                       :: Either String (Vector [T.Text])
                                        )
              )
       . BL.readFile
       . unFeatureFile
       $ gf
    c <- fmap (\ x -> either error (fmap (Cell . head)) ( CSV.decodeWith csvOptsTabs CSV.NoHeader x
                                       :: Either String (Vector [T.Text])
                                        )
              )
       . BL.readFile
       . unCellFile
       $ cf

    return $
        SingleCells { _matrix   = MatObsRow . HS.sparsifySM . unMatObsRow $ m -- We want observations as rows.
                    , _rowNames = c
                    , _colNames = g
                    }

-- | Load output of cellranger >= 3.0.0
loadCellrangerDataFeatures
    :: FeatureColumn
    -> FeatureFile
    -> CellFile
    -> MatrixFileFolder
    -> IO SingleCells
loadCellrangerDataFeatures _ _ _ (MatrixFolder mf) = error "Expected matrix.mtx.gz, impossible error."
loadCellrangerDataFeatures fc gf cf (MatrixFile mf) = do
    let csvOptsTabs = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord '\t') }

    m <- BL.readFile mf
     >>= readMatrix' . decompress
     >>= pure . MatObsRow . HS.transposeSM . matToSpMat  -- We want observations as rows
    g <- fmap (\ x -> either error (fmap (Feature . getFeature fc)) ( CSV.decodeWith csvOptsTabs CSV.NoHeader (decompress x)
                                       :: Either String (Vector [T.Text])
                                        )
              )
       . BL.readFile
       . unFeatureFile
       $ gf
    c <- fmap (\ x -> either error (fmap (Cell . head)) ( CSV.decodeWith csvOptsTabs CSV.NoHeader (decompress x)
                                       :: Either String (Vector [T.Text])
                                        )
              )
       . BL.readFile
       . unCellFile
       $ cf

    return $
        SingleCells { _matrix   = MatObsRow . HS.sparsifySM . unMatObsRow $ m -- We want observations as rows.
                    , _rowNames = c
                    , _colNames = g
                    }

-- | Load an H Matrix in CSV format (rows were features) with row names and
-- column names.
loadHMatrixData :: Delimiter
                -> MatrixFileFolder
                -> IO SingleCells
loadHMatrixData _ (MatrixFolder mf) = error "Expected matrix.mtx, impossible error."
loadHMatrixData (Delimiter delim) (MatrixFile mf) = do
    let csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord delim) }

    all <- fmap (\ x -> either error id ( CSV.decodeWith csvOpts CSV.NoHeader x
                                       :: Either String (Vector (Vector T.Text))
                                        )
                )
         . BL.readFile
         $ mf

    let c = fmap Cell . V.drop 1 . V.head $ all
        g = fmap (Feature . V.head) . V.drop 1 $ all
        m = MatObsRow
          . hToSparseMat
          . H.tr -- We want observations as rows
          . H.fromLists
          . fmap (fmap (either error fst . T.double) . drop 1 . V.toList)
          . drop 1
          . V.toList
          $ all

    return $
        SingleCells { _matrix   = MatObsRow . HS.sparsifySM . unMatObsRow $ m
                    , _rowNames = c
                    , _colNames = g
                    }

-- | Load a sparse matrix in CSV format (rows were features) with row names and
-- column names.
loadSparseMatrixData :: Delimiter
                     -> MatrixFileFolder
                     -> IO SingleCells
loadSparseMatrixData _ (MatrixFolder mf) = error $ mf <> " must be a csv for dense format."
loadSparseMatrixData (Delimiter delim) (MatrixFile mf) = do
    let csvOpts = CSV.defaultDecodeOptions
                    { CSV.decDelimiter = fromIntegral (ord delim) }
        strictRead path = (evaluate . force) =<< BL.readFile path

    all <- fmap (\x -> either error id $ (CSV.decodeWith csvOpts CSV.NoHeader x :: Either String (V.Vector [T.Text])))
         . strictRead
         $ mf

    let c = V.fromList . fmap Cell . drop 1 . V.head $ all
        g = fmap (Feature . head) . V.drop 1 $ all
        m = MatObsRow
          . HS.sparsifySM
          . HS.fromColsV -- We want observations as rows
          . fmap (HS.vr . fmap (either error fst . T.double) . drop 1)
          . V.drop 1
          $ all

    return $
        SingleCells { _matrix   = MatObsRow . HS.sparsifySM . unMatObsRow $ m
                    , _rowNames = c
                    , _colNames = g
                    }

-- | Load a sparse matrix streaming in CSV format (rows were features) with row
-- names and column names.
loadSparseMatrixDataStream :: Delimiter
                           -> MatrixFileFolder
                           -> IO SingleCells
loadSparseMatrixDataStream _ (MatrixFolder mf) = error $ mf <> " must be a csv for dense format."
loadSparseMatrixDataStream (Delimiter delim) (MatrixFile mf) = do
    let csvOpts = S.defaultDecodeOptions
                    { S.decDelimiter = fromIntegral (ord delim) }
        cS = fmap (S.first (fmap Cell . drop 1 . fromMaybe (error "\nNo header.")))
           . S.head
        gS = S.toList . S.map (Feature . head) . S.drop 1
        mS = S.toList
           . S.map (HS.sparsifySV . HS.vr . fmap (either error fst . T.double) . drop 1)
           . S.drop 1

    res <- flip with return $ do

        contents <- S.withBinaryFileContents mf

        (c S.:> g S.:> m S.:> _) <-
            fmap (either (\x -> error $ show x <> " Expecting csv format. Is this the correct file name and file format?") id)
                . runExceptT
                . cS
                . (S.store gS)
                . (S.store mS)
                . S.decode S.NoHeader
                $ (contents :: BS.ByteString (ExceptT S.CsvParseException Managed) ())

        let finalMat = MatObsRow . HS.sparsifySM . HS.fromColsL $ m -- We want observations as rows

        return $
            SingleCells { _matrix   = MatObsRow . HS.sparsifySM . unMatObsRow $ finalMat
                        , _rowNames = V.fromList c
                        , _colNames = V.fromList g
                        }

    return res

-- | Load a range feature list streaming in TSV 10x fragments.tsv.gz format.
loadFragments :: Maybe CellWhitelist
              -> Maybe BlacklistRegions
              -> Maybe ExcludeFragments
              -> Maybe BinWidth
              -> FragmentsFile
              -> IO SingleCells
loadFragments whitelist blacklist excludeFragments binWidth (FragmentsFile mf) = do
  let readDecimal = either error fst . T.decimal
      readDouble = either error fst . T.double
      labelsFold = (,) <$> cellsFold <*> featuresFold
      cellsFold = Fold.premap (\(!c, _, _) -> c) hashNub
      featuresFold = Fold.premap (\(_, !r, _) -> r) hashNub
      preprocessStream = maybe id filterCells whitelist
                       . Turtle.mfilter (\(_, _, !x) -> x /= 0)  -- Ignore missing regions
                       . fmap parseLine
      filterCells (CellWhitelist wl) =
        Turtle.mfilter (\(!b, _, _) -> HSet.member b wl)
      filterFragments (ExcludeFragments ef) = Turtle.mfilter (T.isInfixOf ef)
      filterBlacklist (BlacklistRegions br) =
        Turtle.inproc "bedtools" ["subtract", "-A", "-a", "stdin", "-b", br]
      parseLine (chrom:start:end:barcode:duplicateCount:_) =
        ( barcode
        , maybe showt (\x -> showt . unChrRegionBin . rangeToBin x) binWidth
        . either (\x -> error $ "Cannot read region in file: " <> x) id
        . parseChrRegion
        $ mconcat [chrom, ":", start, "-", end]
        , readDouble duplicateCount
        )
      parseLine xs = error $ "loadFragments: Unexpected number of columns, did you use the fragments.tsv.gz 10x format? Input: " <> show xs
      stream = preprocessStream
             . fmap (T.splitOn "\t")
             . (\x -> maybe x (flip filterFragments x) excludeFragments)  -- Filter out unwanted fragments by name match
             . Turtle.mfilter (not . T.null)
             . fmap Turtle.lineToText
             . (maybe id filterBlacklist blacklist)
             . Turtle.inproc "bedtools" ["sort", "-i", "stdin"]
             . Turtle.toLines
             . TB.toUTF8
             . TB.decompress (WindowBits 31)
             . TB.input
             . Turtle.fromText
             . T.pack
             $ mf

  (cellsList, featuresList) <- Turtle.reduce labelsFold stream

  let cells = V.fromList cellsList
      features = V.fromList featuresList
      cellMap = HMap.fromList . flip zip ([0..] :: [Int]) $ cellsList
      featureMap = HMap.fromList . flip zip ([0..] :: [Int]) $ featuresList
      findErr x = fromMaybe (error $ "loadFragments: No indices found: " <> show x)
                . HMap.lookup x
      matFold = Fold.Fold addToMat init MatObsRow
      addToMat !m val = m HS.^+^ HS.fromListSM (HS.dimSM init) [val]
      init = HS.zeroSM (V.length cells) (V.length features)
      getIndices (!c, !r, !v) =
        (findErr c cellMap, findErr r featureMap, v)

  -- Get map second.
  mat <- Turtle.reduce matFold
       . fmap getIndices
       $ stream

  return $
      SingleCells { _matrix   = MatObsRow . HS.sparsifySM . unMatObsRow $ mat
                  , _rowNames = fmap Cell cells
                  , _colNames = fmap Feature features
                  }

-- | Load a bedGraph or bigWig file as a reference to classify cells.
loadBdgBW :: Maybe BlacklistRegions
          -> Maybe ExcludeFragments
          -> Maybe BinWidth
          -> MatrixFileType
          -> IO SingleCells
loadBdgBW blacklist excludeFragments binWidth inFile = do
  let readDecimal = either error fst . T.decimal
      readDouble = either error fst . T.double
      featuresFold = Fold.premap (\(!r, _) -> r) hashNub
      preprocessStream = Turtle.mfilter (\(_, !x) -> x /= 0)  -- Ignore missing regions
                       . fmap parseLine
      filterFragments (ExcludeFragments ef) = Turtle.mfilter (T.isInfixOf ef)
      filterBlacklist (BlacklistRegions br) =
        Turtle.inproc "bedtools" ["subtract", "-A", "-a", "stdin", "-b", br]
      parseLine (chrom:start:end:duplicateCount:_) =
        ( maybe showt (\x -> showt . unChrRegionBin . rangeToBin x) binWidth
        . either (\x -> error $ "Cannot read region in file: " <> x) id
        . parseChrRegion
        $ mconcat [chrom, ":", start, "-", end]
        , readDouble duplicateCount
        )
      parseLine xs = error $ "loadBW: Unexpected number of columns, did you use the bigWig format? Input: " <> show xs
      fromFile (BigWig file) =
        Turtle.inproc "bigWigToBedGraph" [T.pack file, "stdout"] mempty
      fromFile (BedGraph file) = Turtle.input . Turtle.fromText . T.pack $ file
      fromFile file =
        error $ "Only accepts BedGraph or BigWig for now: " <> show file
      getFileName (BigWig file) = file
      getFileName (BedGraph file) = file
      getFileName file =
        error $ "Only accepts BedGraph or BigWig for now: " <> show file
      stream = preprocessStream
             . fmap (T.splitOn "\t")
             . (\x -> maybe x (flip filterFragments x) excludeFragments)  -- Filter out unwanted fragments by name match
             . Turtle.mfilter (not . T.null)
             . fmap Turtle.lineToText
             . (maybe id filterBlacklist blacklist)
             . Turtle.inproc "bedtools" ["sort", "-i", "stdin"]
             $ fromFile inFile

  featuresList <- Turtle.reduce featuresFold stream

  let features = V.fromList featuresList
      featureMap = HMap.fromList . flip zip ([0..] :: [Int]) $ featuresList
      findErr x = fromMaybe (error $ "loadBW: No indices found: " <> show x)
                . HMap.lookup x
      matFold = Fold.Fold addToMat init MatObsRow
      addToMat !m (!i, !j, !x) = HS.insertSpMatrix i j x m
      init = HS.zeroSM 1 (V.length features)
      getIndices (!r, !v) =
        (0, findErr r featureMap, v)

  -- Get map second.
  mat <- Turtle.reduce matFold
       . fmap getIndices
       $ stream

  return $
      SingleCells { _matrix   = MatObsRow . HS.sparsifySM . unMatObsRow $ mat
                  , _rowNames = V.singleton . Cell . T.pack $ getFileName inFile
                  , _colNames = fmap Feature features
                  }

-- | Load a projection file to get a vector of projections.
loadProjectionFile :: ProjectionFile -> IO (Vector (X, Y))
loadProjectionFile =
    fmap (\ x -> either
                                error
                                (fmap getProjection . V.drop 1)
                                (CSV.decode CSV.NoHeader x :: Either String (Vector [T.Text]))
         )
        . BL.readFile
        . unProjectionFile
  where
    getProjection [_, x, y] = ( X . either error fst . T.double $ x
                              , Y . either error fst . T.double $ y
                              )
    getProjection xs        =
        error $ "\nUnrecognized projection row: " <> show xs

-- | Load a projection file as a map.
loadProjectionMap :: ProjectionFile -> IO ProjectionMap
loadProjectionMap =
    fmap (\ x -> ProjectionMap
               . Map.fromList
               . V.toList
               . either
                  error
                  (fmap getProjection . V.drop 1)
               $ (CSV.decode CSV.NoHeader x :: Either String (Vector [T.Text]))
         )
        . BL.readFile
        . unProjectionFile
  where
    getProjection [b, x, y] = ( Cell b
                              , ( X . either error fst . T.double $ x
                                , Y . either error fst . T.double $ y
                                )
                              )
    getProjection xs        =
        error $ "\nUnrecognized projection row: " <> show xs

-- | Decide to use first two values as the projection.
toPoint :: HS.SpVector Double -> (X, Y)
toPoint = (\[!x, !y] -> (X x, Y y)) . HS.toDenseListSV . HS.takeSV 2

-- | Project a matrix to first two dimensions.
projectMatrix :: MatObsRow -> Vector (X, Y)
projectMatrix = V.fromList . fmap toPoint . HS.toRowsL . unMatObsRow
