{- TooManyCells.MakeTree.ClusterPeaks
Gregory W. Schwartz

Collects functions pertaining to printing cluster fragments and obtaining peaks
per cluster.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Peaks.ClusterPeaks
    ( saveClusterFragments
    , peakCallFiles
    ) where

-- Remote
import BirchBeer.Types (Cluster (..))
import Codec.Compression.GZip (compress, decompress)
import Control.Concurrent.Async.Pool (withTaskGroup, mapTasks)
import Control.Monad (unless, when, void)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Streaming.Zlib (WindowBits (..))
import GHC.Conc (getNumCapabilities)
import Safe (headMay, atMay)
import System.IO.Temp (withSystemTempFile)
import System.Process (callCommand)
import Text.Printf (printf)
import Text.Read (readMaybe)
import TextShow (showt)
import Data.Tuple (swap)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.HashMap.Strict as HMap
import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet as ISet
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Directory as FP
import qualified System.FilePath as FP
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.IO as IO

-- Local
import TooManyCells.File.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Peaks.Types

-- | Get cluster fragment sizes from a split stream.
getClusterTotalMap :: (MonadResource m)
                   => HMap.HashMap T.Text [Int]
                   -> S.Stream (S.Of [T.Text]) m r
                   -> m (IMap.IntMap Double)
getClusterTotalMap cellClusterMap =
  S.fold_ (\acc x -> IMap.insertWith (+) x 1 acc) mempty id
    . S.concat
    . S.mapMaybe getCluster
  where
    getCluster :: [T.Text] -> Maybe [Int]
    getCluster row = atMay row 3 >>= flip HMap.lookup cellClusterMap

-- | Get the genome coverage bedgraph.
toGenomecov :: GenomecovCommand
            -> GenomeFile
            -> IMap.IntMap Double
            -> FilePath
            -> FilePath
            -> IO (Maybe ())
toGenomecov (GenomecovCommand gcc) (GenomeFile gf) clusterTotalMap outputPath file = runMaybeT $ do
  output <- MaybeT
          . return
          . fmap ((FP.</>) outputPath . (<> "_cov.bdg"))
          . headMay
          . splitOn "."
          . FP.takeFileName
          $ file
  wigOut <- MaybeT
          . return
          . fmap ((FP.</>) outputPath . (<> "_cov.bw"))
          . headMay
          . splitOn "."
          . FP.takeFileName
          $ file

  cluster <- MaybeT
           . return
           $ readMaybe
         =<< (flip atMay 1 . splitOn "_" . FP.takeFileName $ file)

  scale <-
    MaybeT . return . fmap (1000000 /) $ IMap.lookup cluster clusterTotalMap

  _ <- MaybeT . fmap Just . callCommand $ printf gcc file gf scale output
  _ <- MaybeT . fmap Just . callCommand $ printf  "sort -k1,1 -k2,2n -o %s %s" output output
  _ <- MaybeT . fmap Just . callCommand $ printf "bedGraphToBigWig %s %s %s" output gf wigOut
  return ()

-- | Write the line of the Fragment or BedGraph file, depending on
-- normalization.
writeLine :: (MonadResource m) => HMap.HashMap T.Text [Int]
                               -> FilePath
                               -> [T.Text]
                               -> m (Maybe ())
writeLine cellClusterMap outputPath all@(chrom:start:end:barcode:duplicateCount:_) = S.liftIO . runMaybeT $ do
  clusters <- MaybeT . return $ HMap.lookup barcode cellClusterMap

  let outputRow = compress
                . B.intercalate "\t"
                $ (fmap (B.fromStrict . T.encodeUtf8) all) <> ["\n"]

  -- Fragment file
  MaybeT
    . fmap Just
    . mapM_ (\ cluster ->
                B.appendFile ( outputPath
                        FP.</> T.unpack ("cluster_" <> showt cluster <> "_fragments.tsv.gz")
                             )
                outputRow
            )
    $ clusters
-- writeLine (Just ctm) cellClusterMap outputPath all@(chrom:start:end:barcode:_:_) = S.liftIO . runMaybeT $ do
--   cluster <- MaybeT . return $ HMap.lookup barcode cellClusterMap
--   clusterSize <- MaybeT . return $ IMap.lookup cluster ctm

--   let val = (1 / clusterSize) * 1000000 -- per million
--       file = outputPath FP.</> T.unpack ("cluster_" <> showt cluster <> "_fragments.bdg")

--   -- Write header if file does not exist.
--   outputPathExists <- MaybeT . fmap Just $ FP.doesFileExist file
--   MaybeT . fmap Just . unless outputPathExists $
--     B.appendFile file
--       . B.fromStrict
--       . T.encodeUtf8
--       $ "track type=bedGraph name=" <> "cluster_" <> showt cluster <> "\n"

--   -- BedGraph file
--   MaybeT
--     . fmap Just
--     . B.appendFile file
--     . B.intercalate "\t"
--     $ (fmap (B.fromStrict . T.encodeUtf8) [chrom, start, end, showt val])
--    <> ["\n"]
writeLine _ _ xs = error $ "Unexpected number of columns, did you use the fragments.tsv.gz 10x format? Input: " <> show xs

-- | Save the fragments per cluster. Loads in the original fragments file to
-- stream out cluster specific fragments.
saveClusterFragments :: OutputDirectory
                     -> BedGraphFlag
                     -> GenomeFile
                     -> GenomecovCommand
                     -> AllNodesFlag
                     -> PeakNodes
                     -> ClusterResults
                     -> Either MatrixFileType MatrixFileType
                     -> IO ()
saveClusterFragments
  output
  (BedGraphFlag bgf)
  genome
  gcc
  (AllNodesFlag anf)
  (PeakNodes pkns)
  cr
  (Left (CompressedFragments (FragmentsFile file))) = do
  -- Where to place output files
  let outputPath = unOutputDirectory output FP.</> "cluster_fragments"

  restartDir outputPath

  let clusterAssocList =
        fmap
            (\ (!cellInfo, !c)
            -> ( unCell $ _barcode cellInfo
               , bool (filter (flip ISet.member pkns)) id (ISet.null pkns)  -- Specific node selection.
               . fmap unCluster
               $ bool (take 1 c) c anf
               )
            )
          . _clusterList
          $ cr
      cellClusterMap :: HMap.HashMap T.Text [Int]
      cellClusterMap = HMap.fromList clusterAssocList
      processedStream = S.map (T.splitOn "\t" . T.decodeUtf8)
                      . S.mapped BS.toStrict
                      . BS.lines
                      . decompressStreamAll (WindowBits 31) -- For gunzip
                      . BS.readFile
                      $ file

  -- Stream in file for fragments.
  runResourceT
    . S.effects
    . S.mapMaybeM (writeLine cellClusterMap outputPath)
    $ processedStream

  -- Stream in file for bedgraph.
  when bgf $ do
    let bedPath = unOutputDirectory output FP.</> "cluster_bedgraphs"

    restartDir bedPath

    clusterTotalMap <-
      runResourceT . getClusterTotalMap cellClusterMap $ processedStream

    files <- fmap (fmap (outputPath FP.</>)) . FP.listDirectory $ outputPath
    cores <- getNumCapabilities
    withTaskGroup cores $ \workers ->
      void . mapTasks workers . fmap (toGenomecov gcc genome clusterTotalMap bedPath) $ files -- mapTasks_ not working
    -- runResourceT
    --   . S.effects
    --   . S.mapMaybeM (writeLine (Just clusterTotalMap) cellClusterMap bedPath)
    --   $ processedStream
saveClusterFragments _ _ _ _ _ _ _ _ = error "Does not supported this matrix type. See too-many-cells -h for each entry point for more information"

-- | Run the specified command on a file. Command should have \"%s\" where the
-- output file and then input file (order is important) should be.
-- For instance: `command --output %s --other --args --input %s`.
peakCallFile :: PeakCommand -> GenomeFile -> FilePath -> FilePath -> IO ()
peakCallFile (PeakCommand c) (GenomeFile gf) outputDir file =
  withSystemTempFile "temp_fragments.tsv" $ \temp h -> do
    let output = (FP.</>) outputDir
               . intercalate ("_")
               . (<> ["peaks"])
               . take 1
               . splitOn "_fragments.tsv.gz"
               . FP.takeFileName
               $ file
        wigIn = output FP.</> (name <> "_treat_pileup.bdg")
        wigOut = output FP.</> (name <> "_treat_pileup.bw")
        name = fromMaybe (error "peakCallFile: Bad file name for fragments.")
             . headMay
             . splitOn "_fragments.tsv.gz"
             . FP.takeFileName
             $ file
    B.readFile file >>= B.hPut h . decompress >> IO.hClose h  -- Decompress file in temporary directory
    _ <- callCommand $ printf c temp name output
    _ <- callCommand $ printf "sort -k1,1 -k2,2n -o %s %s" wigIn wigIn
    -- _ <- callCommand $ printf "bedGraphToBigWig %s %s %s" wigIn gf wigOut
    return ()

-- | Run a specified command on all cluster files asynchronously.
peakCallFiles :: PeakCommand -> GenomeFile -> OutputDirectory -> IO ()
peakCallFiles pc gf (OutputDirectory path) = do
  let output = path FP.</> "cluster_peaks"
      fragmentsPath = path FP.</> "cluster_fragments"

  restartDir output

  files <- fmap (fmap (fragmentsPath FP.</>)) . FP.listDirectory $ fragmentsPath

  cores <- getNumCapabilities
  withTaskGroup cores $ \workers ->
    void . mapTasks workers . fmap (peakCallFile pc gf output) $ files -- mapTasks_ not working

-- | Delete a directory if it exists.
restartDir :: FilePath -> IO ()
restartDir dir = do
  dirExists <- FP.doesDirectoryExist dir
  when dirExists $ FP.removeDirectoryRecursive dir
  FP.createDirectoryIfMissing True dir
  return ()
