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
import BirchBeer.Types (Cluster (..), Label, LabelMap (..), Id (..))
import Codec.Compression.GZip (compress, decompress)
import Control.Concurrent.Async.Pool (withTaskGroup, mapTasks)
import Control.Monad (unless, when, void)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromMaybe, isNothing, isJust, fromJust)
import Data.Streaming.Zlib (WindowBits (..))
import Data.Tuple (swap)
import GHC.Conc (getNumCapabilities)
import Safe (headMay, atMay)
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Process (callCommand)
import Text.Read (readMaybe)
import TextShow (showt)
import Turtle
import Turtle.Line
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.HashMap.Strict as HMap
import qualified Data.IntMap.Strict as IMap
import qualified Data.IntSet as ISet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Printf as TP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.Directory as FP
import qualified System.FilePath as FP
import qualified Filesystem.Path as FSP
import qualified System.IO as IO
import qualified Turtle.Bytes as TB

-- Local
import TooManyCells.File.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Peaks.Types

-- | External sort fragment file.
externalFragmentsSort :: IO.FilePath -> IO ()
externalFragmentsSort file =
  void . callCommand $ TP.printf  "sort -k1,1 -k2,2n -o %s %s" file file

-- | External sort compressed fragment file.
externalCompressedFragmentsSort :: IO.FilePath -> IO ()
externalCompressedFragmentsSort file = do
  void . callCommand $ TP.printf  "gzip -d %s" file
  void
    . callCommand
    $ TP.printf  "cat %s | sort -k1,1 -k2,2n | gzip -c > %s" (FP.dropExtension file) file
  void . callCommand $ TP.printf  "rm %s" (FP.dropExtension file)

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
            -> IO.FilePath
            -> IO.FilePath
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

  _ <- MaybeT . fmap Just $ externalCompressedFragmentsSort file
  _ <- MaybeT . fmap Just . callCommand $ TP.printf gcc file gf scale output
  _ <- MaybeT . fmap Just $ externalFragmentsSort output
  _ <- MaybeT . fmap Just . callCommand $ TP.printf "bedGraphToBigWig %s %s %s" output gf wigOut
  return ()

-- | Write the line of the Fragment or BedGraph file, depending on
-- normalization.
writeLine :: (MonadResource m) => HMap.HashMap T.Text [Int]
                               -> ClusterHandleMap
                               -> [T.Text]
                               -> m (Maybe ())
writeLine cellClusterMap (ClusterHandleMap chm) all@(chrom:start:end:barcode:duplicateCount:_) = S.liftIO . runMaybeT $ do
  clusters <- MaybeT . return $ HMap.lookup barcode cellClusterMap

  let outputRow = compress
                . flip B.append "\n"
                . B.intercalate "\t"
                $ fmap (B.fromStrict . T.encodeUtf8) all
      write :: Int -> MaybeT IO ()
      write c = do
        h <- MaybeT . return $ IMap.lookup c chm
        MaybeT . fmap Just $ B.hPutStr h outputRow

  -- Fragment file
  mapM_ write clusters
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
writeLine _ _ xs = error $ "writeLine: Unexpected number of columns, did you use the fragments.tsv.gz 10x format? Input: " <> show xs

-- | Get cluster handle map to have several handles open.
getClusterHandleMap :: IO.FilePath -> [Int] -> IO ClusterHandleMap
getClusterHandleMap outputPath =
  fmap (ClusterHandleMap . IMap.fromList)
    . mapM (\x -> IO.openBinaryFile (getFileName x) IO.AppendMode >>= return . (x,))
  where
    getFileName cluster =
      outputPath FP.</> T.unpack ("cluster_" <> showt cluster <> "_fragments.tsv.gz")

-- | Get the list of cluster output files.
getClusterFiles :: IO.FilePath -> [Int] -> [IO.FilePath]
getClusterFiles outputPath = fmap getFileName
  where
    getFileName cluster =
      outputPath FP.</> T.unpack ("cluster_" <> showt cluster <> "_fragments.tsv.gz")

-- | Save the fragments per cluster. Loads in the original fragments file to
-- stream out cluster specific fragments.
saveClusterFragments :: OutputDirectory
                     -> BedGraphFlag
                     -> GenomeFile
                     -> GenomecovCommand
                     -> Maybe LabelMap
                     -> AllNodesFlag
                     -> PeakNodes
                     -> PeakNodesLabels
                     -> ClusterResults
                     -> Either MatrixFileType MatrixFileType
                     -> IO ()
saveClusterFragments
  output
  (BedGraphFlag bgf)
  genome
  gcc
  lm
  (AllNodesFlag anf)
  (PeakNodes pkns)
  (PeakNodesLabels pknls)
  cr
  (Left (CompressedFragments (FragmentsFile file))) = do
  -- Where to place output files
  let outputPath = unOutputDirectory output FP.</> "cluster_fragments"

  restartDir outputPath

  -- Check for occurrence of label map.
  when (isNothing lm && (not $ IMap.null pknls)) $
    hPutStrLn IO.stderr "--labels-file argument required for --peak-node-labels. Ignoring filter ..."

  let checkLabel :: CellInfo -> Int -> Maybe Bool
      checkLabel cellInfo cluster = do
        validLabs <- IMap.lookup cluster pknls
        label <- join
               $ fmap
                  ( Map.lookup (Id . unCell $ L.view barcode cellInfo)
                  . unLabelMap
                  )
                  lm
        return $ Set.member label validLabs
      clusterAssocList =
        fmap
            (\ (!cellInfo, !c)
            -> ( unCell $ _barcode cellInfo
               , bool (filter (fromMaybe True . checkLabel cellInfo)) id (IMap.null pknls)  -- Specific node label selection.
               . bool (filter (flip ISet.member pkns)) id (ISet.null pkns)  -- Specific node selection.
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
      clusters = Set.toList
               . Set.fromList
               . mconcat
               . HMap.elems
               $ cellClusterMap

  -- Open the cluster file handles.
  clusterHandleMap <- getClusterHandleMap outputPath clusters

  hPutStrLn IO.stderr "Streaming fragments to cluster fragment files ..."
  -- Stream in file for fragments.
  runResourceT
    . S.effects
    . S.mapMaybeM (writeLine cellClusterMap clusterHandleMap)
    . S.filter (maybe False (not . T.null) . headMay)
    $ processedStream

  hPutStrLn IO.stderr "Closing cluster fragment files ..."
  -- Close the cluster file handles.
  mapM_ IO.hClose . unClusterHandleMap $ clusterHandleMap

  -- Sort cluster files.
  hPutStrLn IO.stderr "Sorting cluster fragment files ..."
  mapM_ externalCompressedFragmentsSort . getClusterFiles outputPath $ clusters

  -- Stream in file for bedgraph.
  when bgf $ do
    hPutStrLn IO.stderr "Creating bedgraphs ..."
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
saveClusterFragments _ _ _ _ _ _ _ _ _ _ = error "saveClusterFragments: Does not supported this matrix type. See too-many-cells -h for each entry point for more information"

-- | Run the specified command on a file. Command should have \"%s\" where the
-- output file and then input file (order is important) should be.
-- For instance: `command --output %s --other --args --input %s`.
peakCallFile :: PeakCommand
             -> GenomeFile
             -> IO.FilePath
             -> IO.FilePath
             -> IO IO.FilePath
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
    _ <- callCommand $ TP.printf c temp name output
    _ <- externalFragmentsSort wigIn
    -- _ <- callCommand $ TP.printf "bedGraphToBigWig %s %s %s" wigIn gf wigOut
    return $ output FP.</> (name <> "_peaks.narrowPeak")

-- | Convert between filepaths.
toTurtleFile :: IO.FilePath -> Turtle.FilePath
toTurtleFile = Turtle.fromText . T.pack

-- | Convert narrowPeak to bdg format.
narrowPeakToBdg :: Turtle.FilePath -> IO Turtle.FilePath
narrowPeakToBdg file = do
  let outFile = FSP.replaceExtension file "bdg"

  sh $
    output outFile
      . fmap fromJust
      . mfilter isJust
      . fmap ( join
             . fmap (textToLine . T.intercalate "\t")
             . (\xs -> mapM (atMay xs) [0, 1, 2, 4])
             . T.splitOn "\t"
             . lineToText
             )
      . input
      $ file

  return outFile

-- | Return the union of peaks from all requested nodes.
unionPeaks :: OutputDirectory -> [IO.FilePath] -> IO IO.FilePath
unionPeaks (OutputDirectory path) files = do
  let outputPath = toTurtleFile $ path
      outputFile = outputPath Turtle.</> "union.bdg"
      union [x] _ = input x
      union xs stdin = inproc "bedtools" (["unionbedg", "-i"] <> fmap (format fp) xs) stdin

  bdgFiles <- mapM (narrowPeakToBdg . Turtle.fromText . T.pack) files

  sh $ do
    mktree outputPath

    output outputFile
      . inproc "bedtools" ["merge", "-i", "stdin"]
      . union bdgFiles
      $ mempty

  return . T.unpack . format fp $ outputFile

-- | Return the original fragments file with unioned peaks.
unionPeaksFragments :: OutputDirectory
                    -> IO.FilePath
                    -> [Either MatrixFileType MatrixFileType]
                    -> IO ()
unionPeaksFragments (OutputDirectory path) unionFile files = sh $ do
  let outputFile = toTurtleFile $ path FP.</> "union_fragments.tsv.gz"
      getInputFile (Left (CompressedFragments (FragmentsFile file))) = file
      getInputFile _ = error "unionPeaksFragments: Does not supported this matrix type. See too-many-cells -h for each entry point for more information"

  TB.output outputFile
    . TB.compress 6 (TB.WindowBits 31)
    . TB.fromUTF8
    . fmap ( flip T.append "\n"
           . T.intercalate "\t"
           . fromMaybe (error "unionPeaksFragments: Not enough columns.")
           . (\xs -> mapM (atMay xs) [0, 1, 2, 6, 7])
           . T.splitOn "\t"
           . lineToText
           )
    . inproc "bedtools" ( ["intersect", "-wa", "-wb", "-a", T.pack unionFile, "-b"]
                         <> fmap (T.pack . getInputFile) files
                        )
    $ mempty

-- | Run a specified command on all cluster files asynchronously.
peakCallFiles :: PeakCommand
              -> GenomeFile
              -> OutputDirectory
              -> [Either MatrixFileType MatrixFileType]
              -> IO ()
peakCallFiles pc gf (OutputDirectory path) fragmentsFiles = do
  let output = path FP.</> "cluster_peaks"
      fragmentsPath = path FP.</> "cluster_fragments"

  restartDir output

  files <- fmap (fmap (fragmentsPath FP.</>)) . FP.listDirectory $ fragmentsPath

  cores <- getNumCapabilities
  peakFiles <- withTaskGroup cores $ \workers ->
    mapTasks workers . fmap (peakCallFile pc gf output) $ files -- mapTasks_ not working

  unionFile <- unionPeaks (OutputDirectory output) peakFiles
  unionPeaksFragments (OutputDirectory output) unionFile fragmentsFiles

-- | Delete a directory if it exists.
restartDir :: IO.FilePath -> IO ()
restartDir dir = do
  dirExists <- FP.doesDirectoryExist dir
  when dirExists $ FP.removeDirectoryRecursive dir
  FP.createDirectoryIfMissing True dir
  return ()

-- -- | Filter a BED file by labels.
-- labelFilterBed :: LabelMap
--                -> Set.Set Label
--                -> Either MatrixFileType MatrixFileType
--                -> FP.FilePath
--                -> IO FP.FilePath
-- labelFilterBed lm labels (Left (CompressedFragments (FragmentsFile file))) inFile = sh $ do
--   let validCells = Set.fromList
--                  . fmap fst
--                  . filter (flip Set.member labels . snd)
--                  . Map.toAscList
--                  . unLabelMap
--                  $ lm

--   out <- mktempfile ".temp" "temp.bed"

--   output out
--     . inproc "bedtools" ["intersect", "-a", "stdin", "-b", inFile]
--     . mfilter (maybe False (flip Set.member validCells) . atMay 3 . T.splitOn "\t")
--     . toLines
--     . TB.decompress (WindowBits 31)
--     . TB.input
--     $ file
