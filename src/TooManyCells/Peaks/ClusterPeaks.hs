{- TooManyCells.MakeTree.ClusterPeaks
Gregory W. Schwartz

Collects functions pertaining to printing cluster fragments and obtaining peaks
per cluster.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Peaks.ClusterPeaks
    ( saveClusterFragments
    ) where

-- Remote
import BirchBeer.Types (Cluster (..))
import Codec.Compression.GZip (compress)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT (..))
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Data.Maybe (mapMaybe)
import Data.Streaming.Zlib (WindowBits (..))
import Safe (headMay)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.Directory as FP
import qualified System.FilePath as FP
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Debug.Trace

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.File.Types

-- | Save the fragments per cluster. Loads in the original fragments file to
-- stream out cluster specific fragments.
saveClusterFragments :: OutputDirectory
                     -> ClusterResults
                     -> Either MatrixFileType MatrixFileType
                     -> IO ()
saveClusterFragments output cr (Left (CompressedFragments (FragmentsFile file))) = do
  -- Where to place output files.
  FP.removeDirectoryRecursive $ unOutputDirectory output
                              FP.</> "cluster_fragments"
  FP.createDirectoryIfMissing True $ unOutputDirectory output
                              FP.</> "cluster_fragments"

  let clusterMap =
        HMap.fromList
          . mapMaybe
              (\ (!cellInfo, !c)
              -> L.sequenceOf L.both ( Just . unCell $ _barcode cellInfo
                                     , fmap (showt . unCluster) $ headMay c
                                     )
              )
          . _clusterList
          $ cr
      writeLine :: (MonadResource m) => [T.Text] -> m (Maybe ())
      writeLine all@(chrom:start:end:barcode:duplicateCount:_) = S.liftIO . runMaybeT $ do
        cluster <- MaybeT . return $ HMap.lookup barcode clusterMap
        MaybeT
          . fmap Just
          . B.appendFile ( unOutputDirectory output
                    FP.</> "cluster_fragments"
                    FP.</> T.unpack ("cluster_" <> cluster <> "_fragments.tsv.gz")
                         )
          . compress
          . B.intercalate "\t"
          $ (fmap (B.fromStrict . T.encodeUtf8) all) <> ["\n"]
      writeLine xs = error $ "Unexpected number of columns, did you use the fragments.tsv.gz 10x format? Input: " <> show xs

  -- Stream in file.
  runResourceT
    . S.effects
    . S.mapMaybeM (writeLine . T.splitOn "\t" . T.decodeUtf8)
    . S.mapped BS.toStrict
    . BS.lines
    . decompressStreamAll (WindowBits 31) -- For gunzip
    . BS.readFile
    $ file
saveClusterFragments _ _ _ = error "Does not supported this matrix type. See too-many-cells -h for each entry point for more information"
