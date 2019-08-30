{- TooManyCells.MakeTree.ClusterPeaks
Gregory W. Schwartz

Collects functions pertaining to printing cluster fragments and obtaining peaks
per cluster.
-}

{-# LANGUAGE BangPatterns #-}

module TooManyCells.Peaks.ClusterPeaks
    ( saveClusterPeaks
    ) where

-- Remote
import Codec.Compression.GZip (compress)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as HMap

-- Local
import TooManyCells.Matrix.Types

-- | Save the fragments per cluster. Loads in the original fragments file to
-- stream out cluster specific fragments.
saveClusterFragments :: FragmentsFile -> ClusterResults -> IO ()
saveClusterFragments (FragmentsFile ff) cr = do
  withSystemTempFile "temp_fragments.tsv" $ \tempFile h -> do
    let csvOpts = S.defaultDecodeOptions
                    { S.decDelimiter = fromIntegral (ord '\t') }
        clusterMap =
          HMap.fromList
            . mapMaybe
                (\ (!cellInfo, !c)
                -> L.sequenceOf L.both ( _barcode cellInfo
                                       , fmap showt $ headMay c
                                       )
                )
            . _clusterList
            $ cr
        writeLine all@(chrom:start:end:barcode:duplicateCount:_) = do
          cluster <- IMap.lookup barcode clusterMap
          B.appendFile ("cluster_" <> cluster <> "_fragments.tsv.gz")
            . compress
            . B.intercalate "\t"
            $ all <> ["\n"]
        writeLine xs = error $ "Unexpected number of columns, did you use the fragments.tsv.gz 10x format? Input: " <> show xs

    B.readFile mf >>= B.hPut h . decompress >> IO.hClose h

    res <- flip with return $ do

        contents <- S.withBinaryFileContents tempFile

        -- Get indices first. Streaming twice due to memory usage from the fold.
        fmap (either (error . show) id)
                . runExceptT
                . S.mapMaybeM writeLine
                . S.decodeWith csvOpts S.NoHeader
                $ (contents :: BS.ByteString (ExceptT S.CsvParseException Managed) ())
