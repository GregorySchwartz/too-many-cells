{- TooManyCells.Program.LoadMatrix
Gregory W. Schwartz

Loading matrix for command line program.
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module TooManyCells.Program.LoadMatrix where

-- Remote
import BirchBeer.Types
import Control.Concurrent.Async.Pool (withTaskGroup, mapReduce, wait)
import Control.Monad (when, join)
import Control.Monad.STM (atomically)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bool (bool)
import Data.Either (isRight)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Conc (getNumCapabilities)
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Options.Generic
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Control.Lens as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.File.Types
import TooManyCells.Matrix.AtacSeq
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Preprocess
import TooManyCells.Matrix.Utility
import TooManyCells.Matrix.Load
import TooManyCells.Program.Options
import TooManyCells.Program.Utility
import qualified Data.Sparse.Common as S
import Control.Lens

-- | Load the single cell matrix, post-whitelist-filtered cells.
loadSSM :: Options -> FilePath -> IO SingleCells
loadSSM opts matrixPath' = do
  fileExist      <- FP.doesFileExist matrixPath'
  directoryExist <- FP.doesDirectoryExist matrixPath'
  compressedFileExist <- FP.doesFileExist $ matrixPath' FP.</> "matrix.mtx.gz"

  matrixFile' <- getMatrixFileType matrixPath'

  let featuresFile'  = FeatureFile
                  $ matrixPath'
             FP.</> (bool "genes.tsv" "features.tsv.gz" compressedFileExist)
      cellsFile'  = CellFile
                  $ matrixPath'
             FP.</> (bool "barcodes.tsv" "barcodes.tsv.gz" compressedFileExist)
      delimiter'      =
          Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
      transpose'      = TransposeFlag . unHelpful . matrixTranspose $ opts
      featureColumn'  =
          FeatureColumn . fromMaybe 1 . unHelpful . featureColumn $ opts
      noBinarizeFlag' = NoBinarizeFlag . unHelpful . noBinarize $ opts
      binWidth' = fmap BinWidth . unHelpful . binwidth $ opts
      excludeFragments' = fmap (ExcludeFragments . T.pack)
                        . unHelpful
                        . excludeMatchFragments
                        $ opts
      blacklistRegionsFile' = fmap BlacklistRegions
                            . unHelpful
                            . blacklistRegionsFile
                            $ opts
      cellWhitelistFile' =
            fmap CellWhitelistFile . unHelpful . cellWhitelistFile $ opts

  cellWhitelist <- liftIO . sequence $ fmap getCellWhitelist cellWhitelistFile'

  let unFilteredSc   =
        case matrixFile' of
            (Left (DecompressedMatrix file))  ->
              loadSparseMatrixDataStream delimiter' file
            (Left (CompressedFragments file))  -> do
              liftIO $ when (isNothing binWidth') $
                hPutStrLn stderr "\nWarning: No binwidth specified for fragments file\
                                  \ input. This will make the feature list extremely large\
                                  \ and may result in many outliers. Please see --binwidth.\
                                  \ Ignore this message if using peaks.\
                                  \ Continuing..."
              liftIO $ when (isNothing cellWhitelist) $
                hPutStrLn stderr "\nWarning: No cell whitelist specified for fragments file\
                                  \ input. This will use all barcodes in the file. Most times\
                                  \ this file contains barcodes that are not cells. Please see\
                                  \ --cell-whitelist-file. Continuing..."
              fmap (bool binarizeSc id . unNoBinarizeFlag $ noBinarizeFlag')
                . loadFragments cellWhitelist blacklistRegionsFile' excludeFragments' binWidth'
                  $ file
            (Left file@(BedGraph _))  -> do
              liftIO $ when (isNothing binWidth') $
                hPutStrLn stderr "\nWarning: No binwidth specified for BedGraph file\
                                  \ input. This will make the feature list extremely large\
                                  \ and may result in many outliers. Please see --binwidth.\
                                  \ Ignore this message if using peaks.\
                                  \ Continuing..."
              fmap (bool binarizeSc id . unNoBinarizeFlag $ noBinarizeFlag')
                . loadBdgBW blacklistRegionsFile' excludeFragments' binWidth'
                $ file
            (Left file@(BigWig _))  -> do
              liftIO $ when (isNothing binWidth') $
                hPutStrLn stderr "\nWarning: No binwidth specified for bigWig file\
                                  \ input. This will make the feature list extremely large\
                                  \ and may result in many outliers. Please see --binwidth.\
                                  \ Ignore this message if using peaks.\
                                  \ Continuing..."
              fmap (bool binarizeSc id . unNoBinarizeFlag $ noBinarizeFlag')
                . loadBdgBW blacklistRegionsFile' excludeFragments' binWidth'
                $ file
            (Right (DecompressedMatrix file)) ->
              loadCellrangerData featureColumn' featuresFile' cellsFile' file
            (Right (CompressedMatrix file))   ->
              loadCellrangerDataFeatures featureColumn' featuresFile' cellsFile' file
            _ -> error "Does not supported this matrix type. See too-many-cells -h for each entry point for more information"

  let whiteListFilter Nothing = id
      whiteListFilter (Just wl) = filterWhitelistSparseMat wl
      -- Convert non-read data (all matrix formats) to bins. This is done
      -- separately from the read data as it's faster to do this process in the
      -- fragment reading before the matrix creation.
      windowSc Nothing sc = sc
      windowSc (Just bw) sc =
        case matrixFile' of
          (Left file@(CompressedFragments _)) -> sc
          (Left file@(BigWig _)) -> sc
          otherwise -> bool sc (rangeToBinSc bw sc) . isChrRegionMat $ sc
      transposeFunc = bool id transposeSc $ unTransposeFlag transpose'  -- Whether to transpose matrix

  fmap (windowSc binWidth' . whiteListFilter cellWhitelist . transposeFunc) unFilteredSc

-- | Load all single cell matrices.
loadAllSSM :: Options -> IO (Maybe (SingleCells, Maybe LabelMap))
loadAllSSM opts = runMaybeT $ do
  let matrixPaths'       = unHelpful . matrixPath $ opts
      normalizations'    = getNormalization opts
      pca'               = fmap PCADim . unHelpful . pca $ opts
      lsa'               = fmap LSADim . unHelpful . lsa $ opts
      svd'               = fmap SVDDim . unHelpful . svd $ opts
      dropDimensionFlag' = DropDimensionFlag . unHelpful . dropDimension $ opts
      binarizeFlag'      = BinarizeFlag . unHelpful . binarize $ opts
      binWidth'          = fmap BinWidth . unHelpful . binwidth $ opts
      shiftPositiveFlag' =
        ShiftPositiveFlag . unHelpful . shiftPositive $ opts
      customLabel' = (\ xs -> bool
                                (fmap (Just . CustomLabel) xs)
                                (repeat Nothing)
                            . null
                            $ xs
                      )
                    . unHelpful
                    . customLabel
                    $ opts
      readOrErr err = fromMaybe (error err) . readMaybe
      filterThresholds'  = fmap FilterThresholds
                         . fmap (readOrErr "Cannot read --filter-thresholds")
                         . unHelpful
                         . filterThresholds
                         $ opts
      customRegions' = CustomRegions
                     . fmap (either (\x -> error $ "Cannot parse region format `chrN:START-END` in: " <> x) id . parseChrRegion)
                     . unHelpful
                     . customRegion
                     $ opts

  liftIO $ when ((isJust pca' || isJust lsa' || isJust svd') && (elem TfIdfNorm normalizations')) $
    hPutStrLn stderr "\nWarning: Dimensionality reduction (creating negative numbers) with tf-idf\
                     \ normalization may lead to NaNs or 0s before spectral\
                     \ clustering (leading to svdlibc to hang or dense SVD\
                     \ to error out)! Continuing..."

  cores <- MaybeT . fmap Just $ getNumCapabilities
  (whitelistSc, whitelistLM) <- MaybeT
        $ if null matrixPaths'
            then return Nothing
            else
              withTaskGroup cores $ \workers ->
                fmap ( Just
                     . (L.over L._1 (fastBinJoinCols binWidth' False))  -- Possibly undo feature change for fast bin joining
                     . unLabelMat
                     )
                  . join
                  . atomically
                  . fmap wait
                  . mapReduce workers
                  . zipWith (\l -> fmap (labelRows l)) customLabel'  -- Depending on which axis to label from transpose.
                  . fmap (fmap (fastBinJoinCols binWidth' True) . loadSSM opts)  -- Load matrices, possible preparing for fast bin joining
                  $ matrixPaths'

  let sc = maybe
            id
            (\x -> filterNumSparseMat x)
            filterThresholds'
         $ whitelistSc
      normMat TfIdfNorm      = tfidfScaleSparseMat
      normMat UQNorm         = uqScaleSparseMat
      normMat MedNorm        = medScaleSparseMat
      normMat TotalMedNorm   = scaleSparseMat
      normMat TotalNorm      = totalScaleSparseMat
      normMat (LogCPMNorm b) = logCPMSparseMat b
      normMat QuantileNorm   = quantileScaleSparseMat
      normMat NoneNorm       = id
      applyNorms mat = foldl' (\acc norm -> L.over matrix (normMat norm) acc) mat normalizations'
      processSc = L.over matrix (MatObsRow . S.sparsifySM . unMatObsRow)
                . L.over matrix ( bool id shiftPositiveMat
                                $ unShiftPositiveFlag shiftPositiveFlag'
                                )
                . (\m -> maybe m (flip (pcaSparseSc dropDimensionFlag') m) pca')
                . (\m -> maybe m (flip (lsaSparseSc dropDimensionFlag') m) lsa')
                . (\m -> maybe m (flip (svdSparseSc dropDimensionFlag') m) svd')
                . bool (transformChrRegions customRegions') id (null $ unCustomRegions customRegions')
                . applyNorms
                . bool id binarizeSc (unBinarizeFlag binarizeFlag')
      processedSc = processSc sc
      -- Filter label map if necessary.
      labelMap = (\ valid -> fmap ( LabelMap
                                  . Map.filterWithKey (\k _ -> Set.member k valid)
                                  . unLabelMap
                                  )
                                  $ whitelistLM
                 )
               . Set.fromList
               . fmap (Id . unCell)
               . V.toList
               . L.view rowNames
               $ processedSc


  -- Check for empty matrix.
  when (V.null . getRowNames $ processedSc) . error $ emptyMatErr "cells"

  liftIO $ when ( length matrixPaths' > 1
               && ( fromMaybe False
                  . fmap (isRight . parseChrRegion)
                  . flip (V.!?) 0
                  $ getColNames processedSc
                  )
                )
         $
    hPutStrLn stderr "\nNote: Detected chromosome region features.\
                     \ Matrices were combined by overlapping features.\
                     \ To disable this feature, make sure the feature names\
                     \ are NOT in the form `chrN:START-END`. For instance,\
                     \ just add any character to the beginning of the feature.\
                     \ Continuing..."

  liftIO . mapM_ print . matrixValidity $ processedSc

  return (processedSc, labelMap)
