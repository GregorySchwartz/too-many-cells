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
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Conc (getNumCapabilities)
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Options.Generic
import System.IO (hPutStrLn, stderr)
import qualified Control.Lens as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
      featureColumn'  =
          FeatureColumn . fromMaybe 1 . unHelpful . featureColumn $ opts
      noBinarizeFlag' = NoBinarizeFlag . unHelpful . noBinarize $ opts
      binWidth' = fmap BinWidth . unHelpful . binwidth $ opts
      excludeFragments' = fmap (ExcludeFragments . B.pack)
                        . unHelpful
                        . excludeMatchFragments
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
                . loadFragments cellWhitelist excludeFragments' binWidth'
                  $ file
            (Right (DecompressedMatrix file)) ->
              loadCellrangerData featureColumn' featuresFile' cellsFile' file
            (Right (CompressedMatrix file))   ->
              loadCellrangerDataFeatures featureColumn' featuresFile' cellsFile' file
            _ -> error "Does not supported this matrix type. See too-many-cells -h for each entry point for more information"

  let whiteListFilter Nothing = id
      whiteListFilter (Just wl) = filterWhitelistSparseMat wl

  fmap (whiteListFilter cellWhitelist) unFilteredSc

-- | Load all single cell matrices.
loadAllSSM :: Options -> IO (Maybe (SingleCells, Maybe LabelMap))
loadAllSSM opts = runMaybeT $ do
    let matrixPaths'       = unHelpful . matrixPath $ opts
        normalization'     = getNormalization opts
        pca'               = fmap PCADim . unHelpful . pca $ opts
        svd'               = fmap SVDDim . unHelpful . svd $ opts
        noFilterFlag'      = NoFilterFlag . unHelpful . noFilter $ opts
        transpose' = TransposeFlag . unHelpful . matrixTranspose $ opts
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
        filterThresholds'  = FilterThresholds
                           . maybe (250, 1) read
                           . unHelpful
                           . filterThresholds
                           $ opts

    liftIO $ when ((isJust pca' || isJust svd') && (elem normalization' [TfIdfNorm, BothNorm])) $
      hPutStrLn stderr "\nWarning: PCA or SVD (creating negative numbers) with tf-idf\
                       \ normalization may lead to NaNs or 0s before spectral\
                       \ clustering (leading to svdlibc to hang or dense SVD\
                       \ to error out)! Continuing..."

    let labelAxis (TransposeFlag False) = labelRows
        labelAxis (TransposeFlag True) = labelCols

    cores <- MaybeT . fmap Just $ getNumCapabilities
    (whitelistSc, whitelistLM) <- MaybeT
          $ if null matrixPaths'
              then return Nothing
              else
                withTaskGroup cores $ \workers ->
                  fmap ( Just
                       . (L.over L._1 (\x -> bool x (transposeSC x) . unTransposeFlag $ transpose'))  -- Whether to transpose the matrix.
                       . unLabelMat
                       )
                    . join
                    . atomically
                    . fmap wait
                    . mapReduce workers
                    . zipWith (\l -> fmap (labelAxis transpose' l)) customLabel'  -- Depending on which axis to label from transpose.
                    . fmap (loadSSM opts)
                    $ matrixPaths'

    let sc           =
          ( bool (filterNumSparseMat filterThresholds') id
          $ unNoFilterFlag noFilterFlag'
          )
            $ whitelistSc
        normMat TfIdfNorm    = id -- Normalize during clustering.
        normMat UQNorm       = uqScaleSparseMat
        normMat MedNorm      = medScaleSparseMat
        normMat TotalMedNorm = scaleSparseMat
        normMat BothNorm     = scaleSparseMat -- TF-IDF comes later.
        normMat LogCPMNorm   = logCPMSparseMat
        normMat NoneNorm     = id
        processMat  = ( bool id shiftPositiveMat
                      $ unShiftPositiveFlag shiftPositiveFlag'
                      )
                    . (\m -> maybe m (flip pcaSparseMat m) pca')
                    . (\m -> maybe m (flip svdMat m) svd')
                    . normMat normalization'
                    . _matrix
        processedSc = sc { _matrix = processMat sc }
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

    liftIO . mapM_ print . matrixValidity $ processedSc

    return (processedSc, labelMap)
