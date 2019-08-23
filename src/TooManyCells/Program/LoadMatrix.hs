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
import Control.Concurrent.Async.Pool (withTaskGroup, mapTasks)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bool (bool)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Conc (getNumCapabilities)
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Options.Generic
import System.IO (hPutStrLn, stderr)
import qualified Control.Lens as L
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

-- | Load the single cell matrix.
loadSSM :: Options -> FilePath -> IO SingleCells
loadSSM opts matrixPath' = do
  fileExist      <- FP.doesFileExist matrixPath'
  directoryExist <- FP.doesDirectoryExist matrixPath'
  compressedFileExist <- FP.doesFileExist $ matrixPath' FP.</> "matrix.mtx.gz"

  let fragmentsFile = (\ (x, y) -> isInfixOf "fragments" x && y == ".tsv.gz")
                    . FP.splitExtensions
                    . FP.takeFileName
                    $ matrixPath'
      matrixFile'
        | fileExist && not fragmentsFile = Left . DecompressedMatrix . MatrixFile $ matrixPath'
        | fileExist && fragmentsFile = Left . CompressedFragments . FragmentsFile $ matrixPath'
        | directoryExist && not compressedFileExist = Right . DecompressedMatrix . MatrixFile $ matrixPath' FP.</> "matrix.mtx"
        | directoryExist && compressedFileExist = Right . CompressedMatrix . MatrixFile $ matrixPath' FP.</> "matrix.mtx.gz"
        | directoryExist = error "Cannot determine matrix pointed to, are there too many matrices here?"
        | otherwise = error "\nMatrix path does not exist."
      featuresFile'  = FeatureFile
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
      cellWhitelistFile' =
            fmap CellWhitelistFile . unHelpful . cellWhitelistFile $ opts
      binWidth' = fmap BinWidth . unHelpful . binwidth $ opts
      unFilteredSc   =
          case matrixFile' of
              (Left (DecompressedMatrix file))  ->
                loadSparseMatrixDataStream delimiter' file
              (Left (CompressedFragments file))  -> do
                liftIO $ when (isNothing binWidth') $
                  hPutStrLn stderr "\nWarning: No binwidth specified for fragments file\
                                   \ input. This will make the feature list extremely large\
                                   \ and may result in many outliers. Please see --binwidth.\
                                   \ Continuing..."
                liftIO $ when (isNothing cellWhitelistFile') $
                  hPutStrLn stderr "\nWarning: No cell whitelist specified for fragments file\
                                   \ input. This will use all barcodes in the file. Most times\
                                   \ this file contains barcodes that are not cells. Please see\
                                   \ --cell-whitelist-file. Continuing..."
                cellWhitelist <- sequence $ fmap getCellWhitelist cellWhitelistFile'
                fmap (bool binarizeSc id . unNoBinarizeFlag $ noBinarizeFlag')
                  . loadFragments cellWhitelist binWidth'
                    $ file
              (Right (DecompressedMatrix file)) ->
                loadCellrangerData featureColumn' featuresFile' cellsFile' file
              (Right (CompressedMatrix file))   ->
                loadCellrangerDataFeatures featureColumn' featuresFile' cellsFile' file
              _ -> error "Does not supported this matrix type. See too-many-cells -h for each entry point for more information"
  unFilteredSc

-- | Load all single cell matrices.
loadAllSSM :: Options -> IO (Maybe (SingleCells, Maybe LabelMap))
loadAllSSM opts = runMaybeT $ do
    let matrixPaths'       = unHelpful . matrixPath $ opts
        cellWhitelistFile' =
            fmap CellWhitelistFile . unHelpful . cellWhitelistFile $ opts
        normalization'     = getNormalization opts
        pca'               = fmap PCADim . unHelpful . pca $ opts
        noFilterFlag'      = NoFilterFlag . unHelpful . noFilter $ opts
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

    liftIO $ when (isJust pca' && (elem normalization' [TfIdfNorm, BothNorm])) $
      hPutStrLn stderr "\nWarning: PCA (creating negative numbers) with tf-idf\
                       \ normalization may lead to NaNs or 0s before spectral\
                       \ clustering (leading to svdlibc to hang or dense SVD\
                       \ to error out)! Continuing..."

    cores <- MaybeT . fmap Just $ getNumCapabilities
    mats <- MaybeT
          $ if null matrixPaths'
              then return Nothing
              else
                withTaskGroup cores $ \workers -> fmap Just
                                                . mapTasks workers
                                                . fmap (loadSSM opts)
                                                $ matrixPaths'
    cellWhitelist <- liftIO . sequence $ fmap getCellWhitelist cellWhitelistFile'

    let whiteListFilter Nothing = id
        whiteListFilter (Just wl) = filterWhitelistSparseMat wl
        (unFilteredSc, unFilteredLM) =
          (\ xs -> ( mconcat $ fmap fst xs
                   , fmap mconcat . sequence . fmap snd $ xs
                   )
          )
            . zipWith labelRows customLabel'
            $ mats
        sc           =
            ( bool (filterNumSparseMat filterThresholds') id
            $ unNoFilterFlag noFilterFlag'
            )
                . whiteListFilter cellWhitelist
                $ unFilteredSc
        normMat TfIdfNorm    = id -- Normalize during clustering.
        normMat UQNorm       = uqScaleSparseMat
        normMat MedNorm      = medScaleSparseMat
        normMat TotalMedNorm = scaleSparseMat
        normMat BothNorm     = scaleSparseMat -- TF-IDF comes later.
        normMat NoneNorm     = id
        processMat  = ( bool id shiftPositiveMat
                      $ unShiftPositiveFlag shiftPositiveFlag'
                      )
                    . (\m -> maybe m (flip pcaDenseMat m) pca')
                    . normMat normalization'
                    . _matrix
        processedSc = sc { _matrix = processMat sc }
        -- Filter label map if necessary.
        labelMap = (\ valid -> fmap ( LabelMap
                                    . Map.filterWithKey (\k _ -> Set.member k valid)
                                    . unLabelMap
                                    )
                                    $ unFilteredLM
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
