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

-- Local
import Options

-- | Load the single cell matrix.
loadSSM :: Options -> FilePath -> IO SingleCells
loadSSM opts matrixPath' = do
  fileExist      <- FP.doesFileExist matrixPath'
  directoryExist <- FP.doesDirectoryExist matrixPath'
  compressedFileExist <- FP.doesFileExist $ matrixPath' FP.</> "matrix.mtx.gz"

  let matrixFile' =
        case (fileExist, directoryExist, compressedFileExist) of
          (False, False, False) -> error "\nMatrix path does not exist."
          (True, False, False)  ->
            Left . DecompressedMatrix . MatrixFile $ matrixPath'
          (False, True, False)  ->
            Right . DecompressedMatrix . MatrixFile $ matrixPath' FP.</> "matrix.mtx"
          (False, True, True)  ->
            Right . CompressedMatrix . MatrixFile $ matrixPath' FP.</> "matrix.mtx.gz"
          _                     -> error "Cannot determine matrix pointed to, are there too many matrices here?"
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
      unFilteredSc   =
          case matrixFile' of
              (Left (DecompressedMatrix file))  ->
                loadSparseMatrixDataStream delimiter' file
              (Right (DecompressedMatrix file)) ->
                loadCellrangerData featureColumn' featuresFile' cellsFile' file
              (Right (CompressedMatrix file))   ->
                loadCellrangerDataFeatures featureColumn' featuresFile' cellsFile' file
              _ -> error "Does not supported this matrix type. See too-many-cells -h for each entry point for more information"
  unFilteredSc

-- | Load all single cell matrices.
loadAllSSM :: Options -> IO (Maybe SingleCells)
loadAllSSM opts = runMaybeT $ do
    let matrixPaths'       = unHelpful . matrixPath $ opts
        cellWhitelistFile' =
            fmap CellWhitelistFile . unHelpful . cellWhitelistFile $ opts
        normalization'     = getNormalization opts
        pca'               = fmap PCADim . unHelpful . pca $ opts
        noFilterFlag'      = NoFilterFlag . unHelpful . noFilter $ opts
        shiftPositiveFlag' =
          ShiftPositiveFlag . unHelpful . shiftPositive $ opts
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

    mats <- MaybeT
          $ if null matrixPaths'
              then return Nothing
              else fmap Just . mapM (loadSSM opts) $ matrixPaths'
    cellWhitelist <- liftIO . sequence $ fmap getCellWhitelist cellWhitelistFile'

    let whiteListFilter Nothing = id
        whiteListFilter (Just wl) = filterWhitelistSparseMat wl
        unFilteredSc = mconcat mats
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
        normMat BothNorm     = scaleSparseMat
        normMat NoneNorm     = id
        processMat  = ( bool id shiftPositiveMat
                      $ unShiftPositiveFlag shiftPositiveFlag'
                      )
                    . (\m -> maybe m (flip pcaDenseMat m) pca')
                    . normMat normalization'
                    . _matrix
        processedSc = sc { _matrix = processMat sc }

    -- Check for empty matrix.
    when (V.null . getRowNames $ processedSc) $ error "Matrix is empty. Check --filter-thresholds, --normalization, or the input matrix for over filtering or incorrect input format."

    liftIO . mapM_ print . matrixValidity $ processedSc

    return processedSc
