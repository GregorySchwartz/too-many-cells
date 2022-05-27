{- TooManyCells.Program.Spatial
Gregory W. Schwartz

Spatial entry point for command line program.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}

module TooManyCells.Program.Spatial where

-- Remote
import BirchBeer.Load (loadLabelData)
import BirchBeer.Types (LabelFile (..), Delimiter (..), LabelMap (..), Sample (..), Feature (..), Label (..))
import Control.Concurrent.Async.Pool (withTaskGroup, mapTasks)
import Control.Monad (join, forM_, guard, unless, when, void)
import Data.Bool (bool)
import Data.Maybe (isJust, fromMaybe)
import GHC.Conc (getNumCapabilities)
import Language.R.Instance as R
import Language.R.QQ
import System.Directory (getTemporaryDirectory)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified H.Prelude as H
import qualified System.FilePath as FP
import qualified Turtle as TU

-- Local
import TooManyCells.Matrix.Load (loadProjectionMap)
import TooManyCells.Program.LoadMatrix (loadAllSSM)
import TooManyCells.Program.Options
import TooManyCells.Spatial.AnnoSpat (scToAnnoSpatFile, runAnnoSpat)
import TooManyCells.Spatial.ProjectionPlot (plotSpatialProjection)
import TooManyCells.Spatial.SummaryPlot (plotSummary)
import TooManyCells.Spatial.Relationships (spatialRelationshipsR)
import TooManyCells.Spatial.Utility (subsampleProjectionMap, markToText)
import qualified TooManyCells.File.Types as Too
import qualified TooManyCells.Matrix.Types as Too
import qualified TooManyCells.Program.Utility as Too
import qualified TooManyCells.Spatial.Types as Too

-- | General call for spatialRelationshipsR.
spatialRelationshipsCall :: Too.OutputDirectory
                         -> Too.SkipFinishedFlag
                         -> Too.PCFCrossFlag
                         -> Too.ProjectionMap
                         -> Maybe LabelMap
                         -> Too.SingleCells
                         -> Maybe Sample
                         -> [Too.Mark]
                         -> IO ()
spatialRelationshipsCall (Too.OutputDirectory outDir) skipFlag' pcfCrossFlag' pm lm sc sample marks = do
  let markString = T.unpack . T.intercalate "_" . fmap markToText $ marks
      folderName (Just (Sample x)) = T.unpack x <> "_" <> markString
      folderName Nothing = markString
      outDir' = Too.OutputDirectory
              $ outDir
         FP.</> folderName sample
      finalFile = TU.fromText
                . T.pack
                $ Too.unOutputDirectory outDir' FP.</> "stats.csv"

  -- If stats.csv exists and is not empty, then run everything
  !skip <- (&&)
       <$> (pure $ Too.unSkipFinishedFlag skipFlag')
       <*> (TU.testfile finalFile)
  when (not skip) $ spatialRelationshipsR outDir' pcfCrossFlag' pm lm sc marks

-- | Helper for spatial relationships function.
spatialRelationshipsHelper :: Too.OutputDirectory
                           -> Too.SkipFinishedFlag
                           -> Too.PCFCrossFlag
                           -> Too.ProjectionMap
                           -> Maybe LabelMap
                           -> Too.SingleCells
                           -> Maybe Sample
                           -> [Too.Mark]
                           -> IO ()
spatialRelationshipsHelper _ _ _ _ _ _ _ [] = pure ()
spatialRelationshipsHelper outDir skipFlag' pcfCrossFlag' pm lm sc sample (fmap markToText -> ["ALL"]) = do
  cores <- getNumCapabilities
  withTaskGroup cores $ \workers ->
    void
      . mapTasks workers  -- mapTasks_ not working
      . fmap (\(!x, !y) -> spatialRelationshipsCall outDir skipFlag' pcfCrossFlag' pm lm sc sample [x, y])
      $ (,) <$> getMarks lm <*> getMarks lm
  where
    getMarks Nothing = fmap Too.MarkFeature . V.toList . L.view Too.colNames $ sc
    getMarks (Just lm) =
      fmap Too.MarkLabel . Set.toList . Set.fromList . Map.elems . unLabelMap $ lm
spatialRelationshipsHelper outDir skipFlag' pcfCrossFlag' pm lm sc sample marks =
  spatialRelationshipsCall outDir skipFlag' pcfCrossFlag' pm lm sc sample marks

-- | Helper for plotting spatial summary.
spatialSummary :: Too.OutputDirectory
               -> Delimiter
               -> Too.IncludeOthersFlag
               -> Maybe Too.StateLabelsFile
               -> IO ()
spatialSummary outputDir' delimiter' iof stateLabelsFile' = do
  stateLabelMap <-
    mapM
      ( fmap (Too.StateLabelMap . unLabelMap)
      . loadLabelData delimiter'
      . LabelFile
      . Too.unStateLabelsFile
      )
      stateLabelsFile'
  let featureList = fmap
                      Feature
                      [ "meanCorr"
                      , "maxCorr"
                      , "minCorr"
                      , "topMaxCorr"
                      , "topMeanCorr"
                      , "negSwap"
                      , "posSwap"
                      , "longestPosLength"
                      , "longestNegLength"
                      , "maxPosWithVal"
                      , "logMaxPosWithVal"
                      , "maxPos"
                      , "minPos"
                      ]
  mapM_ (plotSummary outputDir' iof stateLabelMap) featureList

-- | Spatial path.
spatialMain :: Subcommand -> IO ()
spatialMain sub@(SpatialCommand opts) = H.withEmbeddedR R.defaultConfig $ do
  let onlySummaryFlag' = onlySummaryFlag opts
      outputDir' = Too.OutputDirectory . (output :: Spatial -> String) $ opts
      stateLabelsFile' = fmap Too.StateLabelsFile . stateLabelsFile $ opts
      delimiter' = Delimiter
                 . (delimiter :: LoadMatrixOptions -> Char)
                 . (loadMatrixOptions :: Spatial -> LoadMatrixOptions)
                 $ opts
      skipFlag' = Too.SkipFinishedFlag $ skipFinishedFlag opts
      iof' = Too.IncludeOthersFlag $ includeOthersFlag opts

  unless onlySummaryFlag' $ do

    let readOrErr err = fromMaybe (error err) . readMaybe
        delimiter'        = Delimiter
                          . (delimiter :: LoadMatrixOptions -> Char)
                          . (loadMatrixOptions :: Spatial -> LoadMatrixOptions)
                          $ opts
        projectionFile' =
            maybe (error "--projection-file required") Too.ProjectionFile
              . (projectionFile :: Spatial -> Maybe String)
              $ opts
        annoSpatMarkerFile' = fmap Too.AnnoSpatMarkerFile . annoSpatMarkerFile $ opts
        annoSpatCommand' = Too.AnnoSpatCommand . annoSpatCommand $ opts

    scRes <- fmap (fromMaybe (error "Requires --matrix-path"))
          . loadAllSSM sub
          $ (loadMatrixOptions :: Spatial -> LoadMatrixOptions) opts
    let processedSc = fst scRes
        customLabelMap = snd scRes

    projectionMap <- loadProjectionMap projectionFile'

    let labelsFileError = hPutStrLn stderr "Warning: Problem in AnnoSpat, skipping label generation ..."
    labelsFile' <-
      case annoSpatMarkerFile' of
        Nothing ->
          pure . fmap LabelFile . (labelsFile :: Spatial -> Maybe String) $ opts
        (Just mf) -> (=<<) (maybe (labelsFileError >> pure Nothing) pure) . TU.reduce Fold.head $ do
          let annoOutDir = Too.OutputDirectory
                        $ Too.unOutputDirectory outputDir' FP.</> "AnnoSpat_out"
          tmpDir <- TU.liftIO $ fmap (TU.fromText . T.pack) getTemporaryDirectory
          tmpOut <- fmap Too.TempPath $ TU.mktempfile tmpDir "AnnoSpat_input.csv"
          startEndCols <- TU.liftIO $ scToAnnoSpatFile projectionMap processedSc tmpOut

          case startEndCols of
            Nothing -> pure Nothing
            (Just (startCol, endCol)) ->
              TU.liftIO
                $ runAnnoSpat
                    annoSpatCommand'
                    tmpOut
                    mf
                    annoOutDir
                    startCol
                    endCol

    labelMap <- if isJust labelsFile'
                  then mapM (loadLabelData delimiter') $ labelsFile'
                  else return customLabelMap

    let marks' = fmap
                  ( bool (Too.MarkFeature . Feature) (Too.MarkLabel . Label)
                  . isJust
                  $ labelMap
                  )
              . mark
              $ opts

    pcfCrossFlag' <- case (isJust labelMap, pcfCrossFlag opts) of
                      (False, True) -> do
                        hPutStrLn stderr "Warning: Continuous feature marks detected but pcfcross requested, ignoring pcfcross request ..."
                        return $ Too.PCFCrossFlag False
                      _ ->
                        return . Too.PCFCrossFlag . pcfCrossFlag $ opts

    let samples = Set.toList
                . Set.fromList
                . fmap fst
                . Map.elems
                . Too.unProjectionMap
                $ projectionMap

    forM_ samples $ \s -> do
      let s' = fromMaybe (Sample "total") s
          sOutLabel = T.unpack $ unSample s'
          projectionOutput = Too.OutputDirectory
                            . (FP.</> sOutLabel FP.</> "projections")
                            . Too.unOutputDirectory
                            $ outputDir'
          relationshipsOutput = Too.OutputDirectory
                              . (FP.</> sOutLabel FP.</> "relationships")
                              . Too.unOutputDirectory
                              $ outputDir'
          subPm = subsampleProjectionMap s projectionMap
      plotSpatialProjection projectionOutput labelMap subPm processedSc s'

      case marks' of
        [] -> pure ()
        m -> spatialRelationshipsHelper relationshipsOutput skipFlag' pcfCrossFlag' subPm labelMap processedSc s m

  spatialSummary outputDir' delimiter' iof' stateLabelsFile'
spatialMain _ = error "Wrong path in spatial, contact Gregory Schwartz for this error."
