{- TooManyCells.Program.Spatial
Gregory W. Schwartz

Spatial entry point for command line program.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Program.Spatial where

-- Remote
import qualified System.FilePath as FP
import qualified Turtle as TU

-- Local
import TooManyCells.File.Types (OutputDirectory (..))
import TooManyCells.Program.Options
import TooManyCells.Spatial.PlotProjection (plotSpatialProjection)

-- | Spatial path.
spatialMain :: Options -> IO ()
spatialMain opts = do
  let readOrErr err = fromMaybe (error err) . readMaybe
      delimiter'     =
          Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
      labelsFile' =
          fmap LabelFile . unHelpful . labelsFile $ opts
      projectionFile' =
          maybe (error "--projection-file required") ProjectionFile
            . unHelpful
            . projectionFile
            $ opts
      outputDir' = OutputDirectory
                 . FP.fromText
                 . fromMaybe "out_spatial"
                 . unHelpful
                 . output

  scRes <- loadAllSSM opts
  let processedSc = fmap fst scRes
      customLabelMap = join . fmap snd $ scRes

  projectionMap <- loadProjectionMap projectionFile'
  labelMap <- if isJust labelsFile'
                then mapM (loadLabelData delimiter') $ labelsFile'
                else return customLabelMap

  plotSpatialProjection outputDir' labelMap projectionMap processedSc
