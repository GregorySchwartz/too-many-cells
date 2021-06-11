{- TooManyCells.Program.Spatial
Gregory W. Schwartz

Spatial entry point for command line program.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.Spatial where

-- Remote
import BirchBeer.Load (loadLabelData)
import BirchBeer.Types (LabelFile (..), Delimiter (..))
import Control.Monad (join, forM_)
import Data.Maybe (isJust, fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.FilePath as FP
import qualified Turtle as TU

-- Local
import TooManyCells.File.Types (OutputDirectory (..), ProjectionFile (..))
import TooManyCells.Matrix.Load (loadProjectionMap)
import TooManyCells.Matrix.Types (ProjectionMap (..))
import TooManyCells.Program.LoadMatrix (loadAllSSM)
import TooManyCells.Program.Options
import TooManyCells.Spatial.ProjectionPlot (plotSpatialProjection)
import TooManyCells.Spatial.Utility (subsampleProjectionMap)

-- | Spatial path.
spatialMain :: Subcommand -> IO ()
spatialMain sub@(SpatialCommand opts) = do
  let readOrErr err = fromMaybe (error err) . readMaybe
      delimiter'        = Delimiter
                        . (delimiter :: LoadMatrixOptions -> Char)
                        . (loadMatrixOptions :: Spatial -> LoadMatrixOptions)
                        $ opts
      labelsFile' =
          fmap LabelFile . (labelsFile :: Spatial -> Maybe String) $ opts
      projectionFile' =
          maybe (error "--projection-file required") ProjectionFile
            . (projectionFile :: Spatial -> Maybe String)
            $ opts
      outputDir' = OutputDirectory . (output :: Spatial -> String) $ opts

  scRes <- fmap (fromMaybe (error "Requires --matrix-path"))
         . loadAllSSM sub
         $ (loadMatrixOptions :: Spatial -> LoadMatrixOptions) opts
  let processedSc = fst scRes
      customLabelMap = snd scRes

  projectionMap <- loadProjectionMap projectionFile'
  labelMap <- if isJust labelsFile'
                then mapM (loadLabelData delimiter') $ labelsFile'
                else return customLabelMap

  let samples = Set.toList
              . Set.fromList
              . fmap fst
              . Map.elems
              . unProjectionMap
              $ projectionMap
      projectionOutput =
        OutputDirectory . (FP.</> "projections") . unOutputDirectory $ outputDir'

  forM_ samples $ \s -> do
    let subPm = subsampleProjectionMap s projectionMap
    plotSpatialProjection projectionOutput labelMap subPm processedSc s
spatialMain _ = error "Wrong path in spatial, contact Gregory Schwartz for this error."
