{- too-many-cells
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Remote
import Options.Applicative

-- Local
import TooManyCells.Program.Differential
import TooManyCells.Program.Diversity
import TooManyCells.Program.Interactive
import TooManyCells.Program.MakeTree
import TooManyCells.Program.MatrixOutput
import TooManyCells.Program.Classify
import TooManyCells.Program.Motifs
import TooManyCells.Program.Options
import TooManyCells.Program.Paths
import TooManyCells.Program.Peaks
import TooManyCells.Program.Spatial

main :: IO ()
main = do
  let opts = info (sub <**> helper)
                ( fullDesc
              <> progDesc "Clusters and analyzes single cell data."
              <> header "too-many-cells, Gregory W. Schwartz" )

  config <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) opts

  let mainEntry (MakeTreeCommand _)     = makeTreeMain config
      mainEntry (InteractiveCommand _)  = interactiveMain config
      mainEntry (DifferentialCommand _) = differentialMain config
      mainEntry (DiversityCommand _)    = diversityMain config
      mainEntry (PathsCommand _)        = pathsMain config
      mainEntry (ClassifyCommand _)     = classifyMain config
      mainEntry (PeaksCommand _)        = peaksMain config
      mainEntry (MotifsCommand _)       = motifsMain config
      mainEntry (MatrixOutputCommand _) = matrixOutputMain config
      mainEntry (SpatialCommand _)      = spatialMain config

  mainEntry config
