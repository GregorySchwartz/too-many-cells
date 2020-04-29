{- too-many-cells
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Remote
import Options.Generic

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

main :: IO ()
main = do
    opts <- getRecord "too-many-cells, Gregory W. Schwartz.\
                      \ Clusters and analyzes single cell data."

    case opts of
        MakeTree{}     -> makeTreeMain opts
        Interactive{}  -> interactiveMain opts
        Differential{} -> differentialMain opts
        Diversity{}    -> diversityMain opts
        Paths{}        -> pathsMain opts
        Classify{}     -> classifyMain opts
        Peaks{}        -> peaksMain opts
        Motifs{}       -> motifsMain opts
        MatrixOutput{} -> matrixOutputMain opts
