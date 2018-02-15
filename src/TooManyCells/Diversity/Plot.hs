{- TooManyCells.Diversity.Plot
Gregory W. Schwartz

Collects the functions pertaining to the plotting of data.
-}

{-# LANGUAGE FlexibleContexts #-}

module TooManyCells.Diversity.Plot
    ( plotDiversity
    , plotChao1
    , plotRarefaction
    , plotDiversityPy
    , plotChao1Py
    , plotRarefactionPy
    ) where

-- Remote
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Colour.Palette.BrewerSet (ColorCat (..), brewerSet)
import Control.Lens
import Control.Monad (forM)
import Plots
import Plots.Axis.Line
import qualified Graphics.Matplotlib as P
import qualified Data.Text as T

-- Local
import TooManyCells.Diversity.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Get the label of a population diversity as a String.
getPopLabel :: PopulationDiversity -> String
getPopLabel = T.unpack . unLabel . popLabel

-- | Plot the diversity of a group of populations.
plotDiversity :: [PopulationDiversity] -> Diagram B
plotDiversity xs = renderAxis $ r2Axis &~ do
    namedBarPlot (fmap (\x -> (getPopLabel x, unDiversity . popDiversity $ x)) xs) $ do
        vertical .= True
    yMin ?= 0
    hideGridLines
    hide (xAxis . minorTicks)
    hide (xAxis . majorTicks)
    plotColour .= black
    yLabel .= "Diversity"

-- | Plot the Chao1 of a group of populations.
plotChao1 :: [PopulationDiversity] -> Diagram B
plotChao1 xs = renderAxis $ r2Axis &~ do
    namedBarPlot (fmap (\x -> (getPopLabel x, unChao1 . popChao1 $ x)) xs) $ do
        vertical .= True
    hideGridLines
    yMin ?= 0
    hide (xAxis . minorTicks)
    hide (xAxis . majorTicks)
    plotColour .= black
    yLabel .= "Chao1"

-- | Plot the rarefaction curves of a group of populations.
plotRarefaction :: [PopulationDiversity] -> Diagram B
plotRarefaction xs = renderAxis $ r2Axis &~ do
    forM xs $ \pop -> do
        linePlot (fmap (over _2 unY . over _1 unX) . unRarefaction . popRarefaction $ pop) $ do
            key . getPopLabel $ pop

    hideGridLines
    xAxis.axisLineType .= LeftAxisLine
    yAxis.axisLineType .= LeftAxisLine
    axisColourMap .= (colourMap . zip [1..] . brewerSet Set1 $ 9)
    xLabel .= "Subsample (# cells)"
    yLabel .= "Estimated clusters (# clusters)"

-- | Plot the diversity of a group of populations.
plotDiversityPy :: [PopulationDiversity] -> P.Matplotlib
plotDiversityPy pops =
    mconcat
        $ fmap
            (\pop -> P.bar (getPopLabel pop) (unDiversity . popDiversity $ pop))
            pops

-- | Plot the Chao1 of a group of populations.
plotChao1Py :: [PopulationDiversity] -> P.Matplotlib
plotChao1Py pops =
    mconcat
        $ fmap
            (\pop -> P.bar (getPopLabel pop) (unChao1 . popChao1 $ pop))
            pops

-- | Plot the rarefaction curves of a group of populations.
plotRarefactionPy :: [PopulationDiversity] -> P.Matplotlib
plotRarefactionPy pops =
    (P.% P.legend)
        . mconcat
        $ fmap
            (\ pop -> uncurry P.line (getVals pop)
                P.@@ [P.o2 "label" $ getPopLabel pop]
            )
            pops
  where
    getVals = unzip
            . fmap (over _2 unY . over _1 unX)
            . unRarefaction
            . popRarefaction
