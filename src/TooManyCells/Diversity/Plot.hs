{- TooManyCells.Diversity.Plot
Gregory W. Schwartz

Collects the functions pertaining to the plotting of data.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.Diversity.Plot
    ( plotDiversity
    , plotChao1
    , plotRarefaction
    , plotDiversityR
    , plotChao1R
    , plotRarefactionR
    ) where

-- Remote
import BirchBeer.Types
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Char (toUpper)
import Data.Colour.Palette.BrewerSet (ColorCat (..), brewerSet)
import qualified Data.Colour.Palette.BrewerSet as Brewer
import Control.Lens
import Control.Monad (forM)
import Language.R as R
import Language.R.QQ (r)
import Plots
import Plots.Axis.Line
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
    axisColourMap .= (colourMap . zip [1..] . brewerSet Brewer.Set1 $ 9)
    xLabel .= "Subsample (# cells)"
    yLabel .= "Estimated clusters (# clusters)"

-- | Plot the diversity of a group of populations.
plotDiversityR :: [Colour Double] -> [PopulationDiversity] -> R s (R.SomeSEXP s)
plotDiversityR colors pops = do
    let labels = fmap getPopLabel pops
        values = fmap (unDiversity . popDiversity) pops
        colorsR = fmap (fmap toUpper . sRGB24show) colors

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))

        df = data.frame(x = labels_hs, y = values_hs)

        ggplot(df, aes(x = reorder(x, -y), y = y, fill = x)) +
            geom_col() +
            xlab("") +
            ylab("Diversity") +
            scale_fill_manual(values = as.character(colorsR_hs)) +
            guides(fill = "none") +
            theme_cowplot() +
            theme(aspect.ratio = 1, axis.text.x = element_text(angle = 315, hjust = 0))
    |]

-- | Plot the Chao1 of a group of populations.
plotChao1R :: [Colour Double] -> [PopulationDiversity] -> R s (R.SomeSEXP s)
plotChao1R colors pops = do
    let labels = fmap getPopLabel pops
        values = fmap (unChao1 . popChao1) pops
        colorsR = fmap (fmap toUpper . sRGB24show) colors

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))

        df = data.frame(x = labels_hs, y = values_hs)

        ggplot(df, aes(x = reorder(x, -y), y = y, fill = x)) +
            geom_col() +
            xlab("") +
            ylab("Chao1") +
            scale_fill_manual(values = as.character(colorsR_hs)) +
            guides(fill = "none") +
            theme_cowplot() +
            theme(aspect.ratio = 1, axis.text.x = element_text(angle = 315, hjust = 0))
    |]

-- | Plot the rarefaction curves of a group of populations.
plotRarefactionR ::
     [Colour Double] -> [PopulationDiversity] -> R s (R.SomeSEXP s)
plotRarefactionR colors pops = do
    let labels =
            concatMap
                (\pop ->
                      replicate
                          (length . unRarefaction . popRarefaction $ pop)
                          (getPopLabel pop)
                )
                pops
        valuesX =
            fmap (unX . fst) . concatMap (unRarefaction . popRarefaction) $ pops
        valuesY =
            fmap (unY . snd) . concatMap (unRarefaction . popRarefaction) $ pops
        colorsR = fmap (fmap toUpper . sRGB24show) colors

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))

        df = data.frame(x = valuesX_hs, y = valuesY_hs, labels = labels_hs)

        ggplot(df, aes(x = x, y = y, color = labels)) +
            geom_line() +
            xlab("Subsample") +
            ylab("Estimated richness") +
            scale_color_manual(values = as.character(colorsR_hs)) +
            guides(color = guide_legend(title = "")) +
            theme_cowplot() +
            theme(aspect.ratio = 0.5)
    |]
