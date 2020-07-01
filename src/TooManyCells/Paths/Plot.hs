{- TooManyCells.Paths.Plot
Gregory W. Schwartz

Collects the functions pertaining to plotting the path distances.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module TooManyCells.Paths.Plot
    ( plotPathDistanceR
    ) where

-- Remote
import BirchBeer.Types
import Data.Char (toUpper)
import Data.Colour.SRGB (sRGB24show)
import Language.R as R
import Language.R.QQ (r)
import qualified Control.Lens as L
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- Local
import TooManyCells.Paths.Types

-- | Plot clusters.
plotPathDistanceR :: String
                  -> LabelColorMap
                  -> Bandwidth
                  -> [(Label, Double)]
                  -> R s ()
plotPathDistanceR outputPlot (LabelColorMap cm) (Bandwidth b) distances = do
    let xs = fmap snd distances
        ls = fmap (T.unpack . unLabel . fst) distances
        (cls, ccs) = unzip
                   . fmap (L.over L._1 (T.unpack . unLabel) . L.over L._2 (fmap toUpper . sRGB24show))
                   . Map.toAscList
                   $ cm

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))
        suppressMessages(library(RColorBrewer))
        suppressMessages(library(plyr))
        df = data.frame(x = xs_hs, l = ls_hs)
        spikeDf = ddply(df, "l", here(summarise), groupSpike = density(x, adjust = b_hs)$x[which.max(density(x, adjust = b_hs)$y)])

        p = ggplot(df, aes(x = x, color = l, fill = l)) +
                geom_density(adjust = b_hs, alpha = 0.1) +
                geom_vline(data = spikeDf, aes(xintercept = groupSpike, color = l), linetype="dashed") +
                xlab("Path distance") +
                ylab("Cell density") +
                scale_color_manual(guide = guide_legend(title = ""), aesthetics = c("color", "fill"), values = setNames(c(ccs_hs, "NA"), c(cls_hs, "#000000"))) +
                theme_cowplot() +
                theme(aspect.ratio = 1)

        suppressMessages(ggsave(p, file = outputPlot_hs, height = 8, width = 12))
    |]

    return ()
