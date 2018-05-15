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
import Language.R as R
import Language.R.QQ (r)
import qualified Data.Graph.Inductive as G
import qualified Data.Text as T

-- Local
import TooManyCells.Paths.Types

-- | Plot clusters.
plotPathDistanceR :: String -> Bandwidth -> [(Label, Double)] -> R s ()
plotPathDistanceR outputPlot (Bandwidth b) distances = do
    let xs = fmap snd distances
        ls = fmap (T.unpack . unLabel . fst) distances

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))
        suppressMessages(library(RColorBrewer))
        df = data.frame(x = xs_hs, l = ls_hs)

        p = ggplot(df, aes(x = x, color = l, fill = l)) +
                geom_density(adjust = b_hs, alpha = 0.1) +
                xlab("Path distance") +
                ylab("Cell density") +
                scale_color_discrete(guide = guide_legend(title = "")) +
                guides(fill = FALSE) +
                theme(aspect.ratio = 1)

        suppressMessages(ggsave(p, file = outputPlot_hs))
    |]

    return ()
