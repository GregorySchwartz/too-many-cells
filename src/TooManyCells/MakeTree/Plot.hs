{- TooManyCells.MakeTree.Plot
Gregory W. Schwartz

Collects the functions pertaining to plotting the clusterings.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module TooManyCells.MakeTree.Plot
    ( plotClustersR
    , plotLabelClustersR
    , plotClumpinessHeatmapR
    , plotRankedModularityR
    ) where

-- Remote
import BirchBeer.Types
import Control.Monad (forM, mapM, join)
import Control.Monad.State (State (..))
import Data.Colour (AffineSpace (..), withOpacity)
import Data.Colour.Names (black)
import Data.Colour.Palette.BrewerSet (brewerSet, ColorCat(..), Kolor)
import Data.Colour.SRGB (RGB (..), toSRGB, sRGB24show)
import Data.Function (on)
import Data.List (nub, sort, sortBy, foldl1', transpose, unzip3, unzip4, genericLength)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Tree (Tree (..), flatten)
import Data.Tuple (swap)
import Diagrams.Backend.Cairo
-- import Diagrams.Dendrogram (dendrogramCustom, Width(..))
import Diagrams.Prelude
import Graphics.SVGFonts
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend)
import Plots
import Plots.Axis.ColourBar
import Plots.Legend
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Colour.Palette.BrewerSet as Brewer
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Diagrams.TwoD.GraphViz as G hiding (mkGraph)
import qualified Numeric.LinearAlgebra as H

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Get the projections and label for a cell.
getProjections :: ProjectionMap
               -> (CellInfo, Cluster)
               -> Maybe (Double, Double, String)
getProjections (ProjectionMap pm) (CellInfo { _barcode = !b }, Cluster !c) = do
    p <- Map.lookup b pm
    return $ (unX . fst $ p, unY . snd $ p, show c)

-- | Get the projections and label with colors for a cell.
getProjectionsColor :: LabelMap
                    -> ItemColorMap
                    -> ProjectionMap
                    -> (CellInfo, Cluster)
                    -> Maybe (Double, Double, String, String)
getProjectionsColor
  (LabelMap lm)
  (ItemColorMap icm)
  (ProjectionMap pm)
  (cell@(CellInfo { _barcode = !b }), Cluster !c) = do
  p <- Map.lookup b pm

  let barcodeToColor x = sRGB24show $ Map.findWithDefault black x icm
      barcodeToLabel x = Map.findWithDefault (Label "") x lm
      l = T.unpack . unLabel . barcodeToLabel . getId $ cell
      c = barcodeToColor . getId $ cell

  return $ (unX . fst $ p, unY . snd $ p, l, c)

-- -- | Plot clusters on a 2D axis.
-- plotClusters :: [(CellInfo, Cluster)] -> Axis B V2 Double
-- plotClusters vs = r2Axis &~ do

--     forM vs $ \(CellInfo { _projection = (X !x, Y !y)}, Cluster c) ->
--         scatterPlot [(x, y)] $ do
--             let color = (cycle colours2) !! c
--             plotMarker .= circle 1 # fc color # lwO 1 # lc color

--     hideGridLines

-- | Plot clusters.
plotClustersR :: String -> ProjectionMap -> [(CellInfo, [Cluster])] -> R s ()
plotClustersR outputPlot pm clusterList = do
    let clusterListOrdered =
            sortBy (compare `on` snd) . fmap (L.over L._2 head) $ clusterList -- The first element in the list is the main cluster.
        (xs, ys, cs) =
          unzip3 . catMaybes . fmap (getProjections pm) $ clusterListOrdered
    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))
        df = data.frame(x = xs_hs, y = ys_hs, c = cs_hs)
        df$c = factor(df$c, unique(df$c))
        p = ggplot(df, aes(x = x, y = y, color = factor(c))) +
                geom_point() +
                xlab("Projection 1") +
                ylab("Projection 2") +
                scale_color_discrete(guide = guide_legend(title = "Cluster", ncol = 3)) +
                theme(aspect.ratio = 1)

        suppressMessages(ggsave(p, file = outputPlot_hs))
    |]

    return ()

-- | Plot clusters.
plotLabelClustersR :: String
                   -> ProjectionMap
                   -> LabelMap
                   -> ItemColorMap
                   -> [(CellInfo, [Cluster])]
                   -> R s ()
plotLabelClustersR outputPlot pm lm icm clusterList = do
    let clusterListOrdered =
            sortBy (compare `on` snd) . fmap (L.over L._2 head) $ clusterList -- The first element in the list is the main cluster.
        (xs, ys, ls, cs) = unzip4
                         . catMaybes
                         . fmap (getProjectionsColor lm icm pm)
                         $ clusterListOrdered
    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))
        df = data.frame(x = xs_hs, y = ys_hs, l = ls_hs, c = cs_hs)
        colorMap = toupper(as.character(df$c))
        names(colorMap) = df$l
        # colorMap = colorMap[!duplicated(colorMap)]
        p = ggplot(df, aes(x = x, y = y, color = l)) +
                geom_point() +
                xlab("Projection 1") +
                ylab("Projection 2") +
                scale_color_manual( guide = guide_legend(title = "Label")
                                  , values = colorMap
                                  ) +
                theme(aspect.ratio = 1)

        suppressMessages(ggsave(p, file = outputPlot_hs))
    |]

    return ()

-- | Plot clusters using outputs from R.
plotClustersOnlyR :: String -> RMatObsRow s -> R.SomeSEXP s -> R s ()
plotClustersOnlyR outputPlot (RMatObsRow mat) clustering = do
    -- Plot hierarchy.
    [r| pdf(paste0(outputPlot_hs, "_hierarchy.pdf", sep = ""))
        plot(clustering_hs$hc)
        dev.off()
    |]

    -- Plot flat hierarchy.
    -- [r| pdf(paste0(outputPlot_hs, "_flat_hierarchy.pdf", sep = ""))
    --     plot(clustering_hs)
    --     dev.off()
    -- |]

    -- Plot clustering.
    [r| colors = rainbow(length(unique(clustering_hs$cluster)))
        names(colors) = unique(clustering_hs$cluster)

        pdf(paste0(outputPlot_hs, "_pca.pdf", sep = ""))

        plot( mat_hs[,c(1,2)]
            , col=clustering_hs$cluster+1
            , pch=ifelse(clustering_hs$cluster == 0, 8, 1) # Mark noise as star
            , cex=ifelse(clustering_hs$cluster == 0, 0.5, 0.75) # Decrease size of noise
            , xlab=NA
            , ylab=NA
            )
        colors = sapply(1:length(clustering_hs$cluster)
                       , function(i) adjustcolor(palette()[(clustering_hs$cluster+1)[i]], alpha.f = clustering_hs$membership_prob[i])
                       )
        points(mat_hs, col=colors, pch=20)

        dev.off()
    |]

    -- [r| library(tsne)

    --     colors = rainbow(length(unique(clustering_hs$cluster)))
    --     names(colors) = unique(clustering_hs$cluster)

    --     tsneMat = tsne(mat_hs, perplexity=50)

    --     pdf(paste0(outputPlot_hs, "_tsne.pdf", sep = ""))

    --     plot(tsneMat
    --         , col=clustering_hs$cluster+1
    --         , pch=ifelse(clustering_hs$cluster == 0, 8, 1) # Mark noise as star
    --         , cex=ifelse(clustering_hs$cluster == 0, 0.5, 0.75) # Decrease size of noise
    --         , xlab=NA
    --         , ylab=NA
    --         )
    --     colors = sapply(1:length(clustering_hs$cluster)
    --                    , function(i) adjustcolor(palette()[(clustering_hs$cluster+1)[i]], alpha.f = clustering_hs$membership_prob[i])
    --                    )
    --     points(tsneMat, col=colors, pch=20)

    --     dev.off()
    -- |]

    return ()

-- | Plot ranked modularity.
plotRankedModularityR :: String
                      -> Tree (TreeNode a)
                      -> R s ()
plotRankedModularityR outputPlot tree = do
    let ms = sort
           . catMaybes
           . fmap (L.view BirchBeer.Types.distance)
           . flatten
           $ tree
        rs = [1..genericLength ms] :: [Double]

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))
        df = data.frame(x = rs_hs, y = ms_hs)
        p = ggplot(df, aes(x = x, y = y)) +
                geom_line() +
                # geom_vline(xintercept = cutoff_hs, linetype = "dashed", color = "red") +
                xlab("Ranking") +
                ylab("Modularity") +
                theme(aspect.ratio = 1)

        suppressMessages(ggsave(p, file = outputPlot_hs))
    |]

    return ()

-- | Plot heatmap in R.
plotClumpinessHeatmapR :: String -> [(T.Text, T.Text, Double)] -> Either String (R s ())
plotClumpinessHeatmapR _ [] =
  Left "\nNo labels for leaves based on chosen clumpiness method, skipping clumpiness plot."
plotClumpinessHeatmapR outputPlot cs = Right $ do
    let xs = fmap (T.unpack . L.view L._1) cs
        ys = fmap (T.unpack . L.view L._2) cs
        vs = fmap (L.view L._3) cs

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))
        library(scales)
        library(reshape2)

        df = data.frame(x = xs_hs, y = ys_hs, v = vs_hs)

        # Convert to wide to cluster.
        dfWide = dcast(df, x ~ y, value.var = c("v"))

        # Cluster and get order of labels.
        if (length(unique(df$x)) <= 2) {
            ord = c(1:length(unique(df$x)))
        } else {
            ord = suppressWarnings(hclust(dist(dfWide, method = "euclidean"))$order)
        }
        levels = colnames(dfWide)[-1][ord]

        df$x = factor(df$x, levels = levels)
        df$y = factor(df$y, levels = levels)

        # Plot.
        p = ggplot(df, aes(x = x, y = y, fill = v)) +
                geom_tile(color = "white") +
                coord_equal() +
                xlab("") +
                ylab("") +
                scale_fill_gradient2(guide = guide_colorbar(title = "Clumpiness"), midpoint = 0.5, low = muted("blue"), high = muted("red")) +
                theme(axis.text.x = element_text(angle = 315, hjust = 0))

        suppressMessages(ggsave(p, file = outputPlot_hs))
    |]

    return ()

-- | Plot a heatmap.
-- heatMapAxis :: [[Double]] -> Axis B V2 Double
-- heatMapAxis values = r2Axis &~ do
--     display colourBar
--     axisExtend .= noExtend

--     heatMap values $ heatMapSize .= V2 10 10
