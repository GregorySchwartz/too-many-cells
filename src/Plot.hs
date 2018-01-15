{- Plot
Gregory W. Schwartz

Collects the functions pertaining to plotting the clusterings.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plot
    ( plotClusters
    , plotClustersR
    , plotDendrogram
    , getColorMap
    ) where

-- Remote
import Control.Monad (forM, mapM)
import Data.Colour.Names (black)
import Data.Colour.Palette.BrewerSet (brewerSet, ColorCat(..))
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.Maybe (fromMaybe)
import Diagrams.Backend.Cairo
import Diagrams.Dendrogram (dendrogram, Width(..))
import Diagrams.Prelude
import Graphics.SVGFonts
import Language.R as R
import Language.R.QQ (r)
import Plots
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H

-- Local
import Types
import Utility

-- | Plot clusters on a 2D axis.
plotClusters :: [(CellInfo, Cluster)] -> Axis B V2 Double
plotClusters vs = r2Axis &~ do

    forM vs $ \(CellInfo { projection = (X !x, Y !y)}, Cluster c) ->
        scatterPlot [(x, y)] $ do
            let color = (cycle colours2) !! c
            plotMarker .= circle 1 # fc color # lwO 1 # lc color

    hideGridLines

-- | Plot clusters.
plotClustersR :: String -> [(CellInfo, Cluster)] -> R s ()
plotClustersR outputPlot clusterList = do
    let clusterListOrdered = sortBy (compare `on` snd) clusterList
        xs = fmap (unX . fst . projection . fst) clusterListOrdered
        ys = fmap (unY . snd . projection . fst) clusterListOrdered
        cs = fmap (show . unCluster . snd) clusterListOrdered
        saveFile = outputPlot <> ".pdf"
    [r| library(ggplot2)
        library(cowplot)
        df = data.frame(x = xs_hs, y = ys_hs, c = cs_hs)
        df$c = factor(df$c, unique(df$c))
        p = ggplot(df, aes(x = x, y = y, color = factor(c))) +
                geom_point() +
                xlab("TNSE 1") +
                ylab("TNSE 2") +
                scale_color_discrete(guide = guide_legend(title = "Cluster"))

        ggsave(p, file = saveFile_hs)
    |]

    return ()

-- | Plot clusters using outputs from R.
plotClustersOnlyR :: String -> RMatObsRowImportant s -> R.SomeSEXP s -> R s ()
plotClustersOnlyR outputPlot (RMatObsRowImportant mat) clustering = do
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

-- | Plot a heatmap.
-- heatMapAxis :: [[Double]] -> Axis B V2 Double
-- heatMapAxis values = r2Axis &~ do
--     display colourBar
--     axisExtend .= noExtend

--     heatMap values $ heatMapSize .= V2 10 10

-- | Plot a dendrogram.
plotDendrogram :: Maybe (LabelMap, ColorMap) -> HC.Dendrogram (V.Vector CellInfo) -> Diagram B
plotDendrogram ms dend =
    dendrogram Fixed (dendrogramLeaf ms) dend # lw 0.1 # pad 1.1

-- | How to plot each leaf of the dendrogram.
dendrogramLeaf :: Maybe (LabelMap, ColorMap) -> V.Vector CellInfo -> Diagram B
dendrogramLeaf Nothing leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unCell . barcode . V.head $ leaf) 0.01) # rotateBy (1/4) # alignT # fc black # pad 1.3
        s -> stroke (textSVG (show s) 0.01) # rotateBy (1/4) # alignT # fc black # pad 1.3
dendrogramLeaf (Just (LabelMap lm, ColorMap cm)) leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unCell . barcode . V.head $ leaf) 0.01) # rotateBy (1/4) # alignT #  fc color # lw none # pad 1.3
        s -> stroke (textSVG (show s) 0.01) # rotateBy (1/4) # alignT #  fc color # lw none # pad 1.3
  where
    color = fromMaybe black
          . (=<<) (flip Map.lookup cm . getMostFrequent)
          . mapM (flip Map.lookup lm . barcode)
          . V.toList
          $ leaf

-- | Get the colors of each label.
getColorMap :: LabelMap -> ColorMap
getColorMap = ColorMap
            . Map.fromList
            . flip zip (cycle (brewerSet Set1 9))
            . Set.toList
            . Set.fromList
            . Map.elems
            . unLabelMap
