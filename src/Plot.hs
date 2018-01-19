{- Plot
Gregory W. Schwartz

Collects the functions pertaining to plotting the clusterings.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Plot
    ( plotClusters
    , plotClustersR
    , plotDendrogram
    , getLabelColorMap
    , labelToCellColorMap
    , getCellColorMapExpression
    , plotLabelLegend
    , plotExpressionLegend
    ) where

-- Remote
import Control.Monad (forM, mapM)
import Data.Colour.Names (black)
import Data.Colour.Palette.BrewerSet (brewerSet, ColorCat(..), Kolor)
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.Function (on)
import Data.List (nub, sort, sortBy)
import Data.Maybe (fromMaybe)
import Diagrams.Backend.Cairo
import Diagrams.Dendrogram (dendrogramCustom, Width(..))
import Diagrams.Prelude
import Graphics.SVGFonts
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend)
import Plots
import Plots.Axis.ColourBar
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.List.Split as Split
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
    [r| library(ggplot2)
        library(cowplot)
        df = data.frame(x = xs_hs, y = ys_hs, c = cs_hs)
        df$c = factor(df$c, unique(df$c))
        p = ggplot(df, aes(x = x, y = y, color = factor(c))) +
                geom_point() +
                xlab("TNSE 1") +
                ylab("TNSE 2") +
                scale_color_discrete(guide = guide_legend(title = "Cluster"))

        ggsave(p, file = outputPlot_hs)
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
plotDendrogram
    :: Maybe (Diagram B)
    -> DrawLeaf
    -> Maybe CellColorMap
    -> HC.Dendrogram (V.Vector CellInfo)
    -> Diagram B
plotDendrogram Nothing drawLeaf cm dend =
    pad 1
        . center
        . dendrogramCustom
            Variable
            (whichLeaf drawLeaf $ cm)
            (dendrogramPathLabel drawLeaf cm)
            ((\tree items -> lw 0.1 . scaleToY (3 * height items) $ tree), curry snd)
        $ dend
  where
    whichLeaf DrawText     = dendrogramLeafLabel
    whichLeaf (DrawCell _) = dendrogramLeafCell
plotDendrogram (Just legend) drawLeaf cm dend =
    pad 1
        . hsep 1
        $   [ alignT
            . center
            . dendrogramCustom
                Variable
                (whichLeaf drawLeaf $ cm)
                (dendrogramPathLabel drawLeaf cm)
                ((\tree items -> lw 0.1 . scaleToY (2 * height items) $ tree), curry snd)
            $ dend
            , alignT . center . scaleUToY (height tree / 6) $ legend
            ]
  where
    tree = alignT
         . center
         . dendrogramCustom
             Variable
             (whichLeaf drawLeaf $ cm)
             (dendrogramPathLabel drawLeaf cm)
             ((\tree items -> lw 0.1 . scaleToY (3 * height items) $ tree), curry snd)
         $ dend
    whichLeaf DrawText     = dendrogramLeafLabel
    whichLeaf (DrawCell _) = dendrogramLeafCell

-- | How to draw the path at each junction.
dendrogramPathLabel
    :: DrawLeaf
    -> Maybe CellColorMap
    -> HC.Dendrogram (V.Vector CellInfo)
    -> Diagram B
    -> Diagram B
dendrogramPathLabel _ Nothing _ d                       = d
dendrogramPathLabel (DrawCell (DrawExpression _)) _ _ d = d
dendrogramPathLabel _ (Just (CellColorMap cm)) dend d   =
    lc color d
  where
    color = getMostFrequent
          . concatMap ( fmap (flip (Map.findWithDefault black) cm . barcode)
                      . V.toList
                      )
          . getClusterItemsDend
          $ dend

-- | Plot the leaf of a dendrogram as a text label.
dendrogramLeafLabel :: Maybe CellColorMap -> V.Vector CellInfo -> Diagram B
dendrogramLeafLabel Nothing leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unCell . barcode . V.head $ leaf) 1) # rotateBy (1/4) # fc black # centerX # pad 1.3 # alignT
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc black # centerX # pad 1.3 # alignT
dendrogramLeafLabel (Just (CellColorMap cm)) leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unCell . barcode . V.head $ leaf) 1) # rotateBy (1/4) # fc black # lw none # centerX # pad 1.3 # alignT
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc color # lw none # centerX # pad 1.3 # alignT
  where
    color = getMostFrequent
          . fmap (flip (Map.findWithDefault black) cm . barcode)
          . V.toList
          $ leaf

-- | Plot the leaf of a dendrogram as a collection of cells.
dendrogramLeafCell :: Maybe CellColorMap -> V.Vector CellInfo -> Diagram B
dendrogramLeafCell Nothing leaf = getCell black # centerX # pad 1.3 # alignT
dendrogramLeafCell (Just (CellColorMap cm)) leaf =
    (vcat . fmap hcat . Split.chunksOf 10 $ cells) # centerX # pad 1.3 # alignT
  where
    cells  = fmap getCell colors
    colors = sort
           . fmap (flip (Map.findWithDefault black) cm . barcode)
           . V.toList
           $ leaf

-- | Draw a single cell.
getCell :: Kolor -> Diagram B
getCell color = circle 1 # lc black # fc color # lw 0.1

-- | Get the legend for labels.
plotLabelLegend :: LabelColorMap -> Diagram B
plotLabelLegend = vsep 0.10
                . fmap plotLabel
                . Map.toAscList
                . unLabelColorMap
  where
    plotLabel (!l, !c) =
        stroke (textSVG (T.unpack . unLabel $ l) 1) # fc c # lw none # alignL

-- | Get the legend for expression. Bar from
-- https://archives.haskell.org/projects.haskell.org/diagrams/blog/2013-12-03-Palette1.html
plotExpressionLegend :: Gene -> SingleCells MatObsRow -> Diagram B
plotExpressionLegend g sc =
    renderColourBar
        (L.set visible True defColourBar)
        cm
        (fromMaybe 0 minVal, fromMaybe 0 maxVal)
        1
  where
    cm =
        colourMap . zip [1..] . fmap (\x -> sRGB 1 (1 - x) (1 - x)) $ [1,0.9..0]
    (minVal, maxVal) = Fold.fold ((,) <$> Fold.minimum <*> Fold.maximum)
                     . flip S.extractCol col
                     . unMatObsRow
                     . matrix
                     $ sc
    col = fromMaybe (error "Expression does not exist.")
        . V.elemIndex g
        . colNames
        $ sc


-- | Get the colors of each label.
getLabelColorMap :: LabelMap -> LabelColorMap
getLabelColorMap = LabelColorMap
                 . Map.fromList
                 . flip zip (cycle (brewerSet Set1 9))
                 . Set.toList
                 . Set.fromList
                 . Map.elems
                 . unLabelMap

-- | Get the colors of each cell from a label.
labelToCellColorMap :: LabelColorMap -> LabelMap -> CellColorMap
labelToCellColorMap (LabelColorMap lm) =
    CellColorMap . Map.map (\x -> Map.findWithDefault black x lm) . unLabelMap

-- | Get the colors from a list of expressions.
getExpressionColor :: [Double] -> [Kolor]
getExpressionColor =
    fmap (\x -> sRGB 1 (1 - x) (1 - x))
        . Fold.fold
            ( (\xs m -> fmap (/ (fromMaybe (error "Expression does not exist.") m)) xs)
                    <$> Fold.list
                    <*> Fold.maximum
            )

-- | Get the colors of each label.
getCellColorMapExpression :: Gene -> SingleCells MatObsRow -> CellColorMap
getCellColorMapExpression g sc =
    CellColorMap
        . Map.fromList
        . zip (V.toList . rowNames $ sc)
        . getExpressionColor
        . S.toDenseListSV
        . flip S.extractCol col
        . unMatObsRow
        . matrix
        $ sc
  where
    col = fromMaybe (error "Expression does not exist.")
        . V.elemIndex g
        . colNames
        $ sc
