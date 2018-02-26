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
    ( plotClusters
    , plotClustersR
    , plotDendrogram
    , plotGraph
    , getLabelColorMap
    , labelToItemColorMap
    , getItemColorMapExpression
    , plotLabelLegend
    , plotExpressionLegend
    ) where

-- Remote
import Control.Monad (forM, mapM, join)
import Control.Monad.State (State (..))
import Data.Colour (AffineSpace (..))
import Data.Colour.Names (black)
import Data.Colour.Palette.BrewerSet (brewerSet, ColorCat(..), Kolor)
import Data.Colour.SRGB (RGB (..), toSRGB)
import Data.Function (on)
import Data.List (nub, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Diagrams.Backend.Cairo
import Diagrams.Dendrogram (dendrogramCustom, Width(..))
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
import TooManyCells.MakeTree.Utility
import TooManyCells.Matrix.Types

-- | Plot clusters on a 2D axis.
plotClusters :: [(CellInfo, Cluster)] -> Axis B V2 Double
plotClusters vs = r2Axis &~ do

    forM vs $ \(CellInfo { projection = (X !x, Y !y)}, Cluster c) ->
        scatterPlot [(x, y)] $ do
            let color = (cycle colours2) !! c
            plotMarker .= circle 1 # fc color # lwO 1 # lc color

    hideGridLines

-- | Plot clusters.
plotClustersR :: String -> [(CellInfo, [Cluster])] -> R s ()
plotClustersR outputPlot clusterList = do
    let clusterListOrdered =
            sortBy (compare `on` snd) . fmap (L.over L._2 head) $ clusterList -- The first element in the list is the main cluster.
        xs = fmap (unX . fst . projection . fst) clusterListOrdered
        ys = fmap (unY . snd . projection . fst) clusterListOrdered
        cs = fmap (show . unCluster . snd) clusterListOrdered
    [r| suppressMessages(library(ggplot2))
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
    :: (TreeItem a)
    => Maybe (Diagram B)
    -> DrawLeaf
    -> Maybe ItemColorMap
    -> HC.Dendrogram (V.Vector a)
    -> Diagram B
plotDendrogram Nothing drawLeaf cm dend =
    pad 1
        . center
        . dendrogramCustom
            Variable
            (whichLeaf drawLeaf $ cm)
            (dendrogramPathLabel drawLeaf cm)
            ((\tree items -> lw 0.3 . scaleToY (3 * height items) $ tree), curry snd)
        $ dend
  where
    whichLeaf DrawText     = dendrogramLeafLabel
    whichLeaf (DrawItem _) = dendrogramLeafItem
plotDendrogram (Just legend) drawLeaf cm dend =
    pad 1
        . hsep 1
        $   [ alignT
            . center
            . dendrogramCustom
                Variable
                (whichLeaf drawLeaf $ cm)
                (dendrogramPathLabel drawLeaf cm)
                ((\tree items -> lw 0.3 . scaleToY (2 * height items) $ tree), curry snd)
            $ dend
            , pad 2 . alignT . lw 0.3 center . scaleUToY (height tree / 6) $ legend
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
    whichLeaf (DrawItem _) = dendrogramLeafItem

-- | Get the most frequent color of a dendrogram.
getMostFrequentColorDend
    :: (TreeItem a)
    => ItemColorMap -> HC.Dendrogram (V.Vector a) -> Kolor
getMostFrequentColorDend (ItemColorMap cm) (HC.Leaf leaf) =
    getMostFrequent
        . fmap (flip (Map.findWithDefault black) cm . getId)
        . V.toList
        $ leaf
getMostFrequentColorDend (ItemColorMap cm) dend =
    getMostFrequent
        . concatMap ( fmap (flip (Map.findWithDefault black) cm . getId)
                    . V.toList
                    )
        . getClusterItemsDend
        $ dend

-- | Get the most frequent color of a list.
getMostFrequentColorList
    :: (TreeItem a)
    => Maybe ItemColorMap -> [a] -> Kolor
getMostFrequentColorList Nothing                  = const black
getMostFrequentColorList (Just (ItemColorMap cm)) =
    getMostFrequent . fmap (flip (Map.findWithDefault black) cm . getId)

-- | Get the fraction of each element in a list.
getEachFractionColorList :: (TreeItem a)
                         => Maybe ItemColorMap
                         -> [a]
                         -> [(Double, Kolor)]
getEachFractionColorList Nothing                  = const [(1, black)]
getEachFractionColorList (Just (ItemColorMap cm)) =
    fmap swap
        . getFractions
        . fmap (flip (Map.findWithDefault black) cm . getId)

-- | Get the a color from a fractional list of colors.
blendColors :: [(Double, Kolor)] -> Colour Double
blendColors []     = black
blendColors (x:xs) = affineCombo xs . snd $ x

-- | Get the the blended color from a graph node.
getBlendedColor :: (TreeItem a) => Maybe ItemColorMap -> [a] -> Kolor
getBlendedColor cm = blendColors . getEachFractionColorList cm

-- | How to draw the path at each junction.
dendrogramPathLabel
    :: (TreeItem a)
    => DrawLeaf
    -> Maybe ItemColorMap
    -> HC.Dendrogram (V.Vector a)
    -> Diagram B
    -> Diagram B
dendrogramPathLabel _ Nothing _ d                       = d
dendrogramPathLabel (DrawItem (DrawExpression _)) _ _ d = d
dendrogramPathLabel _ (Just cm) dend d                  = lc color d
  where
    color = getMostFrequentColorDend cm dend

-- | Plot the leaf of a dendrogram as a text label.
dendrogramLeafLabel
    :: (TreeItem a)
    => Maybe ItemColorMap -> V.Vector a -> Diagram B
dendrogramLeafLabel Nothing leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unId . getId . V.head $ leaf) 1) # rotateBy (1/4) # fc black # centerX # pad 1.3 # alignT
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc black # centerX # pad 1.3 # alignT
dendrogramLeafLabel cm leaf =
    case V.length leaf of
        1 -> stroke (textSVG (T.unpack . unId . getId . V.head $ leaf) 1) # rotateBy (1/4) # fc black # lw none # centerX # pad 1.3 # alignT
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc color # lw none # centerX # pad 1.3 # alignT
  where
    color = getMostFrequentColorList cm . V.toList $ leaf

-- | Plot the leaf of a dendrogram as a collection of items.
dendrogramLeafItem
    :: (TreeItem a)
    => Maybe ItemColorMap -> V.Vector a -> Diagram B
dendrogramLeafItem Nothing leaf = getItem black # centerX # pad 1.3 # alignT
dendrogramLeafItem (Just (ItemColorMap cm)) leaf =
    (vcat . fmap hcat . Split.chunksOf 10 $ items) # centerX # pad 1.3 # alignT
  where
    items  = fmap getItem colors
    colors = sort
           . fmap (flip (Map.findWithDefault black) cm . getId)
           . V.toList
           $ leaf

-- | Plot the node of a graph as a text label.
drawGraphLabel
    :: (TreeItem a)
    => Maybe ItemColorMap -> Seq.Seq a -> Diagram B
drawGraphLabel Nothing items =
    case Seq.length items of
        1 -> stroke (textSVG (T.unpack . unId . getId . flip Seq.index 0 $ items) 1) # rotateBy (1/4) # fc black # center
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc black # center
drawGraphLabel cm items =
    case Seq.length items of
        1 -> stroke (textSVG (T.unpack . unId . getId . flip Seq.index 0 $ items) 1) # rotateBy (1/4) # fc black # lw none # center
        s -> stroke (textSVG (show s) 1) # rotateBy (1/4) # fc color # lw none # center
  where
    color = getMostFrequentColorList cm . F.toList $ items

-- | Plot the node of a graph as a collection of items.
drawGraphItem :: (TreeItem a) => Maybe ItemColorMap -> Seq.Seq a -> Diagram B
drawGraphItem Nothing _ = getItem black # centerX # pad 1.3 # center
drawGraphItem (Just (ItemColorMap cm)) node =
    (vcat . fmap hcat . Split.chunksOf boxSize $ items) # center
  where
    boxSize = floor . sqrt . fromIntegral . Seq.length $ node
    items  = fmap getItem colors
    colors = sort
           . fmap (flip (Map.findWithDefault black) cm . getId)
           . F.toList
           $ node

-- | Plot a pie chart of items.
drawPieItem
    :: (TreeItem a)
    => DrawPie -> Maybe ItemColorMap -> Seq.Seq a -> Diagram B
drawPieItem _ Nothing _ = mempty
drawPieItem drawPie (Just (ItemColorMap cm)) node =
    renderAxis $ polarAxis &~ do
        let colorWedge :: (Kolor, Double) -> State (Plot (Wedge Double) b) ()
            colorWedge colorPair = do
                plotColor .= fst colorPair
                areaStyle . _lw .= none
            colorPairs = sortBy (compare `on` snd)
                       . getFractions
                       . fmap (flip (Map.findWithDefault black) cm . getId)
                       . F.toList
                       $ node

        piePlot colorPairs snd $ onWedges colorWedge
        case drawPie of
            PieChart -> return ()
            _        -> wedgeInnerRadius .= 0.9
        hide (axes . traversed)

-- | Draw a single item.
getItem :: Kolor -> Diagram B
getItem color = circle 1 # lc black # fc color # lw 0.1

-- | Get the legend for labels.
-- plotLabelLegend :: LabelColorMap -> Diagram B
-- plotLabelLegend = vsep 0.10
--                 . fmap plotLabel
--                 . Map.toAscList
--                 . unLabelColorMap
--   where
--     plotLabel (!l, !c) =
--         stroke (textSVG (T.unpack . unLabel $ l) 1) # fc c # lw none # alignL

plotLabelLegend :: LabelColorMap -> Diagram B
plotLabelLegend = flip (drawLegend emptyBox) legendOpts
                . fmap plotLabel
                . Map.toAscList
                . unLabelColorMap
  where
    legendOpts :: Legend B Double
    legendOpts = over legendTextStyle (const (mempty # font "Arial" # fontSize (local size)))
               . over legendStyle (lw none)
               $ def
    plotLabel :: (Label, Colour Double) -> (Diagram B, String)
    plotLabel (!l, !c) =
        ( circle size # lc black # fc c # lw (local $ 0.1 * size)
        , T.unpack . unLabel $ l
        )
    size :: Double
    size = (/ 2.5) $ (def :: Legend B Double) ^. legendSpacing

-- | Get the legend for expression. Bar from
-- https://archives.haskell.org/projects.haskell.org/diagrams/blog/2013-12-03-Palette1.html
plotExpressionLegend :: Gene -> SingleCells MatObsRow -> Diagram B
plotExpressionLegend g sc =
    renderColourBar cbOpts cm (fromMaybe 0 minVal, fromMaybe 0 maxVal) 100
  where
    cbOpts :: ColourBar B Double
    cbOpts = over tickLabelStyle (font "Arial")
           . set visible True
           $ defColourBar
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
getLabelColorMap9 :: LabelMap -> LabelColorMap
getLabelColorMap9 (LabelMap lm) =
    LabelColorMap
        . Map.fromList
        . flip zip (cycle (brewerSet Set1 9))
        $ labels
  where
    labels = Set.toAscList . Set.fromList . Map.elems $ lm

-- | Get the colors of each item from a label.
labelToItemColorMap :: LabelColorMap -> LabelMap -> ItemColorMap
labelToItemColorMap (LabelColorMap lm) =
    ItemColorMap . Map.map (\x -> Map.findWithDefault black x lm) . unLabelMap

-- | Get the colors from a list of expressions.
getExpressionColor :: [Double] -> [Kolor]
getExpressionColor =
    fmap (\x -> sRGB (1 - x) (1 - x) (1 - x))
        . Fold.fold
            ( (\xs m -> fmap (/ (fromMaybe (error "Expression does not exist.") m)) xs)
                    <$> Fold.list
                    <*> Fold.maximum
            )

-- | Get the colors of each item, where the color is determined by expression.
getItemColorMapExpression :: Gene -> SingleCells MatObsRow -> ItemColorMap
getItemColorMapExpression g sc =
    ItemColorMap
        . Map.fromList
        . zip (fmap (Id . unCell) . V.toList . rowNames $ sc)
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

-- | Get the maximum cluster size from a graph.
maxClusterSize :: G.Gr (G.Node, Maybe (Seq.Seq a)) e -> Int
maxClusterSize = maximum
               . fmap (maybe 0 Seq.length . snd)
               . F.toList
               . flip getGraphLeaves 0

-- | Get the collection of items in a leaf.
getGraphLeafItems :: ClusterGraph a -> G.Node -> Seq.Seq a
getGraphLeafItems (ClusterGraph gr) =
    join
        . fmap ( fromMaybe (error "No items in leaf.")
            . snd
            )
        . getGraphLeaves gr

-- | Get the size of a leaf for graph plotting. Determines the size based on the
-- diameter of each node (36) and the maximum size of a cluster.
getScaledLeafSize :: Int -> Seq.Seq a -> Double
getScaledLeafSize maxLen =
    isTo (fromIntegral . floor . sqrt . fromIntegral $ maxLen) 36
        . fromIntegral
        . floor
        . sqrt
        . fromIntegral
        . Seq.length

-- | Get the color of a path or node in a graph.
getGraphColor :: (TreeItem a) => Maybe ItemColorMap -> Seq.Seq a -> Kolor
getGraphColor cm = getBlendedColor cm . F.toList

-- | Get the trail making the edge.
getEdgeTrail
    :: P2 Double -> P2 Double -> Double -> Double -> Trail' Loop V2 Double
getEdgeTrail p1 p2 h1 h2 = fromVertices
                                  [ perpendicular p1 p2 d1
                                  , perpendicular p1 p2 (-d1)
                                  , perpendicular p2 p1 d2
                                  , perpendicular p2 p1 (-d2)
                                  ]
                               # closeLine
  where
    d1 = h1 / 2
    d2 = h2 / 2

-- | Distance d away from p on perpendicular line from p to q, by byorgey.
perpendicular :: P2 Double -> P2 Double -> Double -> P2 Double
perpendicular p q d = p .+^ ((q .-. p) # perp # normalize # scale d)

-- | Get the linear gradient of an edge.
getEdgeGrad :: P2 Double
            -> Kolor
            -> P2 Double
            -> Kolor
            -> Double
            -> Double
            -> Texture Double
getEdgeGrad p1 c1 p2 c2 h1 h2 =
    mkLinearGradient (mkStops [(c1, 0, 1), (c2, 1, 1)]) start end GradPad
  where
    start  = perpendicular startPoint ((0 ^& 0) :: P2 Double) (- d1)  -- Start gradient after node.
    end  = endPoint .+^ ((startPoint .-. endPoint) # normalize # scale d2) -- End gradient before node.
    startPoint = p1 ^-^ origin
    endPoint    = p2 ^-^ origin
    origin = perpendicular p1 p2 d1
    d1     = h1 / 2
    d2     = h2 / 2

-- | The function to draw a path connection two nodes in a graph.
drawGraphPath
    :: (TreeItem a)
    => DrawConfig
    -> Maybe ItemColorMap
    -> ClusterGraph a
    -> (G.Node, Maybe (Seq.Seq a))
    -> P2 Double
    -> (G.Node, Maybe (Seq.Seq a))
    -> P2 Double
    -> HC.Distance
    -> Path V2 Double
    -> Diagram B
drawGraphPath opts cm gr (n1, _) p1 (n2, _) p2 _ _ =
    strokeLoop trail
        # lw none
        # fillTexture gradient
        # flip place (perpendicular p1 p2 d1)
  where
    gradient = getEdgeGrad p1 c1 p2 c2 (height draw1) (height draw2)
    d1           = height draw1 / 2
    draw1        = drawNode n1 p1
    draw2        = drawNode n2 p2
    c1           = getGraphColor cm $ getGraphLeafItems gr n1
    c2           = getGraphColor cm $ getGraphLeafItems gr n2
    trail        = getEdgeTrail p1 p2 (height draw1) (height draw2)
    drawNode n p = drawGraphNode opts cm gr (n, Nothing) p -- DrawText is irrelevant here.

-- | Draw the final node of a graph.
drawGraphNode :: (TreeItem a)
              => DrawConfig
              -> Maybe ItemColorMap
              -> ClusterGraph a
              -> (G.Node, Maybe (Seq.Seq a))
              -> P2 Double
              -> Diagram B
drawGraphNode opts@(DrawConfig { _drawLeaf = DrawText }) cm _ (n, Just items) pos =
    (textDia dnn <> drawGraphLabel cm items)
        # scaleUToY 36
        # moveTo pos
  where
    textDia True  = text (show n) # fc black
    textDia False = mempty
    dnn = unDrawNodeNumber . _drawNodeNumber $ opts
drawGraphNode opts@(DrawConfig { _drawLeaf = (DrawItem _) }) cm gr (n, Just items) pos =
    ( pieDia (_drawPie opts)
   <> itemsDia
   <> background (_drawPie opts)
    )
        # moveTo pos
  where
    background PieNone = roundedRect (width itemsDia) (height itemsDia) 1 # fc white # lw none # scaleUToY (scaleVal * 1.1)
    background _       = circle 1 # fc white # lw none # scaleUToY scaleVal
    itemsDia              = getItemsDia $ _drawPie opts
    getItemsDia PieNone   = scaleUToY scaleVal
                          $ textDia dnn <> drawGraphItem cm items
    getItemsDia PieChart  = mempty
    getItemsDia _         = scaleUToY (0.50 * scaleVal)
                          $ textDia dnn <> drawGraphItem cm items
    pieDia PieNone     = mempty
    pieDia x           = scaleUToY scaleVal $ drawPieItem x cm items
    scaleVal = if unDrawNoScaleNodesFlag . _drawNoScaleNodesFlag $ opts
                then unDrawMaxNodeSize . _drawMaxNodeSize $ opts
                else getScaledLeafSize maxClusterSize' items
    maxClusterSize' = maxClusterSize . unClusterGraph $ gr
    textDia True  = text (show n) # fc black
    textDia False = mempty
    dnn = unDrawNodeNumber . _drawNodeNumber $ opts
drawGraphNode opts cm gr (n, Nothing) pos               =
    (textDia dnn <> (circle 1 # fc color # rootDiffer n))
        # scaleUToY (getNodeSize (_drawNoScaleNodesFlag opts) items)
        # moveTo pos
  where
    dnn = unDrawNodeNumber . _drawNodeNumber $ opts
    textDia True  = text (show n) # fc black
    textDia False = mempty
    rootDiffer 0 = lw none
    rootDiffer n = lw none
    color = getGraphColor cm items
    items = getGraphLeafItems gr n
    getNodeSize :: DrawNoScaleNodesFlag -> Seq.Seq a -> Double
    getNodeSize (DrawNoScaleNodesFlag False) =
        isTo totalItems (unDrawMaxNodeSize . _drawMaxNodeSize $ opts)
            . fromIntegral
            . Seq.length
    getNodeSize (DrawNoScaleNodesFlag True) =
        const (unDrawMaxNodeSize . _drawMaxNodeSize $ opts)
    totalItems = fromIntegral . Seq.length . getGraphLeafItems gr $ 0

-- | Plot a graph rather than a traditional tree. Uses only info in leaves
-- rather than a tree which stores all leaves.
plotGraph
    :: (Ord a, TreeItem a)
    => Maybe (Diagram B)
    -> DrawConfig
    -> Maybe ItemColorMap
    -> ClusterGraph a
    -> IO (Diagram B)
plotGraph legend opts cm (ClusterGraph gr) = do
    let numClusters :: Double
        numClusters = fromIntegral . Seq.length $ getGraphLeaves gr 0
        params :: (TreeItem a) => G.GraphvizParams Int (G.Node, Maybe (Seq.Seq a)) HC.Distance () (G.Node, Maybe (Seq.Seq a))
        params = G.defaultDiaParams
            { G.fmtEdge = (\(_, _, w) -> [G.Len w])
            , G.globalAttributes = [G.GraphAttrs { G.attrs = [G.Sep $ G.DVal 36] }]
            }

    layout <- G.layoutGraph' params G.TwoPi gr

    let treeDia =
            G.drawGraph'
                (drawGraphNode opts cm (ClusterGraph gr))
                (drawGraphPath opts cm (ClusterGraph gr))
                layout
        dia = case legend of
                Nothing  -> pad 1 . center $ treeDia
                (Just l) ->
                    pad 1
                        . hsep
                            1
                        $   [ alignY 1.5 . lw 0.3 . center . scaleUToX (width treeDia / 8) $ l
                            , alignT . center $ treeDia
                            ]

    return dia
