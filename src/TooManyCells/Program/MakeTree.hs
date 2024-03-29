{- TooManyCells.Program.MakeTree
Gregory W. Schwartz

MakeTree entrypoint into the program.
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.MakeTree where

-- Remote
import Control.Concurrent
import BirchBeer.ColorMap
import BirchBeer.Interactive
import BirchBeer.Load
import BirchBeer.MainDiagram
import BirchBeer.Plot
import BirchBeer.Types
import BirchBeer.Utility
import Control.Monad (when, unless, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bool (bool)
import Data.Colour.SRGB (sRGB24read)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))
import Language.R as R
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Math.Clustering.Spectral.Sparse (b1ToB2, B1 (..), B2 (..))
import Math.Modularity.Types (Q (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe, readEither)
import TextShow (showt)
import qualified "find-clumpiness" Types as Clump
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Colour.Palette.BrewerSet as D
import qualified Data.Colour.Palette.Harmony as D
import qualified Data.Csv as CSV
import qualified Data.GraphViz as G
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified H.Prelude as H
import qualified Plots as D
import qualified System.Directory as FP
import qualified System.FilePath as FP
import qualified System.ProgressBar as Progress

-- Local
import TooManyCells.File.Types
import TooManyCells.MakeTree.Clumpiness
import TooManyCells.MakeTree.Cluster
import TooManyCells.MakeTree.Load
import TooManyCells.MakeTree.Plot
import TooManyCells.MakeTree.Print
import TooManyCells.MakeTree.Types
import TooManyCells.MakeTree.Utility
import TooManyCells.Matrix.Load
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Program.LoadMatrix
import TooManyCells.Program.Options
import TooManyCells.Program.Utility

makeTreeMain :: Subcommand -> IO ()
makeTreeMain sub@(MakeTreeCommand opts) = H.withEmbeddedR defaultConfig $ do
    let readOrErr err = fromMaybe (error err) . readMaybe
        matrixPaths'      = matrixPath . (loadMatrixOptions :: MakeTree -> LoadMatrixOptions) $ opts
        labelsFile'       =
            fmap LabelFile . (labelsFile :: MakeTree -> Maybe String) $ opts
        prior'            =
            fmap PriorPath . (prior :: MakeTree -> Maybe String) $ opts
        updateTreeRows'   =
          UpdateTreeRowsFlag . not . (noUpdateTreeRows :: MakeTree -> Bool) $ opts
        delimiter'        = Delimiter
                          . (delimiter :: LoadMatrixOptions -> Char)
                          . (loadMatrixOptions :: MakeTree -> LoadMatrixOptions)
                          $ opts
        eigenGroup'       = eigenGroup opts
        dense'            = DenseFlag . dense $ opts
        normalizations'   = getNormalization sub
                          . normalization
                          $ (loadMatrixOptions :: MakeTree -> LoadMatrixOptions) opts
        numEigen'         = fmap NumEigen . numEigen $ opts
        numRuns'          = fmap NumRuns . numRuns $ opts
        minSize'          = fmap MinClusterSize . minSize $ opts
        maxStep'          = fmap MaxStep . maxStep $ opts
        maxProportion'    =
            fmap MaxProportion . maxProportion $ opts
        minDistance'       = fmap MinDistance . minDistance $ opts
        minModularity'     = fmap Q . minModularity $ opts
        minDistanceSearch' = fmap MinDistanceSearch . minDistanceSearch $ opts
        smartCutoff'      = fmap SmartCutoff . smartCutoff $ opts
        elbowCutoff'      =
          fmap ( ElbowCutoff
               . readOrErr "Cannot read --elbow-cutoff."
               )
            . elbowCutoff
            $ opts
        customCut'        = CustomCut . Set.fromList . customCut $ opts
        rootCut'          = fmap RootCut . rootCut $ opts
        dendrogramOutput' = DendrogramFile
                          . fromMaybe "dendrogram.svg"
                          . dendrogramOutput
                          $ opts
        matrixOutput'     = fmap (getMatrixOutputType . (unOutputDirectory output' FP.</>))
                          . matrixOutput
                          $ opts
        labelMapOutputFlag' =
            LabelMapOutputFlag . labelsOutput $ opts
        fragmentsOutputFlag' =
            FragmentsOutputFlag . fragmentsOutput $ opts
        drawLeaf'         =
            maybe
              (maybe DrawText (const (DrawItem DrawLabel)) labelsFile')
              (readOrErr "Cannot read --draw-leaf. If using DrawContinuous, remember to put features in a list: DrawItem (DrawContinuous [\\\"FEATURE\\\"])")
                . drawLeaf
                $ opts
        drawCollection'   = drawCollection opts
        drawMark'         = drawMark opts
        drawNodeNumber'   = DrawNodeNumber . drawNodeNumber $ opts
        drawMaxNodeSize'  = DrawMaxNodeSize . drawMaxNodeSize $ opts
        drawMaxLeafNodeSize' = DrawMaxLeafNodeSize
                             . fromMaybe (unDrawMaxNodeSize drawMaxNodeSize')
                             . drawMaxLeafNodeSize
                             $ opts
        drawNoScaleNodes' =
            DrawNoScaleNodesFlag . drawNoScaleNodes $ opts
        drawLegendSep'    = DrawLegendSep . drawLegendSep $ opts
        drawLegendAllLabels' = DrawLegendAllLabels . drawLegendAllLabels $ opts
        drawPalette' = drawPalette opts
        drawColors'       = fmap ( CustomColors
                                 . fmap sRGB24read
                                 . (\x -> readOrErr "Cannot read --draw-colors." x :: [String])
                                 )
                          . drawColors
                          $ opts
        drawDiscretize' = (=<<) (\x -> either error Just
                                . either
                                    (\ err -> either
                                                (\y -> Left $ finalError err y)
                                                (Right . SegmentColorMap)
                                              (readEither x :: Either String Int)
                                    )
                                    (Right . CustomColorMap . fmap sRGB24read)
                                $ (readEither x :: Either String [String])
                                )
                        . drawDiscretize
                        $ opts
          where
            finalError err x = "Error in draw-discretize: " <> err <> " " <> x
        drawScaleSaturation' =
            fmap DrawScaleSaturation . drawScaleSaturation $ opts
        drawItemLineWeight' = fmap DrawItemLineWeight
                            . drawItemLineWeight
                            $ opts
        drawFont' = fmap DrawFont . drawFont $ opts
        drawBarBounds' = DrawBarBounds . drawBarBounds $ opts
        order'            = Order . (order :: MakeTree -> Double) $ opts
        clumpinessMethod' = clumpinessMethod opts
        projectionFile' = fmap ProjectionFile
                        . (projectionFile :: MakeTree -> Maybe String)
                        $ opts
        output' = OutputDirectory . (output :: MakeTree -> String) $ opts

    pb <- Progress.newProgressBar Progress.defStyle 10 (Progress.Progress 0 11 ())
    -- Increment  progress bar.
    Progress.incProgress pb 1

    -- Load matrix once.
    scRes <- loadAllSSM sub
           $ (loadMatrixOptions :: MakeTree -> LoadMatrixOptions) opts
    let processedSc = fmap fst scRes
        customLabelMap = join . fmap snd $ scRes

    -- Increment  progress bar.
    Progress.incProgress pb 1

    -- Where to place output files.
    FP.createDirectoryIfMissing True . unOutputDirectory $ output'

    -- Increment  progress bar.
    Progress.incProgress pb 1

    --R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        -- For r clustering.
        -- mat         <- scToRMat processedSc
        -- clusterRes  <- hdbscan mat
        -- clusterList <- clustersToClusterList sc clusterRes

        -- For agglomerative clustering.
        --let clusterResults = fmap hClust processedSc

    -- Load previous results or calculate results if first run.
    originalClusterResults <- case prior' of
        Nothing -> do
            (fullCr, _) <-
                  hSpecClust dense' eigenGroup' numEigen' minModularity' numRuns'
                    . extractSc
                    $ processedSc

            return fullCr :: IO ClusterResults
        (Just x) -> do
            let clInput = unPriorPath x FP.</> "cluster_list.json"
                treeInput = unPriorPath x FP.</> "cluster_tree.json"

            -- Strict loading in order to avoid locked file.
            fmap (L.over clusterDend (updateTreeRowBool updateTreeRows' processedSc))
              $ loadClusterResultsFiles clInput treeInput

    projectionMap <- mapM loadProjectionMap projectionFile'

    -- Only used for proximity labels for now.
    let oldGr = treeToGraph
              . updateTreeRowBool updateTreeRows' processedSc
              . _clusterDend
              $ originalClusterResults

    -- Get the label map from either a file or from expression thresholds.
    labelMap <- case drawLeaf' of
                    (DrawItem (DrawThresholdContinuous gs)) ->
                        return
                            . Just
                            . getLabelMapThresholdContinuous
                                (fmap (L.over L._1 Feature) gs)
                            . extractSc
                            $ processedSc
                    (DrawItem (DrawProximity ps)) -> return $ do
                      coordMap <- fmap projectionMapToCoordinateMap projectionMap
                      return $ getLabelMapProximity
                                oldGr
                                coordMap
                                ps
                    _ -> if isJust labelsFile'
                          then mapM (loadLabelData delimiter') labelsFile'
                          else return customLabelMap


    -- Increment  progress bar.
    Progress.incProgress pb 1

    let birchMat = processedSc
        birchSimMat =
            case (not . null . matrixPath . (loadMatrixOptions :: MakeTree -> LoadMatrixOptions) $ opts, drawCollection') of
                (True, CollectionGraph{} )  ->
                    Just
                        . B2Matrix
                        . L.over matrix (MatObsRow . unB2 . b1ToB2 . B1 . unMatObsRow)
                        . extractSc
                        $ processedSc
                _ -> Nothing

    -- Increment  progress bar.
    Progress.incProgress pb 1

    let config :: BirchBeer.Types.Config CellInfo SingleCells
        config = BirchBeer.Types.Config
                    { _birchLabelMap = labelMap
                    , _birchMinSize = minSize'
                    , _birchMaxStep = maxStep'
                    , _birchMaxProportion = maxProportion'
                    , _birchMinDistance = minDistance'
                    , _birchMinDistanceSearch   = minDistanceSearch'
                    , _birchSmartCutoff = smartCutoff'
                    , _birchElbowCutoff = elbowCutoff'
                    , _birchCustomCut   = customCut'
                    , _birchRootCut     = rootCut'
                    , _birchOrder = Just order'
                    , _birchDrawLeaf = drawLeaf'
                    , _birchDrawCollection = drawCollection'
                    , _birchDrawMark = drawMark'
                    , _birchDrawNodeNumber = drawNodeNumber'
                    , _birchDrawMaxNodeSize = drawMaxNodeSize'
                    , _birchDrawMaxLeafNodeSize = drawMaxLeafNodeSize'
                    , _birchDrawNoScaleNodes = drawNoScaleNodes'
                    , _birchDrawLegendSep    = drawLegendSep'
                    , _birchDrawLegendAllLabels = drawLegendAllLabels'
                    , _birchDrawPalette = drawPalette'
                    , _birchDrawColors = drawColors'
                    , _birchDrawDiscretize      = drawDiscretize'
                    , _birchDrawScaleSaturation = drawScaleSaturation'
                    , _birchDrawFont            = drawFont'
                    , _birchDrawItemLineWeight  = drawItemLineWeight'
                    , _birchDrawBarBounds       = drawBarBounds'
                    , _birchTree = _clusterDend originalClusterResults
                    , _birchMat = birchMat
                    , _birchSimMat = birchSimMat
                    }

    (plot, labelColorMap, itemColorMap, markColorMap, tree', gr') <- mainDiagram config

    -- Increment  progress bar.
    Progress.incProgress pb 1

    -- Write results.
    clusterResults <- case prior' of
        Nothing -> do
            let clusterList' = treeToClusterList tree'
                cr' = ClusterResults clusterList' tree'

            return cr'

        (Just x) -> do
            let clusterList' = treeToClusterList tree'
                cr' = ClusterResults clusterList' tree'

            return cr'

    B.writeFile
        (unOutputDirectory output' FP.</> "cluster_list.json")
        . A.encode
        . _clusterList
        $ clusterResults
    B.writeFile
        (unOutputDirectory output' FP.</> "cluster_tree.json")
        . A.encode
        . _clusterDend
        $ clusterResults
    T.writeFile
        (unOutputDirectory output' FP.</> "graph.dot")
        . G.printDotGraph
        . G.graphToDot G.nonClusteredParams
        . unClusterGraph
        $ gr'
    B.writeFile
        (unOutputDirectory output' FP.</> "cluster_info.csv")
        . printClusterInfo
        $ gr'
    -- Write matrix
    mapM_ (\x -> writeMatrixLike (MatrixTranspose False) x . extractSc $ processedSc) matrixOutput'
    -- Write label map
    mapM_
      ( bool
          (const (return ()))
          ( B.writeFile (unOutputDirectory output' FP.</> "labels.csv")
          . printLabelMap
          )
      . unLabelMapOutputFlag
      $ labelMapOutputFlag'
      )
      labelMap
    -- Write fragments.tsv.gz
    when (unFragmentsOutputFlag fragmentsOutputFlag')
      $ mapM_
          (\x -> saveFragments output' x =<< mapM getMatrixFileType matrixPaths')
          processedSc
    -- Write node info
    B.writeFile
        (unOutputDirectory output' FP.</> "node_info.csv")
        . printNodeInfo labelMap
        $ gr'
    case labelMap of
        Nothing   -> return ()
        (Just lm) ->
            -- Write cluster diversity
            case clusterDiversity order' lm clusterResults of
                (Left err) -> hPutStrLn stderr
                            $ err
                           <> "\nWarning: Problem in diversity, skipping cluster_diversity.csv output ..."
                (Right result) ->
                    B.writeFile
                        ( unOutputDirectory output'
                   FP.</> "cluster_diversity.csv"
                        )
                        . printClusterDiversity
                        $ result

    -- Increment  progress bar.
    Progress.incProgress pb 1

    -- Header
    B.putStrLn $ "cell,cluster,path"

    -- Body
    B.putStrLn
        . CSV.encode
        . fmap (\ (!ci, !(c:cs))
                -> ( unCell . _barcode $ ci
                , showt $ unCluster c
                , T.intercalate "/" . fmap (showt . unCluster) $ c:cs
                )
                )
        . _clusterList
        $ clusterResults

    -- Increment  progress bar.
    Progress.incProgress pb 1

    -- Plot only if needed and ignore non-tree analyses if dendrogram is
    -- supplied.
    H.runRegion $ do
        -- Calculations with plotting the label map (clumpiness).
        case labelMap of
            Nothing ->
                H.io $ hPutStrLn stderr "\nClumpiness requires labels for cells, skipping ..."
            (Just lm) -> do
                -- Get clumpiness.
                case treeToClumpList clumpinessMethod' lm . _clusterDend $ clusterResults of
                    (Left err) -> H.io
                                . hPutStrLn stderr
                                $ err
                               <> "\nWarning: Problem in clumpiness, skipping clumpiness output ..."
                    (Right clumpList) -> do
                        -- Save clumpiness to a file.
                        H.io
                            . B.writeFile (unOutputDirectory output' FP.</> "clumpiness.csv")
                            . clumpToCsv
                            $ clumpList

                        -- Plot clumpiness.
                        either (H.io . hPutStrLn stderr) id
                            $ plotClumpinessHeatmapR
                                (unOutputDirectory output' FP.</> "clumpiness.pdf")
                                clumpList

        -- View cutting location for modularity.
        case minDistanceSearch' of
          Nothing -> return ()
          (Just _) -> plotRankedModularityR
                        (unOutputDirectory output' FP.</> "modularity_rank.pdf")
                    . L.view clusterDend
                    $ clusterResults

        -- Increment  progress bar.
        H.io $ Progress.incProgress pb 1

        -- Plot.
        H.io $ do
            -- cr <- clusterResults
            -- gr <- graph
            -- cm <- itemColorMap

            -- plot <- if drawDendrogram'
            --         then return . plotDendrogram legend drawLeaf' cm . _clusterDend $ cr
            --         else do
            --             plotGraph legend drawConfig cm markColorMap gr

            D.renderCairo
                    ( unOutputDirectory output'
               FP.</> unDendrogramFile dendrogramOutput'
                    )
                    (D.mkHeight 1000)
                    plot

        -- Increment  progress bar.
        H.io $ Progress.incProgress pb 1

        -- Plot clustering of the projections.
        case projectionMap of
          Nothing  -> return ()
          (Just pm) -> do
            plotClustersR
              (unOutputDirectory output' FP.</> "projection.pdf")
              pm
              . _clusterList
              $ clusterResults

            -- Plot clustering with labels.
            case (labelMap, itemColorMap) of
                (Just lm, Just icm) ->
                    plotLabelClustersR
                        (unOutputDirectory output' FP.</> "label_projection.pdf")
                        pm
                        lm
                        icm
                        (_clusterList clusterResults)
                _ -> return ()

        -- Increment  progress bar.
        H.io $ Progress.incProgress pb 1

        return ()
makeTreeMain _ = error "Wrong path in make-tree, contact Gregory Schwartz for this error."
