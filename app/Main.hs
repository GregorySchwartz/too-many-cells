{- sc-cluster
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module Main where

-- Remote
import Control.Monad (when, unless, join)
import Data.Bool (bool)
import Data.Matrix.MatrixMarket (readMatrix, writeMatrix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Sparse (B (..))
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend)
import Options.Generic
import System.IO (hPutStrLn, stderr)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.GraphViz as G
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified Graphics.Matplotlib as P
import qualified H.Prelude as H
import qualified Plots as D
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.Differential.Differential
import TooManyCells.Differential.Types
import TooManyCells.Diversity.Diversity
import TooManyCells.Diversity.Load
import TooManyCells.Diversity.Plot
import TooManyCells.Diversity.Types
import TooManyCells.File.Types
import TooManyCells.MakeTree.Clumpiness
import TooManyCells.MakeTree.Cluster
import TooManyCells.MakeTree.Load
import TooManyCells.MakeTree.Plot
import TooManyCells.MakeTree.Types
import TooManyCells.MakeTree.Utility
import TooManyCells.Matrix.Load
import TooManyCells.Matrix.Preprocess
import TooManyCells.Matrix.Types

-- | Command line arguments
data Options
    = MakeTree { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (matrix.mtx, genes.tsv, and barcodes.tsv)or, if genes-file and cells-file are not specified, or an input csv file containing gene row names and cell column names. If given as a list (--matrixPath input1 --matrixPath input2 etc.) then will join all matrices together. Assumes the same number and order of genes in each matrix, so only cells are added."
               , projectionFile :: Maybe String <?> "([Nothing] | FILE) The input file containing positions of each cell for plotting. Format is \"barcode,x,y\" and matches column order in the matrix file. Useful for 10x where a TNSE projection is generated in \"projection.csv\". If not supplied, the resulting plot will use the first two features."
               , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
               , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell, with \"cell,label\" header."
               , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output."
               , preNormalization :: Bool <?> "Do not pre-normalize matrix before cluster normalization."
               , minSize :: Maybe Int <?> "([1] | INT) The minimum size of a cluster. Defaults to 1."
               , drawLeaf :: Maybe String <?> "([DrawText] | DrawItem) How to draw leaves in the dendrogram. DrawText is the number of cells in that leaf if --labels-file is provided, otherwise the leaves are labeled by majority cell label in that leaf. DrawItem is the collection of cells represented by circles, consisting of: DrawItem DrawLabel, where each cell is colored by its label, and DrawItem (DrawExpression GENE), where each cell is colored by the expression of GENE (corresponding to a gene name in the input matrix, not yet implemented)."
               , drawPie :: Maybe String <?> "([PieRing] | PieChart | PieNone) How to draw cell leaves in the dendrogram. PieRing draws a pie chart ring around the cells. PieChart only draws a pie chart instead of cells. PieNone only draws cells, no pie rings or charts."
               , drawDendrogram :: Bool <?> "Draw a dendrogram instead of a graph."
               , drawNodeNumber :: Bool <?> "Draw the node numbers on top of each vertex in the graph."
               , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
               , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity."
               , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    | Differential { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (matrix.mtx, genes.tsv, and barcodes.tsv)or, if genes-file and cells-file are not specified, or an input csv file containing gene row names and cell column names. If given as a list (--matrixPath input1 --matrixPath input2 etc.) then will join all matrices together. Assumes the same number and order of genes in each matrix, so only cells are added."
                   , projectionFile :: Maybe String <?> "([Nothing] | FILE) The input file containing positions of each cell for plotting. Format is \"barcode,x,y\" and matches column order in the matrix file. Useful for 10x where a TNSE projection is generated in \"projection.csv\". If not supplied, the resulting plot will use the first two features."
                   , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
                   , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output."
                   , preNormalization :: Bool <?> "Do not pre-normalize matrix before cluster normalization."
                   , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
                   , vertices :: String <?> "([VERTEX], [VERTEX]) Find the differential expression between cells belonging downstream of a list of vertices versus another list of vertices."
                   , topN :: Maybe Int <?> "([100] | INT ) The top INT differentially expressed genes."}
    | Diversity { priors :: [String] <?> "(PATH) Either input folders containing the output from a run of too-many-cells or a csv files containing the clusters for each cell in the format \"cell,cluster\". Advanced features not available in the latter case."
                , start :: Maybe Integer <?> "([0] | INT) For the rarefaction curve, start the curve at this subsampling."
                , interval :: Maybe Integer <?> "([1] | INT) For the rarefaction curve, the amount to increase each subsampling. For instance, starting at 0 with an interval of 4, we would sampling 0, 4, 8, 12, ..."
                , end :: Maybe Integer <?> "([N] | INT) For the rarefaction curve, which subsample to stop at. By default, the curve stops at the observed number of species for each population."
                , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity."
                , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    deriving ((Generic))

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "minSize"           = Just 'M'
    short "projectionFile"    = Just 'j'
    short "priors"            = Just 'P'
    short "preNormalization"  = Just 'n'
    short "drawLeaf"          = Just 'L'
    short "drawDendrogram"    = Just 'D'
    short "drawNodeNumber"    = Just 'N'
    short "order"             = Just 'O'
    short x                   = firstLetter x

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers

-- | Load the single cell matrix.
loadSSM :: Options -> FilePath -> IO (SingleCells MatObsRow)
loadSSM opts matrixPath' = do
    fileExist      <- FP.doesFileExist matrixPath'
    directoryExist <- FP.doesDirectoryExist matrixPath'

    let matrixFile' =
            case (fileExist, directoryExist) of
                (False, False) -> error "Matrix path does not exist."
                (True, False)  -> Left $ MatrixFile matrixPath'
                (False, True)  ->
                    Right . MatrixFile $ matrixPath' FP.</> "matrix.mtx"
        genesFile'  = GeneFile $ matrixPath' FP.</> "genes.tsv"
        cellsFile'  = CellFile $ matrixPath' FP.</> "barcodes.tsv"
        projectionFile' =
            fmap ProjectionFile . unHelpful . projectionFile $ opts
        delimiter'      =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        unFilteredSc   =
            case matrixFile' of
                (Left file) -> loadSparseMatrixDataStream
                                delimiter'
                                projectionFile'
                                file
                (Right file) -> loadCellrangerData
                                    projectionFile'
                                    genesFile'
                                    cellsFile'
                                    file
    unFilteredSc

-- | Load all single cell matrices.
loadAllSSM :: Options -> IO (SingleCells MatObsRow)
loadAllSSM opts = do
    let matrixPaths'    =
            (\xs -> bool (error "Need a matrix path.") xs . not . null $ xs)
                . unHelpful
                . matrixPath
                $ opts
        cellWhitelistFile' =
            fmap CellWhitelistFile . unHelpful . cellWhitelistFile $ opts
        preNormalization' =
            PreNormalization . unHelpful . preNormalization $ opts

    cellWhitelist <- sequence $ fmap getCellWhitelist cellWhitelistFile'

    mats <- mapM (loadSSM opts) matrixPaths'

    let whiteListFilter Nothing = id
        whiteListFilter (Just wl) = filterWhitelistSparseMat wl
        unFilteredSc = mconcat mats
        sc           = filterNumSparseMat
                     . whiteListFilter cellWhitelist
                     $ unFilteredSc
        processMat   = bool matrix (scaleSparseMat . matrix)
                      . unPreNormalization
                      $ preNormalization'
        processedSc  = sc { matrix = processMat sc }

    return processedSc

makeTreeMain :: Options -> IO ()
makeTreeMain opts = do
    let labelsFile'     =
            fmap LabelFile . unHelpful . labelsFile $ opts
        prior'          =
            fmap PriorPath . unHelpful . prior $ opts
        delimiter'      =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        preNormalization' =
            PreNormalization . unHelpful . preNormalization $ opts
        minSize'        =
            MinClusterSize . fromMaybe 1 . unHelpful . minSize $ opts
        drawLeaf'       = maybe DrawText read . unHelpful . drawLeaf $ opts
        drawPie'        = maybe PieRing read . unHelpful . drawPie $ opts
        drawDendrogram' = unHelpful . drawDendrogram $ opts
        drawNodeNumber' = DrawNodeNumber . unHelpful . drawNodeNumber $ opts
        order'          = Order . fromMaybe 1 . unHelpful . order $ opts
        output'         =
            OutputDirectory . fromMaybe "out" . unHelpful . output $ opts

        drawConfig      = DrawConfig drawLeaf' drawPie' drawNodeNumber'

        processedSc = loadAllSSM opts

    -- Where to place output files.
    FP.createDirectoryIfMissing True . unOutputDirectory $ output'

    labelMap <- sequence . fmap (loadLabelData delimiter') $ labelsFile'

    --R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        -- For r clustering.
        -- mat         <- scToRMat processedSc
        -- clusterRes  <- hdbscan mat
        -- clusterList <- clustersToClusterList sc clusterRes

        -- For agglomerative clustering.
        --let clusterResults = fmap hClust processedSc

    -- Load previous results or write if first run.
    (clusterResults, bMat, graph) <-
        case prior' of
            Nothing -> do
                (cr, b, gr) <- fmap (hSpecClust minSize') processedSc

                B.writeFile
                    (unOutputDirectory output' FP.</> "cluster_results.json")
                    . A.encode
                    $ cr
                writeMatrix (unOutputDirectory output' FP.</> "b.mtx")
                    . spMatToMat
                    . unB
                    $ b
                T.writeFile
                    (unOutputDirectory output' FP.</> "graph.dot")
                    . G.printDotGraph
                    . G.graphToDot G.nonClusteredParams
                    . unClusterGraph
                    $ gr
                case labelMap of
                    Nothing   -> return ()
                    (Just lm) ->
                        B.writeFile
                            (unOutputDirectory output' FP.</> "cluster_diversity.csv")
                            . printClusterDiversity order' lm
                            $ cr

                return ((return cr, return b, return gr) :: (IO ClusterResults, IO B, IO (ClusterGraph CellInfo)))
            (Just x) -> do
                let crInput =
                        (FP.</> "cluster_results.json") . unPriorPath $ x
                    bInput  = (FP.</> "b.mtx") . unPriorPath $ x
                    grInput  = (FP.</> "graph.dot") . unPriorPath $ x
                    cr :: IO ClusterResults
                    cr = fmap (either error id . A.eitherDecode)
                       . B.readFile
                       $ crInput
                    b :: IO B
                    b  = fmap (B . matToSpMat)
                       . readMatrix
                       $ bInput

                FP.copyFile crInput
                    . (FP.</> "cluster_results.json")
                    . unOutputDirectory
                    $ output'
                FP.copyFile bInput
                    . (FP.</> "b.mtx")
                    . unOutputDirectory
                    $ output'
                FP.copyFile grInput
                    . (FP.</> "graph.dot")
                    . unOutputDirectory
                    $ output'
                FP.copyFile crInput
                    . (FP.</> "cluster_diversity.csv")
                    . unOutputDirectory
                    $ output'

                gr <- fmap (dendrogramToGraph . clusterDend) cr

                return (cr, b, return gr)

    -- Calculations with the label map (clumpiness and cluster diversity).
    case labelMap of
        Nothing ->
            hPutStrLn stderr "Clumpiness requires labels for cells, skipping..."
        (Just lcm) -> do
            clusterResults
              >>= B.writeFile (unOutputDirectory output' FP.</> "clumpiness.csv")
                . dendToClumpCsv lcm
                . clusterDend

            clusterResults
                >>= B.writeFile (unOutputDirectory output' FP.</> "cluster_diversity.csv")
                  . printClusterDiversity order' lcm

    -- Header
    B.putStrLn $ "cell,cluster,path"

    -- Body
    clusterResults
        >>= B.putStrLn
          . CSV.encode
          . fmap (\ (!ci, !(c:cs))
                 -> ( unCell . barcode $ ci
                    , showt $ unCluster c
                    , T.intercalate "," . fmap (showt . unCluster) $ c:cs
                    )
                 )
          . clusterList

    -- | Plot only if needed and ignore non-tree analyses if dendrogram is
    -- supplied.
    H.withEmbeddedR defaultConfig $ H.runRegion $ do
        -- Get the color of each label.
        labelColorMap <- sequence $ fmap getLabelColorMap labelMap

        let itemColorMap =
                case drawLeaf' of
                    DrawText           ->
                        return $ do
                            lcm <- labelColorMap
                            lm  <- labelMap
                            return $ labelToItemColorMap lcm lm
                    DrawItem DrawLabel ->
                        return $ do
                            lcm <- labelColorMap
                            lm  <- labelMap
                            return $ labelToItemColorMap lcm lm
                    DrawItem (DrawExpression x) ->
                        fmap (Just . getItemColorMapExpression (Gene x)) processedSc

        -- Plot dendrogram.
        H.io $ do
            cr <- clusterResults
            gr <- graph
            cm <- itemColorMap
            legend <- case drawLeaf' of
                        (DrawItem (DrawExpression g)) ->
                            fmap
                                (Just . plotExpressionLegend (Gene g))
                                processedSc
                        _ -> return $ fmap plotLabelLegend labelColorMap

            plot <- if drawDendrogram'
                    then return . plotDendrogram legend drawLeaf' cm . clusterDend $ cr
                    else do
                        plotGraph legend drawConfig cm gr

            D.renderCairo
                    (unOutputDirectory output' FP.</> "dendrogram.pdf")
                    (D.mkHeight 1000)
                    plot

        -- Plot clustering.
        (H.io clusterResults)
          >>= plotClustersR (unOutputDirectory output' FP.</> "projection.pdf")
            . clusterList
            -- >>= D.renderCairo (x <> ".pdf") (D.mkWidth 1000)
            -- . D.renderAxis
            -- . plotClusters

        return ()

-- | Differential path.
differentialMain :: Options -> IO ()
differentialMain opts = do
    let vertices' = DiffVertices . read . unHelpful . vertices $ opts
        prior'    = PriorPath
                  . fromMaybe (error "Requires a previous run to get the graph.")
                  . unHelpful
                  . prior
                  $ opts
        topN'     = TopN . fromMaybe 100 . unHelpful . topN $ opts

    processedSc <- loadAllSSM opts

    let crInput = (FP.</> "cluster_results.json") . unPriorPath $ prior'
        cr :: IO ClusterResults
        cr = fmap (either error id . A.eitherDecode)
            . B.readFile
            $ crInput

    gr <- fmap (dendrogramToGraph . clusterDend) cr

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
        res <- getDEGraph
                topN'
                processedSc
                (fst . unDiffVertices $ vertices')
                (snd . unDiffVertices $ vertices')
                gr

        H.io . putStrLn . getDEString $ res

-- | Diversity path.
diversityMain :: Options -> IO ()
diversityMain opts = do
    let priors'         =
            fmap PriorPath . unHelpful . priors $ opts
        output'         =
            OutputDirectory . fromMaybe "out" . unHelpful . output $ opts
        order'       = Order . fromMaybe 1 . unHelpful . order $ opts
        start'       = Start . fromMaybe 0 . unHelpful . start $ opts
        interval'    = Interval . fromMaybe 1 . unHelpful . interval $ opts
        endMay'      = fmap End . unHelpful . end $ opts

    -- Where to place output files.
    FP.createDirectoryIfMissing True . unOutputDirectory $ output'

    pops <- sequence
          . fmap (\x -> do
                            pop <- fmap (L.view L._1) . loadPopulation $ x
                            return (Label . T.pack . unPriorPath $ x , pop)
                 )
          $ priors'

    popDiversities <-
        mapM
            (\ (l, pop) -> getPopulationDiversity
                                l
                                order'
                                start'
                                interval'
                                endMay'
                                pop
            )
            pops

    D.renderCairo (unOutputDirectory output' FP.</> "diversity.pdf") D.absolute
        . plotDiversity
        $ popDiversities

    D.renderCairo (unOutputDirectory output' FP.</> "chao1.pdf") D.absolute
        . plotChao1
        $ popDiversities

    D.renderCairo (unOutputDirectory output' FP.</> "rarefaction.pdf") D.absolute
        . plotRarefaction
        $ popDiversities

    P.file (unOutputDirectory output' FP.</> "diversity_py.pdf")
        . plotDiversityPy
        $ popDiversities

    P.file (unOutputDirectory output' FP.</> "chao1_py.pdf")
        . plotChao1Py
        $ popDiversities

    P.file (unOutputDirectory output' FP.</> "rarefaction_py.pdf")
        . plotRarefactionPy
        $ popDiversities

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
        let divFile = unOutputDirectory output' FP.</> "diversity_r.pdf"
        divPlot <- plotDiversityR popDiversities
        [r| ggsave(divPlot_hs, file = divFile_hs) |]

        let chao1File = unOutputDirectory output' FP.</> "chao_r.pdf"
        chao1Plot <- plotChao1R popDiversities
        [r| ggsave(chao1Plot_hs, file = chao1File_hs) |]

        let rarefactionFile = unOutputDirectory output' FP.</> "rarefaction_r.pdf"
        rarefactionPlot <- plotRarefactionR popDiversities
        [r| ggsave(rarefactionPlot_hs, file = rarefactionFile_hs) |]

        return ()

    return ()

main :: IO ()
main = do
    opts <- getRecord "too-many-cells, Gregory W. Schwartz.\
                      \ Clusters and analyzes single cell data."

    case opts of
        (MakeTree _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> makeTreeMain opts
        (Differential _ _ _ _ _ _ _ _)         -> differentialMain opts
        (Main.Diversity _ _ _ _ _ _)           -> diversityMain opts
