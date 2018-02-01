{- sc-cluster
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
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
import TooManyCells.Diversity.Diversity
import TooManyCells.Diversity.Load
import TooManyCells.Diversity.Plot
import TooManyCells.Diversity.Types
import TooManyCells.MakeTree.Clumpiness
import TooManyCells.MakeTree.Cluster
import TooManyCells.MakeTree.Load
import TooManyCells.MakeTree.Plot
import TooManyCells.MakeTree.Preprocess
import TooManyCells.MakeTree.Types
import TooManyCells.MakeTree.Utility

-- | Command line arguments
data Options = MakeTree { matrixFile  :: Maybe String
                               <?> "([matrix.mtx] | FILE) The input file containing the matrix output of cellranger or, if genes-file and cells-file are not specified, a csv containing gene row names and cell column names."
                       , genesFile :: Maybe String
                               <?> "([genes.tsv] | FILE) The input file containing gene information from cellranger. Matches row order in the matrix file."
                       , cellsFile :: Maybe String
                               <?> "([barcodes.tsv] | FILE) The input file containing barcode information from cellranger. Matches column order in the matrix file."
                       , projectionFile :: Maybe String
                               <?> "([Nothing] | FILE) The input file containing positions of each cell for plotting. Format is \"barcode,x,y\" and matches column order in the matrix file. Useful for 10x where a TNSE projection is generated in \"projection.csv\". If not supplied, the resulting plot will use the first two features."
                       , labelsFile :: Maybe String
                               <?> "([Nothing] | FILE) The input file containing the label for each cell, with \"cell,label\" header."
                       , delimiter :: Maybe Char
                               <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output."
                       , preNormalization :: Bool
                               <?> "Do not pre-normalize matrix before cluster normalization."
                       , minSize   :: Maybe Int
                               <?> "([1] | INT) The minimum size of a cluster. Defaults to 1."
                       , drawLeaf :: Maybe String
                               <?> "([DrawText] | DrawCell) How to draw leaves in the dendrogram. DrawText is the number of cells in that leaf if --labels-file is provided, otherwise the leaves are labeled by majority cell label in that leaf. DrawCell is the collection of cells represented by circles, consisting of: DrawCell DrawLabel, where each cell is colored by its label, and DrawCell (DrawExpression GENE), where each cell is colored by the expression of GENE (corresponding to a gene name in the input matrix, not yet implemented)."
                       , drawDendrogram :: Bool
                               <?> "Draw a dendrogram instead of a graph."
                       , drawNodeNumber :: Bool
                               <?> "Draw the node numbers on top of each vertex in the graph."
                       , prior :: Maybe String
                               <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
                       , vertices :: String
                                 <?> "([VERTEX], [VERTEX]) Find the differential expression between cells belonging downstream of a list of vertices versus another list of vertices. Supports only one population."
                       , output :: Maybe String
                               <?> "([out] | STRING) The folder containing output."
                       }
            | Diversity { priors   :: [String]
                                 <?> "(PATH) Either input folders containing the output from a run of sc-cluster or a csv files containing the clusters for each cell in the format \"cell,cluster\". Advanced features not available in the latter case."
                         , start    :: Maybe Integer
                                 <?> "([0] | INT) For the rarefaction curve, start the curve at this subsampling."
                         , interval :: Maybe Integer
                                 <?> "([1] | INT) For the rarefaction curve, the amount to increase each subsampling. For instance, starting at 0 with an interval of 4, we would sampling 0, 4, 8, 12, ..."
                         , end      :: Maybe Integer
                                 <?> "([N] | INT) For the rarefaction curve, which subsample to stop at. By default, the curve stops at the observed number of species for each population."
                         , order    :: Maybe Double
                                 <?> "([1] | DOUBLE) The order of diversity."
                         , output   :: Maybe String
                                 <?> "([out] | STRING) The folder containing output."
                         }
              deriving (Generic)

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

makeTreeMain :: Options -> IO ()
makeTreeMain opts = do
    let matrixFile'     =
            MatrixFile . fromMaybe "matrix.mtx" . unHelpful . matrixFile $ opts
        genesFile'      =
            GeneFile . fromMaybe "genes.tsv" . unHelpful . genesFile $ opts
        cellsFile'      =
            CellFile . fromMaybe "barcodes.tsv" . unHelpful . cellsFile $ opts
        projectionFile' =
            fmap ProjectionFile . unHelpful . projectionFile $ opts
        labelsFile'     =
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
        drawDendrogram' = unHelpful . drawDendrogram $ opts
        drawNodeNumber' = DrawNodeNumber . unHelpful . drawNodeNumber $ opts
        output'         =
            OutputDirectory . fromMaybe "out" . unHelpful . output $ opts
        matrixCsv       =
            any
                isNothing
                [unHelpful . genesFile $ opts, unHelpful . cellsFile $ opts]
        unFilteredSc   =
            if matrixCsv
                then loadSparseMatrixDataStream
                        delimiter'
                        projectionFile'
                        matrixFile'
                else loadCellrangerData
                        projectionFile'
                        genesFile'
                        cellsFile'
                        matrixFile'
        sc             = fmap filterSparseMat unFilteredSc
        processMat     = bool matrix (scaleSparseMat . matrix)
                       . unPreNormalization
                       $ preNormalization'
        processedSc    = sc >>= (\x -> return $ x { matrix = processMat x })

    -- Where to place output files.
    FP.createDirectoryIfMissing True . unOutputDirectory $ output'

    labelMap     <- sequence . fmap (loadLabelData delimiter') $ labelsFile'

    let labelColorMap = fmap getLabelColorMap labelMap
        cellColorMap =
            case drawLeaf' of
                DrawText           ->
                    return $ do
                        lcm <- labelColorMap
                        lm  <- labelMap
                        return $ labelToCellColorMap lcm lm
                DrawCell DrawLabel ->
                    return $ do
                        lcm <- labelColorMap
                        lm  <- labelMap
                        return $ labelToCellColorMap lcm lm
                DrawCell (DrawExpression x) ->
                    fmap (Just . getCellColorMapExpression (Gene x)) processedSc

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
                    . unCellGraph
                    $ gr

                return ((return cr, return b, return gr) :: (IO ClusterResults, IO B, IO CellGraph))
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

                gr <- fmap (dendrogramToGraph . clusterDend) cr

                return (cr, b, return gr)

    -- Find clumpiness.
    case labelMap of
        Nothing ->
            hPutStrLn stderr "Clumpiness requires labels for cells, skipping..."
        (Just lcm) ->
            clusterResults
              >>= B.writeFile (unOutputDirectory output' FP.</> "clumpiness.csv")
                . dendToClumpCsv lcm
                . clusterDend


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
        -- Plot dendrogram.
        H.io $ do
            cr <- clusterResults
            gr <- graph
            cm <- cellColorMap
            legend <- case drawLeaf' of
                        (DrawCell (DrawExpression g)) ->
                            fmap
                                (Just . plotExpressionLegend (Gene g))
                                processedSc
                        _ -> return $ fmap plotLabelLegend labelColorMap

            plot <- if drawDendrogram'
                    then return . plotDendrogram legend drawLeaf' cm . clusterDend $ cr
                    else do
                        plotGraph legend drawNodeNumber' drawLeaf' cm gr

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

    return ()

main :: IO ()
main = do
    opts <- getRecord "too-many-cells, Gregory W. Schwartz.\
                      \ Clusters and analyzes single cell data."

    case opts of
        (MakeTree _ _ _ _ _ _ _ _ _ _ _ _ _ _) -> makeTreeMain opts
        (Main.Diversity _ _ _ _ _ _) -> diversityMain opts
