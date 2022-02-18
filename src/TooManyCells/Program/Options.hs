{- TooManyCells.Program.Options
Gregory W. Schwartz

Options for the command line program.
-}

-- {-# LANGUAGE DataKinds         #-}
-- {-# LANGUAGE DeriveGeneric     #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module TooManyCells.Program.Options where

-- Remote
import BirchBeer.Types (DrawNodeMark (MarkNone), DrawCollection (PieChart), DrawLeaf (DrawText), Palette (..))
import TooManyCells.Paths.Types (PathDistance (PathStep))
import Math.Clustering.Hierarchical.Spectral.Types (EigenGroup (SignGroup))
import Options.Applicative
import qualified Data.Text as T
import qualified "find-clumpiness" Types as Clump

-- Local
import TooManyCells.Matrix.Types (NormType)

deriving instance Show Clump.Exclusivity

-- | Command line arguments
data Subcommand
    = MakeTreeCommand MakeTree
    | InteractiveCommand Interactive
    | DifferentialCommand Differential
    | DiversityCommand Diversity
    | PathsCommand Paths
    | ClassifyCommand Classify
    | PeaksCommand Peaks
    | MotifsCommand Motifs
    | MatrixOutputCommand MatrixOutput
    | SpatialCommand Spatial
    deriving (Read, Show)

sub :: Parser Subcommand
sub = hsubparser
  ( commandGroup "Analyses using the single-cell matrix"
 <> command "make-tree" (info makeTreeParser ( progDesc "Generate and plot the too-many-cells tree" ))
 <> command "interactive" (info interactiveParser ( progDesc "Interactive tree plotting (legacy, slow)" ))
 <> command "differential" (info differentialParser ( progDesc "Find differential features between groups of nodes" ))
 <> command "classify" (info classifyParser ( progDesc "Classify single-cells based on reference profiles" ))
 <> command "spatial" (info spatialParser ( progDesc "Spatially analyze single-cells" ))
 <> command "matrix-output" (info matrixOutputParser ( progDesc "Transform the input matrix only" ))
 ) <|> hsubparser ( commandGroup "No single-cell matrix analyses"
 <> command "diversity" (info diversityParser ( progDesc "Quantify the diversity and rarefaction curves of the tree" ))
 <> command "paths" (info pathsParser ( progDesc "Infer pseudo-time information from the tree" ))
  ) <|> hsubparser (commandGroup "too-many-peaks analyses for scATAC-seq"
 <> command "peaks" (info peaksParser ( progDesc "Find peaks in nodes for scATAC-seq" ))
 <> command "motifs" (info motifsParser ( progDesc "Find motifs from peaks for scATAC-seq" ))
  )

-- Global parsers
delimiterParser :: Parser Char
delimiterParser =
  option auto ( long "delimiter" <> metavar "CHAR" <> value ',' <> showDefault <> help "The delimiter for the most csv files in the program. For instance, if using a normal csv rather than cellranger output and for --labels-file." )

outputParser :: Parser String
outputParser = option str ( long "output" <> short 'o' <> metavar "FOLDER" <> value "out" <> showDefault <> help "The folder containing output." )

projectionParser :: Parser (Maybe String)
projectionParser = optional $ option str ( long "projection-file" <> short 'j' <> metavar "FILE" <> help "The input file containing positions of each cell for plotting. Format is \"item,x,y,sample\" as the header, with the sample column being optional (to separate, for instance, slides in a spatial analysis so there is no proximity between slides). Useful for 10x where a t-NSE projection is generated in \"projection.csv\". Cells without projections will not be plotted. If not supplied, no plot will be made." )

labelsFileParser :: Parser (Maybe String)
labelsFileParser = optional $ option str ( long "labels-file" <> short 'l' <> metavar "FILE" <> help "The input file containing the label for each cell barcode, with \"item,label\" header." )

data LoadMatrixOptions = LoadMatrixOptions {matrixPath :: [String]
                               , binwidth :: Maybe Int
                               , noBinarize :: Bool
                               , binarize :: Bool
                               , excludeMatchFragments :: Maybe String
                               , blacklistRegionsFile :: Maybe T.Text
                               , customRegion :: [T.Text]
                               , cellWhitelistFile :: Maybe String
                               , customLabel :: [T.Text]
                               , delimiter :: Char
                               , featureColumn :: Int
                               , normalization :: [NormType]
                               , matrixTranspose :: Bool
                               , pca :: Maybe Int
                               , lsa :: Maybe Int
                               , svd :: Maybe Int
                               , dropDimension :: Bool
                               , filterThresholds :: Maybe String
                               , shiftPositive :: Bool
                               } deriving (Read, Show)

loadMatrixParser :: Parser LoadMatrixOptions
loadMatrixParser = do
  matrixPath <- many $ option str ( long "matrix-path" <> short 'm' <> metavar "PATH" <> help "The path to the input directory containing the matrix output of cellranger (cellranger < 3 (matrix.mtx, genes.tsv, and barcodes.tsv) or cellranger >= 3 (matrix.mtx.gz, features.tsv.gz, and barcodes.tsv.gz) or an input csv file containing feature row names and cell column names. scATAC-seq is supported if input file contains \"fragments\", ends with \".tsv.gz\" (such as \"fragments.tsv.gz\" or \"sample1_fragments.tsv.gz\"), and is in the SORTED (sort -k1,1 -k2,2n) 10x fragments format (see also --binwidth, --no-binarize). If given as a list (--matrix-path input1 --matrix-path input2 etc.) then will join all matrices together. Assumes the same number and order of features in each matrix, so only cells are added." )
  binwidth <- optional $ option auto ( long "binwidth" <> short 'b' <> metavar "BINSIZE" <> help "If input data has region features in the format of `chrN:START-END`, BINSIZE input is required to convert ranges to fixed width bins." )
  noBinarize <- switch ( long "no-binarize" <> help "If a fragments.tsv.gz file, do not binarize data." )
  binarize <- switch ( long "binarize" <> help "Binarize data. Default for fragments.tsv.gz." )
  excludeMatchFragments <- optional $ option str ( long "exclude-match-fragments" <> short 'e' <> metavar "STRING" <> help "Exclude fragments from fragments.tsv.gz file if STRING is infix of fragment row. For instance, exclude all chrY fragments with \"--exclude-match-fragments chrY\"." )
  blacklistRegionsFile <- optional $ option str ( long "blacklist-regions-file" <> metavar "FILE" <> help "Bed file containing regions to ignore. Any fragments overlapping these regions are ignored if the input is not a fragments file." )
  customRegion <- many $ option str ( long "custom-region" <> metavar "chrN:START-END" <> help "Only look at these regions in the matrix for chromosome region features. A list in the format `chrN:START-END`, will assign values for a cell intersecting this region to this region. If a cell region overlaps two or more regions, will be counting that many times in the new features. Example input: `--custom-regions \"chr1:1003-10064\" --custom-regions \"chr2:3021-5034\"` etc." )
  cellWhitelistFile <- optional $ option str ( long "cell-whitelist-file" <> short 'c' <> metavar "FILE" <> help "The input file containing the cells to include. No header, line separated list of barcodes." )
  customLabel <- many $ option str ( long "customLabel" <> short 'Z' <> metavar "[LABEL]" <> help "List of labels to assign each matrix if all cells from each matrix are given the same label per matrix. This argument intends to simplify the process of labeling by bypassing --labels-file if the user just wants each matrix to have its own label (i.e. sample). Must be the same length and order as --matrix-path: for instance, --matrix-path input1 --custom-label sample1 --matrix-path input2 --custom-label sample2 etc. will label all cells from input1 with sample1, input2 with sample2, etc. If there are multiple labels per matrix, you must use --labels-file." )
  delimiter <- delimiterParser
  featureColumn <- option auto ( long "feature-column" <> metavar "COLUMN" <> value 1 <> showDefault <> help "The column (1-indexed) in the features.tsv.gz file to use for feature names. If using matrix market format, cellranger stores multiple columns in the features file, usually the first column for the Ensembl identifier and the second column for the gene symbol. If the Ensembl identifier is not quickly accessible, use --feature-column 2 for the second column, which is usually more ubiquitous. Useful for overlaying gene expression so you can say --draw-leaf \"DrawItem (DrawContinuous \\\"CD4\\\")\") instead of --draw-leaf \"DrawItem (DrawContinuous \\\"ENSG00000010610\\\")\"). Does not affect CSV format (the column names will be the feature names)." )
  normalization <- many $ option auto ( long "normalization" <> short 'z' <> metavar "[TfIdfNorm] | UQNorm | MedNorm | TotalMedNorm | TotalNorm | LogCPMNorm DOUBLE | QuantileNorm | NoneNorm" <> help "Type of normalization before clustering. Can be used as a list to perform one after the other: --normalization QuantileNorm --normalization TfIdfNorm will first apply quantile then tf-idf normalization. TfIdfNorm normalizes based on the prevalence of each feature. UQNorm normalizes each observation by the upper quartile non-zero counts of that observation. MedNorm normalizes each observation by the median non-zero counts of that observation. TotalNorm normalizes each cell by the total count. TotalMedNorm normalized first each observation by total count then by median of non-zero counts across features. (LogCPMNorm DOUBLE) normalizes by logB(CPM + 1) where B is DOUBLE. QuantileNorm normalizes by quantile normalization, ignores zeros, may be slow. MinMaxNorm does min-max normalization of each cell. TransposeNorm just transposes the matrix, so any row-based normalization here can instead be applied to columns: for instance, --normalization QuantileNorm --normalization TransposeNorm --normalization MinMaxNorm --normalization TransposeNorm will first apply quantile normalization to each cell, then min-max normalization to each column (before returning the cells to the proper axis with another transpose. If the number of TransposeNorms are odd, then one is automatically added to the end to ensure the same axis as before normalizations (see --matrix-transpose to permanently re-orientate the matrix)). NoneNorm does not normalize. Default is TfIdfNorm for clustering and NoneNorm for everything else (note, using --edger in differential analysis will result in edgeR preprocessing on top of any other normalization chosen). Older versions had BothNorm which has been replaced with --normalization TotalMedNorm --normalization TfIdfNorm. This change will also affect other analyses, as TfIdfNorm will now be in non-cluster-related entry points if specified, so --normalization UQNorm from < v2.0.0.0 is now --normalization UQNorm --normalization TfIdfNorm." )
  matrixTranspose <- switch ( long "matrix-transpose" <> short 'T' <> help "Whether to transpose the matrix before all processing (observations become features and vice-versa). Will be affected by other options (check your filtering thresholds, normalizations, etc!)" )
  pca <- optional $ option auto ( long "pca" <> metavar "INT" <> help "Not recommended, as it makes cosine similarity less meaningful (therefore less accurate -- instead, consider making your own similarity matrix and using cluster-tree, our sister algorithm, to cluster the matrix and plot with birch-beer). The number of dimensions to keep for PCA dimensionality reduction before clustering. Default is no PCA at all in order to keep all information. Should use with --shift-positive to ensure no negative values (as --pca will center and scale). Consider changing the modularity cutoff to a lower value (such as --min-modularity -0.5)." )
  lsa <- optional $ option auto ( long "lsa" <> metavar "INT" <> help "The number of dimensions to keep for LSA dimensionality reduction. Uses TD-IDF followed by SVD before clustering, same warnings as --pca apply, including the use of --shift-positive with possible --min-modularity -0.5." )
  svd <- optional $ option auto ( long "svd" <> metavar "INT" <> help "The number of dimensions to keep for SVD dimensionality reduction. Will center and scale, same warnings as --pca apply, including the use of --shift-positive with possible --min-modularity -0.5." )
  dropDimension <- switch ( long "drop-dimension" <> help "Instead of keeping dimensions for --pca, --lsa, or --svd, drop the number of dimensions (i.e. --lsa 1 drops the first projection which may have a strong batch effect)." )
  filterThresholds <- optional $ option str ( long "filter-thresholds" <> short 'H' <> metavar "(DOUBLE, DOUBLE)" <> help "The minimum filter thresholds for (MINCELL, MINFEATURE) when filtering cells and features by low read counts. Default changed to Nothing due to additional supported assays. To use the original filter thresholds, use --filter-thresholds \"(250, 1)\"." )
  shiftPositive <- switch ( long "shift-positive" <> help "Shift features to positive values. Positive values are shifted to allow modularity to work correctly." )

  pure $ LoadMatrixOptions {..}

data MakeTree = MakeTree { loadMatrixOptions :: LoadMatrixOptions
               , projectionFile :: Maybe String
               , labelsFile :: Maybe String
               , eigenGroup :: EigenGroup
               , numEigen :: Maybe Int
               , numRuns :: Maybe Int
               , minSize :: Maybe Int
               , maxStep :: Maybe Int
               , maxProportion :: Maybe Double
               , minModularity :: Maybe Double
               , minDistance :: Maybe Double
               , minDistanceSearch :: Maybe Double
               , smartCutoff :: Maybe Double
               , elbowCutoff :: Maybe String
               , customCut :: [Int]
               , rootCut :: Maybe Int
               , dendrogramOutput :: Maybe String
               , matrixOutput :: Maybe String
               , labelsOutput :: Bool
               , fragmentsOutput :: Bool
               , drawLeaf :: Maybe String
               , drawCollection :: DrawCollection
               , drawMark :: DrawNodeMark
               , drawNodeNumber :: Bool
               , drawMaxNodeSize :: Double
               , drawMaxLeafNodeSize :: Maybe Double
               , drawNoScaleNodes :: Bool
               , drawLegendSep :: Double
               , drawLegendAllLabels :: Bool
               , drawPalette :: Palette
               , drawColors :: Maybe String
               , drawDiscretize :: Maybe String
               , drawScaleSaturation :: Maybe Double
               , drawItemLineWeight :: Maybe Double
               , drawFont :: Maybe String
               , drawBarBounds :: Bool
               , prior :: Maybe String
               , noUpdateTreeRows :: Bool
               , order :: Double
               , clumpinessMethod :: Clump.Exclusivity
               , dense :: Bool
               , output :: String
               } deriving (Read, Show)

makeTreeParser :: Parser Subcommand
makeTreeParser = do
  loadMatrixOptions <- loadMatrixParser
  projectionFile <- projectionParser
  labelsFile <- labelsFileParser
  eigenGroup <- option auto ( long "eigen-group" <> short 'B' <> metavar "SignGroup | KMeansGroup" <> value SignGroup <> showDefault <> help "Whether to group the eigenvector using the sign or kmeans while clustering. While the default is sign, kmeans may be more accurate (but starting points are arbitrary)." )
  numEigen <- optional $ option auto ( long "num-eigen" <> short 'G' <> metavar "INT" <> help "Number of eigenvectors to use while clustering with kmeans. Takes from the second to last eigenvector. Recommended to start at 1 and work up from there if needed. May help offset the possible instability and inaccuracy of SVDLIBC." )
  numRuns <- optional $ option auto ( long "numRuns" <> metavar "([Nothing] | INT)" <> help "Number of runs for permutation test at each split for modularity. Defaults to no test." )
  minSize <- optional $ option auto ( long "min-size" <> short 'M' <> metavar "INT" <> help "The minimum size of a cluster. Defaults to 1." )
  maxStep <- optional $ option auto ( long "max-step" <> short 'S' <> metavar "INT" <> help "Only keep clusters that are INT steps from the root. Defaults to all steps." )
  maxProportion <- optional $ option auto ( long "max-proportion" <> short 'X' <> metavar "DOUBLE" <> help "Stopping criteria to stop at the node immediate after a node with DOUBLE proportion split. So a node N with L and R children will stop with this criteria at 0.5 if |L| / |R| < 0.5 or > 2 (absolute log2 transformed), that is, if one child has over twice as many items as the other child. Includes L and R in the final result." )
  minModularity <- optional $ option auto ( long "min-modularity" <> metavar "DOUBLE" <> help "Nearly the same as --min-distance, but for clustering instead of drawing (so the output json tree can be larger). Stopping criteria to stop at the node with DOUBLE modularity. So a node N with L and R children will stop with this criteria the distance at N to L and R is < DOUBLE. Does not include L and R in the final result." )
  minDistance <- optional $ option auto ( long "min-distance" <> short 't' <> metavar "DOUBLE" <> help "Stopping criteria to stop at the node immediate after a node with DOUBLE distance. So a node N with L and R children will stop with this criteria the distance at N to L and R is < DOUBLE. Includes L and R in the final result." )
  minDistanceSearch <- optional $ option auto ( long "min-distance-search" <> metavar "DOUBLE" <> help "Similar to --min-distance, but searches from the leaves to the root -- if a path from a subtree contains a distance of at least DOUBLE, keep that path, otherwise prune it. This argument assists in finding distant nodes." )
  smartCutoff <- optional $ option auto ( long "smart-cutoff" <> short 's' <> metavar "DOUBLE" <> help "Whether to set the cutoffs for --min-size, --max-proportion, --min-distance, and --min-distance-search based off of the distributions (median + (DOUBLE * MAD)) of all nodes. To use smart cutoffs, use this argument and then set one of the four arguments to an arbitrary number, whichever cutoff type you want to use. --max-proportion and --min-size distributions are log2 transformed." )
  elbowCutoff <- optional $ option str ( long "elbow-cutoff" <> metavar "Max | Min" <> help "Whether to set the cutoffs for --min-size, --max-proportion, --min-distance, and --min-distance-search based off of the elbow point of distributions of all nodes. For a distribution in positive x and y on a graph, the top left hump would be Max and the bottom right dip would be Min. To use elbow cutoffs, use this argument and then set one of the three arguments to an arbitrary number, whichever cutoff type you want to use. --max-proportion and --min-size distributions are log2 transformed. Conflicts with --smart-cutoff, so this argument takes precedent." )
  customCut <- many $ option auto ( long "custom-cut" <> metavar "NODE" <> help "List of nodes to prune (make these nodes leaves). Invoked by --custom-cut 34 --custom-cut 65 etc." )
  rootCut <- optional $ option auto ( long "root-cut" <> metavar "NODE" <> help "Assign a new root to the tree, removing all nodes outside of the subtree." )
  dendrogramOutput <- optional $ option str ( long "dendrogram-output" <> short 'U' <> metavar "FILE" <> value "dendrogram.svg" <> showDefault <> help "The filename for the dendrogram. Supported formats are PNG, PS, PDF, and SVG." )
  matrixOutput <- optional $ option str ( long "matrix-output" <> metavar "FOLDER | FILE.csv" <> help "Output the filtered and normalized (not including TfIdfNorm) matrix in this folder under the --output directory in matrix market format or, if a csv file is specified, a dense csv format. Like input, features are rows." )
  labelsOutput <- switch ( long "labels-output" <> help "Whether to write the labels used for each observation as a labels.csv file in the output folder." )
  fragmentsOutput <- switch ( long "fragments-output" <> help "Whether to output fragments_tsv.gz with barcodes altered by --custom-label in the output folder (excludes filtered-out cells). Useful for downstream analysis by the peaks entry point where the cluster barcodes differ from the original fragments.tsv.gz file when using --custom-label. Matches barcodes based on BARCODE-LABEL." )
  drawLeaf <- optional $ option str ( long "draw-leaf" <> short 'L' <> metavar "DrawText | DrawItem DrawItemType" <> help "How to draw leaves in the dendrogram. DrawText is the number of items in that leaf. DrawItem is the collection of items represented by circles, consisting of: DrawItem DrawLabel, where each item is colored by its label, DrawItem (DrawContinuous [FEATURE]), where each item is colored by the expression of FEATURE (corresponding to a feature name in the input matrix, [FEATURE] is a list, so if more than one FEATURE is listed, uses the average of the feature values), DrawItem (DrawThresholdContinuous [(FEATURE, THRESHOLD)]), where each item is colored by the binary high / low expression of FEATURE based on THRESHOLD (either `Exact DOUBLE` or `MadMedian DOUBLE`, where Exact just uses the DOUBLE as a cutoff value while MadMedian uses the DOUBLE as the number of MADs away from the median value of the feature) and multiple FEATUREs can be used to combinatorically label items (FEATURE1 high / FEATURE2 low, etc.), DrawItem DrawSumContinuous, where each item is colored by the sum of the post-normalized columns (use --normalization NoneNorm for UMI counts, default), DrawItem (DrawProximity ([NODE], DISTANCE)), where each item is colored by its proximity in Euclidean distance (a neighbor is defined as less than DISTANCE to the selected nodes) to a set of items drawn from a list of NODE, and DrawItem DrawDiversity, where each node is colored by the diversity based on the labels of each item and the color is normalized separately for the leaves and the inner nodes. The default is DrawText, unless --labels-file is provided, in which DrawItem DrawLabel is the default. If the label or feature cannot be found, the default color will be black (check your spelling!)." )
  drawCollection <- option auto ( long "draw-collection" <> short 'E' <> metavar "PieChart | PieRing | IndividualItems | Histogram | NoLeaf | CollectionGraph MAXWEIGHT THRESHOLD [NODE]" <> value PieChart <> showDefault <> help "How to draw item leaves in the dendrogram. PieRing draws a pie chart ring around the items. PieChart only draws a pie chart instead of items. IndividualItems only draws items, no pie rings or charts. Histogram plots a histogram of the features requested. NoLeaf has no leaf, useful if there are so many items the tree takes very long to render. (CollectionGraph MAXWEIGHT THRESHOLD [NODE]) draws the nodes and edges within leaves that are descendents of NODE (empty list [] indicates draw all leaf networks) based on the input matrix, normalizes edges based on the MAXWEIGHT, and removes edges for display less than THRESHOLD (after normalization, so for CollectionGraph 2 0.5 [26], draw the leaf graphs for all leaves under node 26, then a edge of 0.7 would be removed because (0.7 / 2) < 0.5). For CollectionGraph with no colors, use --draw-leaf \"DrawItem DrawLabel\" and all nodes will be black. If you don't specify this option, DrawText from --draw-leaf overrides this argument and only the number of cells will be plotted." )
  drawMark <- option auto ( long "draw-mark" <> short 'K' <> metavar "MarkNone | MarkModularity | MarkSignificance" <> value MarkNone <> showDefault <> help "How to draw annotations around each inner node in the tree. MarkNone draws nothing and MarkModularity draws a black circle representing the modularity at that node, darker black means higher modularity for that next split. MarkSignificance is for significance, i.e. p-value, darker means higher value." )
  drawNodeNumber <- switch ( long "draw-node-number" <> short 'N' <> help "Draw the node numbers on top of each node in the graph." )
  drawMaxNodeSize <- option auto ( long "draw-max-node-size" <> short 'A' <> metavar "DOUBLE" <> value 72 <> showDefault <> help "The max node size when drawing the graph. 36 is the theoretical default, but here 72 makes for thicker branches." )
  drawMaxLeafNodeSize <- optional $ option auto ( long "draw-max-leaf-node-size" <> metavar "DOUBLE" <> help "The max leaf node size when drawing the graph. Defaults to the value of --draw-max-node-size." )
  drawNoScaleNodes <- switch ( long "draw-no-scale-nodes" <> short 'W' <> help "Do not scale inner node size when drawing the graph. Instead, uses draw-max-node-size as the size of each node and is highly recommended to change as the default may be too large for this option." )
  drawLegendSep <- option auto ( long "draw-legend-sep" <> short 'Q' <> metavar "DOUBLE" <> value 1 <> showDefault <> help "The amount of space between the legend and the tree." )
  drawLegendAllLabels <- switch ( long "draw-legend-all-labels" <> short 'J' <> help "Whether to show all the labels in the label file instead of only showing labels within the current tree. The program generates colors from all labels in the label file first in order to keep consistent colors. By default, this value is false, meaning that only the labels present in the tree are shown (even though the colors are the same). The subset process occurs after --draw-colors, so when using that argument make sure to account for all labels." )
  drawPalette <- option auto ( long "draw-palette" <> short 'Y' <> metavar "Set1 | Hsv | Ryb | Blues" <> value Set1 <> showDefault <> help "Palette to use for legend colors. With high saturation in --draw-scale-saturation, consider using Hsv to better differentiate colors." )
  drawColors <- optional $ option str ( long "draw-colors" <> short 'R' <> metavar "COLORS" <> help "Custom colors for the labels or continuous features. Will repeat if more labels than provided colors. For continuous feature plots, uses first two colors [high, low], defaults to [red, gray]. For instance: --draw-colors \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\"" )
  drawDiscretize <- optional $ option str ( long "draw-discretize" <> metavar "COLORS | INT" <> help "Discretize colors by finding the nearest color for each item and node. For instance, --draw-discretize \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\" will change all node and item colors to one of those two colors, based on Euclidean distance. If using \"--draw-discretize INT\", will instead take the default map and segment (or interpolate) it into INT colors, rather than a more continuous color scheme. May have unintended results when used with --draw-scale-saturation." )
  drawScaleSaturation <- optional $ option auto ( long "draw-scale-saturation" <> short 'V' <> metavar "DOUBLE" <> help "Multiply the saturation value all nodes by this number in the HSV model. Useful for seeing more visibly the continuous colors by making the colors deeper against a gray scale." )
  drawItemLineWeight <- optional $ option auto ( long "draw-item-line-weight" <> metavar "DOUBLE" <> help "The line weight for items in the leaves if shown. Supplied as if there are too many items, the collection may look like a black box. Set to 0 to disable outlines of items to avoid this. Default: 0.1" )
  drawFont <- optional $ option str ( long "draw-font" <> metavar "FONT" <> help "Specify the font to use for the labels when plotting. Default: Arial" )
  drawBarBounds <- switch ( long "draw-bar-bounds" <> help "Whether to plot only the minimum and maximum ticks for the color bars." )
  prior <- optional $ option str ( long "prior" <> short 'p' <> metavar "STRING" <> help "The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files." )
  noUpdateTreeRows <- switch ( long "no-update-tree-rows" <> help "Don't update the row indices of a tree if using an identical matrix to the one which generated the tree. Should not be used unless the matrix to make the tree is identical, then can result in speedup." )
  order <- option auto ( long "order" <> short 'O' <> metavar "DOUBLE" <> value 1 <> showDefault <> help "The order of diversity." )
  clumpinessMethod <- option auto ( long "clumpiness-method" <> short 'u' <> metavar "Majority | Exclusive | AllExclusive" <> value Clump.Majority <> showDefault <> help "The method used when calculating clumpiness: Majority labels leaves according to the most abundant label, Exclusive only looks at leaves consisting of cells solely from one label, and AllExclusive treats the leaf as containing both labels." )
  dense <- switch ( long "dense" <> help "Whether to use dense matrix algorithms for clustering. Should be faster for dense matrices, so if batch correction, PCA, or other algorithms are applied upstream to the input matrix, consider using this option to speed up the tree generation." )
  output <- outputParser

  pure $ MakeTreeCommand (MakeTree {..})

data Interactive = Interactive { loadMatrixOptions :: LoadMatrixOptions
                               , labelsFile :: Maybe String
                               , prior :: String
                               , noUpdateTreeRows :: Bool
                               } deriving (Read, Show)

interactiveParser :: Parser Subcommand
interactiveParser = do
  loadMatrixOptions <-  loadMatrixParser
  labelsFile <- labelsFileParser
  prior <- option str ( long "prior" <> metavar "STRING" <> help "The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files." )
  noUpdateTreeRows <- switch ( long "no-update-tree-rows" <> help "Don't update the row indices of a tree if using an identical matrix to the one which generated the tree. Should not be used unless the matrix to make the tree is identical, then can result in speedup." )

  pure $ InteractiveCommand (Interactive {..})

data Differential = Differential { loadMatrixOptions :: LoadMatrixOptions
                   , labelsFile :: Maybe String
                   , prior :: String
                   , noUpdateTreeRows :: Bool
                   , edger :: Bool
                   , nodes :: String
                   , labels :: Maybe String
                   , topN :: Int
                   , seed :: Int
                   , subsampleGroups :: Maybe Int
                   , features :: [T.Text]
                   , aggregate :: Bool
                   , plotSeparateNodes :: Bool
                   , plotSeparateLabels :: Bool
                   , plotViolin :: Bool
                   , plotNoOutlier :: Bool
                   , plotOutput :: String
                   } deriving (Read, Show)

differentialParser :: Parser Subcommand
differentialParser = do
  loadMatrixOptions <-  loadMatrixParser
  labelsFile <- labelsFileParser
  prior <- option str ( long "prior" <> metavar "STRING" <> help "The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files." )
  noUpdateTreeRows <- switch ( long "no-update-tree-rows" <> help "Don't update the row indices of a tree if using an identical matrix to the one which generated the tree. Should not be used unless the matrix to make the tree is identical, then can result in speedup." )
  edger <- switch ( long "edger" <> help "Use edgeR instead of Kruskall-Wallis for differential expression between two sets of nodes (automatically off and required to be off for all to all comparisons)." )
  nodes <- option str ( long "nodes" <> short 'n' <> metavar "([NODE], [NODE])" <> help "Find the differential expression between cells belonging downstream of a list of nodes versus another list of nodes. Directionality is \"([1], [2])\" -> 2 / 1. \"([], [])\" switches the process to instead find the log2 average division between all nodes with all other cells in the provided data set matrix regardless of whether they are present within the tree (node / other cells) using the Kruskal-Wallis Test (--features does not work for this, --labels works, and UQNorm for the normalization is recommended. Only returns nodes where the comparison had both groups containing at least five cells.). If not using --no-update-tree-rows, remember to filter the matrix for cells outside of the tree if you only want to compare against other nodes within the tree." )
  labels <- optional $ option str ( long "labels" <> metavar "([LABEL], [LABEL])" <> help "Use --labels-file to restrict the differential analysis to cells with these labels. Same format as --nodes, so the first list in --nodes and --labels gets the cells within that list of nodes with this list of labels. The same for the second list. For instance, --nodes \"([1], [2])\" --labels \"([\\\"A\\\"], [\\\"B\\\"])\" will compare cells from node 1 of label \"A\" only with cells from node 2 of label \"B\" only. To use all cells for that set of nodes, use an empty list, i.e. --labels \"([], [\\\"A\\\"])\". When comparing all nodes with all other cells, remember that the notation would be ([Other Cells], [Node]), so to compare cells of label X in Node with cells of label Y in Other Cells, use --labels \"([\\\"Y\\\", \\\"X\\\"])\". Requires both --labels and --labels-file, otherwise will include all labels." )
  topN <- option auto ( long "top-n" <> metavar "INT" <> value 100 <> showDefault <> help "The top INT differentially expressed features." )
  seed <- option auto ( long "seed" <> metavar "INT" <> value 0 <> showDefault <> help "The seed to use for subsampling. See --subsample-groups." )
  subsampleGroups <- optional $ option auto ( long "subsample-groups" <> metavar "INT" <> help "Whether to subsample each group in the differential comparison. Subsets the specified number of cells. When set to 0, subsamples the larger group to equal the size of the smaller group. When using with --nodes \"([], [])\" to compare all nodes against each other, note that the compared nodes may be resampled. Highly experimental at this stage, use with caution. See --seed." )
  features <- many $ option str ( long "features" <> metavar "FEATURE" <> help "List of features (e.g. genes) to plot for all cells within selected nodes. Invoked by --features CD4 --features CD8 etc. When this argument is supplied, only the plot is outputted and differential expression is ignored. Outputs to --output." )
  aggregate <- switch ( long "aggregate" <> help "Whether to plot the aggregate (mean here) of features for each cell from \"--features\" instead of plotting different distributions for each feature." )
  plotSeparateNodes <- switch ( long "plot-separate-nodes" <> help "Whether to plot each node separately. This will plot each node provided in --nodes from both entries in the tuple (as they may be different from --labels)." )
  plotSeparateLabels <- switch ( long "plot-separate-labels" <> help "Whether to plot each label separately. This will plot each label provided in --labels from both entries in the tuple (as they may be different from --nodes)." )
  plotViolin <- switch ( long "plot-violin" <> help "Whether to plot features as a violin plots instead of boxplots." )
  plotNoOutlier <- switch ( long "plot-no-outlier" <> help "Whether to avoid plotting outliers as there can be too many, making the plot too large." )
  plotOutput <- option str ( long "plot-output" <> metavar "FILE" <> value "out.pdf" <> showDefault <> help "The file containing the output plot." )

  pure $ DifferentialCommand (Differential {..})

data Diversity = Diversity { priors :: [String]
                , delimiter :: Char
                , labelsFile :: Maybe String
                , start :: Integer
                , interval :: Integer
                , end :: Maybe Integer
                , order :: Double
                , output :: String
                } deriving (Read, Show)

diversityParser :: Parser Subcommand
diversityParser = do
  priors <- many $ option str ( long "priors" <> short 'P' <> metavar "PATH" <> help "Either input folders containing the output from a run of too-many-cells or a csv files containing the clusters for each cell in the format \"cell,cluster\". Advanced features not available in the latter case. If --labels-file is specified, those labels designate entity type, otherwise the assigned cluster is the entity type." )
  delimiter <- delimiterParser
  labelsFile <- labelsFileParser
  start <- option auto ( long "start" <> short 's' <> metavar "INT" <> value 0 <> showDefault <> help "For the rarefaction curve, start the curve at this subsampling." )
  interval <- option auto ( long "interval" <> short 'i' <> metavar "INT" <> value 1 <> showDefault <> help "For the rarefaction curve, the amount to increase each subsampling. For instance, starting at 0 with an interval of 4, we would sampling 0, 4, 8, 12, ..." )
  end <- optional $ option auto ( long "end" <> metavar "INT" <> help "For the rarefaction curve, which subsample to stop at. By default, the curve stops at the observed number of species for each population." )
  order <- option auto ( long "order" <> metavar "DOUBLE" <> value 1 <> showDefault <> help "The order of diversity." )
  output <- outputParser

  pure $ DiversityCommand (Diversity {..})

data Paths = Paths { prior :: Maybe String
            , labelsFile :: Maybe String
            , flipDirection :: Bool
            , shallowStart :: Bool
            , pathDistance :: PathDistance
            , bandwidth :: Double
            , delimiter :: Char
            , pathsPalette :: Palette
            , output :: String
            } deriving (Read, Show)

pathsParser :: Parser Subcommand
pathsParser = do
  prior <- optional $ option str ( long "prior" <> metavar "FOLDER" <> help "The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files." )
  labelsFile <- labelsFileParser
  flipDirection <- switch ( long "flip-direction" <> help "Flip the starting node when calculating the distances." )
  shallowStart <- switch ( long "shallow-start" <> help "Choose the shallowest leaf as the starting node (rather than the deepest)." )
  pathDistance <- option auto ( long "path-distance" <> metavar "PathStep | PathModularity" <> value PathStep <> showDefault <> help "How to measure the distance from the starting leaf. PathModularity weighs the steps by the modularity, while PathStep counts the number of steps." )
  bandwidth <- option auto ( long "bandwidth" <> metavar "DOUBLE" <> value 1 <> showDefault <> help "Bandwidth of the density plot." )
  delimiter <- delimiterParser
  pathsPalette <- option auto ( long "paths-palette" <> metavar "Set1 | Hsv | Ryb | Blues" <> value Set1 <> showDefault <> help "Palette to use for legend colors." )
  output <- outputParser

  pure $ PathsCommand (Paths {..})

data Classify = Classify { loadMatrixOptions :: LoadMatrixOptions
               , referenceFile :: [String]
               , singleReferenceMatrix :: Bool
               , skipAggregation :: Bool
               , labelsFile :: Maybe String
               } deriving (Read, Show)

classifyParser :: Parser Subcommand
classifyParser = do
  loadMatrixOptions <- loadMatrixParser
  referenceFile <- many $ option str (long "reference-file" <> short 'r' <> metavar "PATH" <> help "The path to the reference file to compare each cell to. Every transformation (e.g. filters and normalizations) applied to --matrix-path apply here as well." )
  singleReferenceMatrix <- switch ( long "single-reference-matrix" <> help "Treat the reference file as a single matrix such that each observation (barcode) is an aggregated reference population." )
  skipAggregation <- switch ( long "skip-aggregation" <> help "If there is more than one reference file, treat each reference file as a bulk population. Useful when comparing to bulk populations for BigWig or BedGraph reference files where each file is a separate population." )
  labelsFile <- labelsFileParser

  pure $ ClassifyCommand (Classify {..})

data Peaks = Peaks { fragmentsPath :: [String]
            , prior :: Maybe String
            , delimiter :: Char
            , labelsFile :: Maybe String
            , peaksExcludeMatchFragments :: Maybe String
            , peaksBlacklistRegionsFile :: Maybe T.Text
            , peaksCustomRegion :: [T.Text]
            , peakCallCommand :: String
            , genomecovCommand :: String
            , genome :: String
            , skipFragments :: Bool
            , peakNode :: [Int]
            , peakNodeLabels :: [String]
            , allNodes :: Bool
            , bedgraph :: Bool
            , output :: String
            } deriving (Read, Show)

peaksParser :: Parser Subcommand
peaksParser = do
  fragmentsPath <- many $ option str ( long "fragments-path" <> short 'f' <> metavar "PATH" <> help "The path to the input fragments.tsv.gz file. The input file must contain \"fragments\" and end with \".tsv.gz\" (such as \"fragments.tsv.gz\" or \"sample1_fragments.tsv.gz\"), and is in the SORTED (sort -k1,1 -k2,2n) 10x fragments format (CHR\tSTART\tEND\tBARCODE\tVALUE). See --fragments-output from the make-tree and matrix-output entry points for assistance in merging files." )
  prior <- optional $ option str ( long "prior" <> metavar "STRING" <> help "The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files." )
  delimiter <- delimiterParser
  labelsFile <- labelsFileParser
  peaksExcludeMatchFragments <- optional $ option str ( long "peaks-exclude-match-fragments" <> metavar "STRING" <> help "Exclude fragments from fragments.tsv.gz file if STRING is infix of fragment row. For instance, exclude all chrY fragments with \"--exclude-match-fragments chrY\"." )
  peaksBlacklistRegionsFile <- optional $ option str ( long "peaks-blacklist-regions-file" <> metavar "FILE" <> help "Bed file containing regions to ignore. Any fragments overlapping these regions are ignored if the input is not a fragments file." )
  peaksCustomRegion <- many $ option str ( long "peaks-custom-region" <> metavar "chrN:START-END" <> help "Only look at these regions in the matrix for chromosome region features. A list in the format `chrN:START-END`, will assign values for a cell intersecting this region to this region. If a cell region overlaps two or more regions, will be counting that many times in the new features. Example input: `--custom-regions \"chr1:1003-10064\" --custom-regions \"chr2:3021-5034\"` etc." )
  peakCallCommand <- option str ( long "peak-call-command" <> metavar "STRING" <> value "macs2 callpeak --nomodel --nolambda -p 0.001 -B -t %s -n %s --outdir %s" <> showDefault <> help "The command to call peaks with. Can be any command that will be run on each generated fragment file per cluster, but the first \"%s\" must be the input argument, second \"%s\" is the name of the sample, and the third \"%s\" should be the output directory. Uses macs2 by default. Must return a .narrowPeak file with each row being \"CHR\tSTART\tEND\t*\tVALUE\n\" at least (* can be anything, after VALUE there can be anything as well. Check macs2 output for guidance)." )
  genomecovCommand <- option str ( long "genomecov-command" <> metavar "STRING" <> value "bedtools genomecov -i %s -g %s -scale %f -bg -trackline > %s" <> showDefault <> help "The command to convert to coverage bedgraph output. Can be any command that will be run on each bed per cluster, but the first \"%s\" must be the input argument, the second \"%s\" is the genome file (see https://github.com/arq5x/bedtools2/tree/master/genomes), followed by the \"%f\" scaling argument, with the last \"%s\" as the output argument, in order. Uses bedtools genomecov by default." )
  genome <- option str ( long "genome" <> metavar "PATH" <> value "./human.hg38.genome" <> showDefault <> help "The location of the genome file for the --genomecov-command, see https://github.com/arq5x/bedtools2/tree/master/genomes" )
  skipFragments <- switch ( long "skip-fragments" <> help "Whether to skip the generation of the fragments (e.g. if changing only --peak-call-command and fragment separation by cluster already exists)." )
  peakNode <- many $ option auto ( long "peak-node" <> metavar "[NODE]" <> help "List of nodes to peak call, i.e. \"--peak-node 3 --peak-node 5 --peak-node 7\". If the node is not a leaf node, make sure to use --all-nodes in addition. Defaults to all leaf nodes." )
  peakNodeLabels <- many $ option str ( long "peakNodeLabels" <> metavar "[LABEL])" <> help "List of labels to keep in each node when outputting fragments and peaks, i.e. --peak-node-labels \"(3, [\\\"Red\\\"])\" --peak-node-labels \"(5, [\\\"Red\\\", \\\"Blue\\\"]. Nodes not listed will include all labels. Defaults to all labels." )
  allNodes <- switch ( long "allNodes" <> help "Whether to get fragments and peaks for all nodes, not just the leaves." )
  bedgraph <- switch ( long "bedgraph" <> help "Whether to output cluster normalized per million bedgraph output." )
  output <- outputParser

  pure $ PeaksCommand (Peaks {..})

data Motifs = Motifs { diffFile :: T.Text
             , backgroundDiffFile :: Maybe T.Text
             , motifGenome :: T.Text
             , motifCommand :: String
             , motifGenomeCommand :: Maybe String
             , topN :: Int
             , output :: String
             } deriving (Read, Show)

motifsParser :: Parser Subcommand
motifsParser = do
  diffFile <- option str ( long "diff-file" <> metavar "FILE" <> help "The input file containing the differential features between nodes. Must be in the format `node,feature,log2FC,pVal,qVal`. The node column is optional (if wanting to separate per node)." )
  backgroundDiffFile <- optional $ option str ( long "background-diff-file" <> metavar "FILE" <> help "The input file containing the differential features between nodes for use as a background in motif finding. Must be in the format `node,feature,log2FC,pVal,qVal`. The node column is optional (if wanting to separate per node). If using this argument, be sure to update the --motif-command appropriately (background file comes last, e.g. with homer use `/path/to/findMotifs.pl %s fasta %s -bgFasta %s`)." )
  motifGenome <- option str ( long "motif-genome" <> metavar "FILE" <> help "The location of the genome file in fasta format to convert bed to fasta." )
  motifCommand <- option str ( long "motif-command" <> metavar "STRING" <> value "meme %s -nmotifs 50 -oc %s" <> showDefault <> help "The command to find motifs in a fasta file. Can be any command that will be run on each fasta file converted from the bed optionally per node, but the first \"%s\" must be the input file, the second \"%s\" is the output. An example of homer: `/path/to/findMotifs.pl %s fasta %s`. Uses meme by default." )
  motifGenomeCommand <- optional $ option str ( long "motif-genome-command" <> metavar "STRING" <> help "The command to find motifs from a bed file instead of --motif-command (replaces that argument), as in homer's findMotifsGenome.pl. Can be any command that will be run on each bed file optionally per node, but the first \"%s\" must be the input file, the second \"%s\" is the genome file, and the last is the output. An example of homer: `/path/to/findMotifsGenome.pl %s %s %s`." )
  topN <- option auto ( long "top-n" <> metavar "INT " <> value 100 <> showDefault <> help "The top INT differentially expressed features." )
  output <- outputParser

  pure $ MotifsCommand (Motifs {..})

data MatrixOutput = MatrixOutput { loadMatrixOptions :: LoadMatrixOptions
                   , matOutput :: String
                   } deriving (Read, Show)

matrixOutputParser :: Parser Subcommand
matrixOutputParser = do
  loadMatrixOptions <- loadMatrixParser
  matOutput <- option str ( long "matOutput" <> metavar "FOLDER | FILE.csv" <> value "out_matrix" <> showDefault <> help "Output the filtered and normalized (not including TfIdfNorm) matrix in this folder in matrix market format or, if a csv file is specified, a dense csv format. Like input, features are rows." )

  pure $ MatrixOutputCommand (MatrixOutput {..})

data Spatial = Spatial { loadMatrixOptions :: LoadMatrixOptions
              , projectionFile :: Maybe String
              , labelsFile :: Maybe String
              , stateLabelsFile :: Maybe String
              , annoSpatMarkerFile :: Maybe String
              , annoSpatCommand :: String
              , mark :: [T.Text]
              , pcfCrossFlag :: Bool
              , onlySummaryFlag :: Bool
              , skipFinishedFlag :: Bool
              , output :: String
              } deriving (Read, Show)

spatialParser :: Parser Subcommand
spatialParser = do
  loadMatrixOptions <- loadMatrixParser
  labelsFile <- labelsFileParser
  projectionFile <- projectionParser
  stateLabelsFile <- optional $ option str ( long "state-labels-file" <> metavar "FILE" <> help "The input file containing a metadata label for sample (i.e. disease or control) to group samples by, with \"item,label\" header." )
  mark <- many $ option str ( long "mark" <> metavar "FEATURE | LABEL" <> help "Marks for the spatial relationship analysis. A list (`--mark MARK --mark MARK`) where `MARK` is either a feature such as a gene or protein or a label from the `--labels-file`. If the cells have labels (from `--labels-file`, `--annospat-marker-file`, or `--custom-label`), the `MARK` will be interpreted as a label. If `--mark ALL` (capitalized ALL, only ALL and nothing else) and `--labels-file` is given, then a pairwise comparison between labels will be computed, or if the `--labels-file` is not given then a pairwise comparison between features will be computed. If no marks are given, no spatial relationship will be carried out and only the interactive projection plot will be given." )
  annoSpatMarkerFile <- optional $ option str ( long "annospat-marker-file" <> metavar "STRING" <> help "The location of the AnnoSpat marker file for cell classification. Triggers the use of AnnoSpat to generate the labels file rather than --labels-file. Overrides the --labels-file argument." )
  annoSpatCommand <- option str ( long "annospat-command" <> metavar "STRING" <> value "AnnoSpat generateLabels -i %s -m %s -o %s -f %s -l %s -r %s -s \"\"" <> showDefault <> help "The AnnoSpat command to label cells. To customize, use the default value then follow the with custom additional arguments, making sure not to alter the default arguments used here." )
  pcfCrossFlag <- switch ( long "pcf-cross" <> help "Whether to use the multitype point correlation function (pcfcross) instead of the mark correlation function (markcrosscorr) for spatial relationships using categorical marks." )
  onlySummaryFlag <- switch ( long "summary-only" <> help "Whether to only calculate the summarization of the spatial statistics, for use only if skipping the spatial relationship calculations." )
  skipFinishedFlag <- switch ( long "skip-finished" <> help "Whether to skip the finished comparisons (contains the stats.csv file in the output folder)." )
  output <- outputParser

  pure $ SpatialCommand (Spatial {..})
