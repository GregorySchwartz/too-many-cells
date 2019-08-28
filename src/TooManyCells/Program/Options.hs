{- TooManyCells.Program.Options
Gregory W. Schwartz

Options for the command line program.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TooManyCells.Program.Options where

-- Remote
import Options.Generic
import qualified Data.Text as T

-- Local

-- | Command line arguments
data Options
    = MakeTree { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (cellranger < 3 (matrix.mtx, genes.tsv, and barcodes.tsv) or cellranger >= 3 (matrix.mtx.gz, features.tsv.gz, and barcodes.tsv.gz) or an input csv file containing feature row names and cell column names. scATAC-seq is supported if input file contains \"fragments\" and ends with \".tsv.gz\" (such as \"fragments.tsv.gz\" or \"sample1_fragments.tsv.gz\") and is in the 10x fragments format (see also --binwidth, --no-binarize). If given as a list (--matrix-path input1 --matrix-path input2 etc.) then will join all matrices together. Assumes the same number and order of features in each matrix, so only cells are added."
               , binwidth :: Maybe Int <?> "([5000] | BINSIZE) If input data is comprised of scATAC-seq features, where each feature is a range of the genome, BINSIZE input is required to convert ranges to fixed width bins."
               , noBinarize :: Bool <?> "If a fragments.tsv.gz file, do not binarize data."
               , projectionFile :: Maybe String <?> "([Nothing] | FILE) The input file containing positions of each cell for plotting. Format with header is \"barcode,x,y\". Useful for 10x where a TNSE projection is generated in \"projection.csv\". Cells without projections will not be plotted. If not supplied, no plot will be made."
               , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
               , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
               , customLabel :: [T.Text] <?> "([] | [LABEL]) List of labels to assign each matrix if all cells from each matrix are given the same label per matrix. This argument intends to simplify the process of labeling by bypassing --labels-file if the user just wants each matrix to have its own label (i.e. sample). Must be the same length and order as --matrix-path: for instance, --matrix-path input1 --custom-label sample1 --matrix-path input2 --custom-label sample2 etc. will label all cells from input1 with sample1, input2 with sample2, etc. If there are multiple labels per matrix, you must use --labels-file."
               , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
               , featureColumn :: Maybe Int <?> "([1] | COLUMN) The column (1-indexed) in the features.tsv.gz file to use for feature names. If using matrix market format, cellranger stores multiple columns in the features file, usually the first column for the Ensembl identifier and the second column for the gene symbol. If the Ensembl identifier is not quickly accessible, use --feature-column 2 for the second column, which is usually more ubiquitous. Useful for overlaying gene expression so you can say --draw-leaf \"DrawItem (DrawContinuous \\\"CD4\\\")\") instead of --draw-leaf \"DrawItem (DrawContinuous \\\"ENSG00000010610\\\")\"). Does not affect CSV format (the column names will be the feature names)."
               , normalization :: Maybe String <?> "([TfIdfNorm] | UQNorm | MedNorm | TotalMedNorm | BothNorm | NoneNorm) Type of normalization before clustering. TfIdfNorm normalizes based on the prevalence of each feature. UQNorm normalizes each observation by the upper quartile non-zero counts of that observation. MedNorm normalizes each observation by the median non-zero counts of that observation. TotalMedNorm normalized first each observation by total count then by median of non-zero counts across features. BothNorm uses both normalizations (first TotalMedNorm for all analysis then additionally TfIdfNorm during clustering). NoneNorm does not normalize. Default is TfIdfNorm for clustering and NoneNorm for differential (which instead uses the recommended edgeR single cell preprocessing including normalization and filtering, any normalization provided here will result in edgeR preprocessing on top). Cannot use TfIdfNorm for any other process as NoneNorm will become the default."
               , eigenGroup :: Maybe String <?> "([SignGroup] | KMeansGroup) Whether to group the eigenvector using the sign or kmeans while clustering. While the default is sign, kmeans may be more accurate (but starting points are arbitrary)."
               , numEigen :: Maybe Int <?> "([1] | INT) Number of eigenvectors to use while clustering with kmeans. Takes from the second to last eigenvector. Recommended to start at 1 and work up from there if needed. May help offset the possible instability and inaccuracy of SVDLIBC."
               , minSize :: Maybe Int <?> "([1] | INT) The minimum size of a cluster. Defaults to 1."
               , maxStep :: Maybe Int <?> "([Nothing] | INT) Only keep clusters that are INT steps from the root. Defaults to all steps."
               , maxProportion :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE proportion split. So a node N with L and R children will stop with this criteria at 0.5 if |L| / |R| < 0.5 or > 2 (absolute log2 transformed), that is, if one child has over twice as many items as the other child. Includes L and R in the final result."
               , minModularity :: Maybe Double <?> "([Nothing] | DOUBLE) Nearly the same as --min-distance, but for clustering instead of drawing (so the output json tree can be larger). Stopping criteria to stop at the node with DOUBLE modularity. So a node N with L and R children will stop with this criteria the distance at N to L and R is < DOUBLE. Does not include L and R in the final result."
               , minDistance :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE distance. So a node N with L and R children will stop with this criteria the distance at N to L and R is < DOUBLE. Includes L and R in the final result."
               , minDistanceSearch :: Maybe Double <?> "([Nothing] | DOUBLE) Similar to --min-distance, but searches from the leaves to the root -- if a path from a subtree contains a distance of at least DOUBLE, keep that path, otherwise prune it. This argument assists in finding distant nodes."
               , smartCutoff :: Maybe Double <?> "([Nothing] | DOUBLE) Whether to set the cutoffs for --min-size, --max-proportion, --min-distance, and --min-distance-search based off of the distributions (median + (DOUBLE * MAD)) of all nodes. To use smart cutoffs, use this argument and then set one of the four arguments to an arbitrary number, whichever cutoff type you want to use. --min-proportion distribution is log2 transformed."
               , customCut :: [Int] <?> "([Nothing] | NODE) List of nodes to prune (make these nodes leaves). Invoked by --custom-cut 34 --custom-cut 65 etc."
               , dendrogramOutput :: Maybe String <?> "([dendrogram.svg] | FILE) The filename for the dendrogram. Supported formats are PNG, PS, PDF, and SVG."
               , matrixOutput :: Maybe String <?> "([Nothing] | FOLDER | FILE.csv) Output the filtered and normalized (not including TfIdfNorm) matrix in this folder under the --output directory in matrix market format or, if a csv file is specified, a dense csv format. Like input, features are rows. See --matrix-output-transpose."
               , matrixOutputTranspose :: Maybe String <?> "([Nothing] | FOLDER | FILE.csv) Output the filtered and normalized (not including TfIdfNorm) matrix in this folder under the --output directory in matrix market format or, if a csv file is specified, a dense csv format. Differs from --matrix-output in that features are columns.columns."
               , drawLeaf :: Maybe String <?> "([DrawText] | DrawItem DrawItemType) How to draw leaves in the dendrogram. DrawText is the number of items in that leaf. DrawItem is the collection of items represented by circles, consisting of: DrawItem DrawLabel, where each item is colored by its label, DrawItem (DrawContinuous [FEATURE]), where each item is colored by the expression of FEATURE (corresponding to a feature name in the input matrix, [FEATURE] is a list, so if more than one FEATURE is listed, uses the average of the feature values), DrawItem (DrawThresholdContinuous [(FEATURE, DOUBLE)]), where each item is colored by the binary high / low expression of FEATURE based on DOUBLE and multiple FEATUREs can be used to combinatorically label items (FEATURE1 high / FEATURE2 low, etc.), DrawItem DrawSumContinuous, where each item is colored by the sum of the post-normalized columns (use --normalization NoneNorm for UMI counts, default), and DrawItem DrawDiversity, where each node is colored by the diversity based on the labels of each item and the color is normalized separately for the leaves and the inner nodes. The default is DrawText, unless --labels-file is provided, in which DrawItem DrawLabel is the default. If the label or feature cannot be found, the default color will be black (check your spelling!)."
               , drawCollection :: Maybe String <?> "([PieChart] | PieRing | PieNone | CollectionGraph MAXWEIGHT THRESHOLD [NODE]) How to draw item leaves in the dendrogram. PieRing draws a pie chart ring around the items. PieChart only draws a pie chart instead of items. PieNone only draws items, no pie rings or charts. (CollectionGraph MAXWEIGHT THRESHOLD [NODE]) draws the nodes and edges within leaves that are descendents of NODE (empty list [] indicates draw all leaf networks) based on the input matrix, normalizes edges based on the MAXWEIGHT, and removes edges for display less than THRESHOLD (after normalization, so for CollectionGraph 2 0.5 [26], draw the leaf graphs for all leaves under node 26, then a edge of 0.7 would be removed because (0.7 / 2) < 0.5). For CollectionGraph with no colors, use --draw-leaf \"DrawItem DrawLabel\" and all nodes will be black. If you don't specify this option, DrawText from --draw-leaf overrides this argument and only the number of cells will be plotted."
               , drawMark :: Maybe String <?> "([MarkNone] | MarkModularity) How to draw annotations around each inner node in the tree. MarkNone draws nothing and MarkModularity draws a black circle representing the modularity at that node, darker black means higher modularity for that next split."
               , drawNodeNumber :: Bool <?> "Draw the node numbers on top of each node in the graph."
               , drawMaxNodeSize :: Maybe Double <?> "([72] | DOUBLE) The max node size when drawing the graph. 36 is the theoretical default, but here 72 makes for thicker branches."
               , drawMaxLeafNodeSize :: Maybe Double <?> "([--draw-max-node-size] | DOUBLE) The max leaf node size when drawing the graph. Defaults to the value of --draw-max-node-size."
               , drawNoScaleNodes :: Bool <?> "Do not scale inner node size when drawing the graph. Instead, uses draw-max-node-size as the size of each node and is highly recommended to change as the default may be too large for this option."
               , drawLegendSep :: Maybe Double <?> "([1] | DOUBLE) The amount of space between the legend and the tree."
               , drawLegendAllLabels :: Bool <?> "Whether to show all the labels in the label file instead of only showing labels within the current tree. The program generates colors from all labels in the label file first in order to keep consistent colors. By default, this value is false, meaning that only the labels present in the tree are shown (even though the colors are the same). The subset process occurs after --draw-colors, so when using that argument make sure to account for all labels."
               , drawPalette :: Maybe String <?> "([Set1] | Hsv | Ryb) Palette to use for legend colors. With high saturation in --draw-scale-saturation, consider using Hsv to better differentiate colors."
               , drawColors :: Maybe String <?> "([Nothing] | COLORS) Custom colors for the labels or continuous features. Will repeat if more labels than provided colors. For continuous feature plots, uses first two colors [high, low], defaults to [red, gray]. For instance: --draw-colors \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\""
               , drawDiscretize :: Maybe String <?> "([Nothing] | COLORS | INT) Discretize colors by finding the nearest color for each item and node. For instance, --draw-discretize \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\" will change all node and item colors to one of those two colors, based on Euclidean distance. If using \"--draw-discretize INT\", will instead take the default map and segment (or interpolate) it into INT colors, rather than a more continuous color scheme. May have unintended results when used with --draw-scale-saturation."
               , drawScaleSaturation :: Maybe Double <?> "([Nothing] | DOUBLE) Multiply the saturation value all nodes by this number in the HSV model. Useful for seeing more visibly the continuous colors by making the colors deeper against a gray scale."
               , pca :: Maybe Int <?> "([Nothing] | INT) Not recommended, as it makes cosine similarity less meaningful (therefore less accurate -- instead, consider making your own similarity matrix and using cluster-tree, our sister algorithm, to cluster the matrix and plot with birch-beer). The number of dimensions to keep for PCA dimensionality reduction before clustering. Default is no PCA at all in order to keep all information. Should use with --shift-positive to ensure no negative values."
               , noFilter :: Bool <?> "Whether to bypass filtering features and cells by low counts."
               , shiftPositive :: Bool <?> "Shift features to positive values. Positive values are shifted to allow modularity to work correctly."
               , filterThresholds :: Maybe String <?> "([(250, 1)] | (DOUBLE, DOUBLE)) The minimum filter thresholds for (MINCELL, MINFEATURE) when filtering cells and features by low read counts. See also --no-filter."
               , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
               , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity."
               , clumpinessMethod :: Maybe String <?> "([Majority] | Exclusive | AllExclusive) The method used when calculating clumpiness: Majority labels leaves according to the most abundant label, Exclusive only looks at leaves consisting of cells solely from one label, and AllExclusive treats the leaf as containing both labels."
               , dense :: Bool <?> "Whether to use dense matrix algorithms for clustering. Should be faster for dense matrices, so if batch correction, PCA, or other algorithms are applied upstream to the input matrix, consider using this option to speed up the tree generation."
               , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    | Interactive { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (cellranger < 3 (matrix.mtx, genes.tsv, and barcodes.tsv) or cellranger >= 3 (matrix.mtx.gz, features.tsv.gz, and barcodes.tsv.gz) or an input csv file containing feature row names and cell column names. scATAC-seq is supported if input file contains \"fragments\" and ends with \".tsv.gz\" (such as \"fragments.tsv.gz\" or \"sample1_fragments.tsv.gz\") and is in the 10x fragments format (see also --binwidth, --no-binarize). If given as a list (--matrix-path input1 --matrix-path input2 etc.) then will join all matrices together. Assumes the same number and order of features in each matrix, so only cells are added."
                  , binwidth :: Maybe Int <?> "([5000] | BINSIZE) If input data is comprised of scATAC-seq features, where each feature is a range of the genome, BINSIZE input is required to convert ranges to fixed width bins."
                  , noBinarize :: Bool <?> "If a fragments.tsv.gz file, do not binarize data."
                  , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
                  , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
                  , customLabel :: [T.Text] <?> "([] | [LABEL]) List of labels to assign each matrix if all cells from each matrix are given the same label per matrix. This argument intends to simplify the process of labeling by bypassing --labels-file if the user just wants each matrix to have its own label (i.e. sample). Must be the same length and order as --matrix-path: for instance, --matrix-path input1 --custom-label sample1 --matrix-path input2 --custom-label sample2 etc. will label all cells from input1 with sample1, input2 with sample2, etc. If there are multiple labels per matrix, you must use --labels-file."
                  , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
                  , featureColumn :: Maybe Int <?> "([1] | COLUMN) The column (1-indexed) in the features.tsv.gz file to use for feature names. If using matrix market format, cellranger stores multiple columns in the features file, usually the first column for the Ensembl identifier and the second column for the gene symbol. If the Ensembl identifier is not quickly accessible, use --feature-column 2 for the second column, which is usually more ubiquitous. Useful for overlaying gene expression so you can say --draw-leaf \"DrawItem (DrawContinuous \\\"CD4\\\")\") instead of --draw-leaf \"DrawItem (DrawContinuous \\\"ENSG00000010610\\\")\"). Does not affect CSV format (the column names will be the feature names)."
                  , normalization :: Maybe String <?> "([TfIdfNorm] | UQNorm | MedNorm | TotalMedNorm | BothNorm | NoneNorm) Type of normalization before clustering. TfIdfNorm normalizes based on the prevalence of each feature. UQNorm normalizes each observation by the upper quartile non-zero counts of that observation. MedNorm normalizes each observation by the median non-zero counts of that observation. TotalMedNorm normalized first each observation by total count then by median of non-zero counts across features. BothNorm uses both normalizations (first TotalMedNorm for all analysis then additionally TfIdfNorm during clustering). NoneNorm does not normalize. Default is TfIdfNorm for clustering and NoneNorm for differential (which instead uses the recommended edgeR single cell preprocessing including normalization and filtering, any normalization provided here will result in edgeR preprocessing on top). Cannot use TfIdfNorm for any other process as NoneNorm will become the default."
                  , pca :: Maybe Int <?> "([Nothing] | INT) Not recommended, as it makes cosine similarity less meaningful (therefore less accurate -- instead, consider making your own similarity matrix and using cluster-tree, our sister algorithm, to cluster the matrix and plot with birch-beer). The number of dimensions to keep for PCA dimensionality reduction before clustering. Default is no PCA at all in order to keep all information. Should use with --shift-positive to ensure no negative values."
                  , shiftPositive :: Bool <?> "Shift features to positive values. Positive values are shifted to allow modularity to work correctly."
                  , noFilter :: Bool <?> "Whether to bypass filtering features and cells by low counts."
                  , filterThresholds :: Maybe String <?> "([(250, 1)] | (DOUBLE, DOUBLE)) The minimum filter thresholds for (MINCELL, MINFEATURE) when filtering cells and features by low read counts. See also --no-filter."
                  , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."}
    | Differential { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (cellranger < 3 (matrix.mtx, genes.tsv, and barcodes.tsv) or cellranger >= 3 (matrix.mtx.gz, features.tsv.gz, and barcodes.tsv.gz) or an input csv file containing feature row names and cell column names. scATAC-seq is supported if input file contains \"fragments\" and ends with \".tsv.gz\" (such as \"fragments.tsv.gz\" or \"sample1_fragments.tsv.gz\") and is in the 10x fragments format (see also --binwidth, --no-binarize). If given as a list (--matrix-path input1 --matrix-path input2 etc.) then will join all matrices together. Assumes the same number and order of features in each matrix, so only cells are added."
                   , binwidth :: Maybe Int <?> "([5000] | BINSIZE) If input data is comprised of scATAC-seq features, where each feature is a range of the genome, BINSIZE input is required to convert ranges to fixed width bins."
                   , noBinarize :: Bool <?> "If a fragments.tsv.gz file, do not binarize data."
                   , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
                   , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
                   , customLabel :: [T.Text] <?> "([] | [LABEL]) List of labels to assign each matrix if all cells from each matrix are given the same label per matrix. This argument intends to simplify the process of labeling by bypassing --labels-file if the user just wants each matrix to have its own label (i.e. sample). Must be the same length and order as --matrix-path: for instance, --matrix-path input1 --custom-label sample1 --matrix-path input2 --custom-label sample2 etc. will label all cells from input1 with sample1, input2 with sample2, etc. If there are multiple labels per matrix, you must use --labels-file."
                   , featureColumn :: Maybe Int <?> "([1] | COLUMN) The column (1-indexed) in the features.tsv.gz file to use for feature names. If using matrix market format, cellranger stores multiple columns in the features file, usually the first column for the Ensembl identifier and the second column for the gene symbol. If the Ensembl identifier is not quickly accessible, use --feature-column 2 for the second column, which is usually more ubiquitous. Useful for overlaying gene expression so you can say --draw-leaf \"DrawItem (DrawContinuous \\\"CD4\\\")\") instead of --draw-leaf \"DrawItem (DrawContinuous \\\"ENSG00000010610\\\")\"). Does not affect CSV format (the column names will be the feature names)."
                   , pca :: Maybe Int <?> "([Nothing] | INT) Not recommended, as it makes cosine similarity less meaningful (therefore less accurate -- instead, consider making your own similarity matrix and using cluster-tree, our sister algorithm, to cluster the matrix and plot with birch-beer). The number of dimensions to keep for PCA dimensionality reduction before clustering. Default is no PCA at all in order to keep all information. Should use with --shift-positive to ensure no negative values."
                   , noFilter :: Bool <?> "Whether to bypass filtering features and cells by low counts."
                   , shiftPositive :: Bool <?> "Shift features to positive values. Positive values are shifted to allow modularity to work correctly."
                   , filterThresholds :: Maybe String <?> "([(250, 1)] | (DOUBLE, DOUBLE)) The minimum filter thresholds for (MINCELL, MINFEATURE) when filtering cells and features by low read counts. See also --no-filter."
                   , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
                   , normalization :: Maybe String <?> "([TfIdfNorm] | UQNorm | MedNorm | TotalMedNorm | BothNorm | NoneNorm) Type of normalization before clustering. TfIdfNorm normalizes based on the prevalence of each feature. UQNorm normalizes each observation by the upper quartile non-zero counts of that observation. MedNorm normalizes each observation by the median non-zero counts of that observation. TotalMedNorm normalized first each observation by total count then by median of non-zero counts across features. BothNorm uses both normalizations (first TotalMedNorm for all analysis then additionally TfIdfNorm during clustering). NoneNorm does not normalize. Default is TfIdfNorm for clustering and NoneNorm for differential (which instead uses the recommended edgeR single cell preprocessing including normalization and filtering, any normalization provided here will result in edgeR preprocessing on top). Cannot use TfIdfNorm for any other process as NoneNorm will become the default."
                   , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
                   , nodes :: String <?> "([NODE], [NODE]) Find the differential expression between cells belonging downstream of a list of nodes versus another list of nodes. Directionality is \"([1], [2])\" -> 2 / 1. \"([], [])\" switches the process to instead find the log2 average division between all nodes with all other cells (node / other cells) using the Kruskal-Wallis Test (--features does not work for this, --labels works, and UQNorm for the normalization is recommended. Only returns nodes where the comparison had both groups containing at least five cells.)."
                   , labels :: Maybe String <?> "([Nothing] | ([LABEL], [LABEL])) Use --labels-file to restrict the differential analysis to cells with these labels. Same format as --nodes, so the first list in --nodes and --labels gets the cells within that list of nodes with this list of labels. The same for the second list. For instance, --nodes \"([1], [2])\" --labels \"([\\\"A\\\"], [\\\"B\\\"])\" will compare cells from node 1 of label \"A\" only with cells from node 2 of label \"B\" only. To use all cells for that set of nodes, use an empty list, i.e. --labels \"([], [\\\"A\\\"])\". When comparing all nodes with all other cells, remember that the notation would be ([Other Cells], [Node]), so to compare cells of label X in Node with cells of label Y in Other Cells, use --labels \"([\\\"Y\\\", \\\"X\\\"])\". Requires both --labels and --labels-file, otherwise will include all labels."
                   , topN :: Maybe Int <?> "([100] | INT ) The top INT differentially expressed features."
                   , features :: [T.Text] <?> "([Nothing] | FEATURE) List of features (e.g. genes) to plot for all cells within selected nodes. Invoked by --features CD4 --features CD8 etc. When this argument is supplied, only the plot is outputted and edgeR differential expression is ignored. Outputs to --output."
                   , aggregate :: Bool <?> "([False] | True) Whether to plot the aggregate (mean here) of features for each cell from \"--features\" instead of plotting different distributions for each feature."
                   , plotOutput :: Maybe String <?> "([out.pdf] | STRING) The file containing the output plot."}
    | Diversity { priors :: [String] <?> "(PATH) Either input folders containing the output from a run of too-many-cells or a csv files containing the clusters for each cell in the format \"cell,cluster\". Advanced features not available in the latter case. If --labels-file is specified, those labels designate entity type, otherwise the assigned cluster is the entity type."
                , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
                , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
                , start :: Maybe Integer <?> "([0] | INT) For the rarefaction curve, start the curve at this subsampling."
                , interval :: Maybe Integer <?> "([1] | INT) For the rarefaction curve, the amount to increase each subsampling. For instance, starting at 0 with an interval of 4, we would sampling 0, 4, 8, 12, ..."
                , end :: Maybe Integer <?> "([N] | INT) For the rarefaction curve, which subsample to stop at. By default, the curve stops at the observed number of species for each population."
                , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity."
                , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    | Paths { prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
            , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
            , flipDirection :: Bool <?> "Flip the starting node when calculating the distances."
            , pathDistance :: Maybe String <?> "([PathModularity] | PathStep) How to measure the distance from the starting leaf. PathModularity weighs the steps by the modularity, while PathStep counts the number of steps."
            , bandwidth :: Maybe Double <?> "([1] | DOUBLE) Bandwidth of the density plot."
            , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
            , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "aggregate"             = Nothing
    short "atac"                  = Nothing
    short "clumpinessMethod"      = Just 'u'
    short "clusterNormalization"  = Just 'C'
    short "customCut"             = Nothing
    short "customLabel"           = Just 'Z'
    short "dendrogramOutput"      = Just 'U'
    short "drawCollection"        = Just 'E'
    short "drawColors"            = Just 'R'
    short "drawDendrogram"        = Just 'D'
    short "drawDiscretize"        = Nothing
    short "drawLeaf"              = Just 'L'
    short "drawLegendAllLabels"   = Just 'J'
    short "drawLegendSep"         = Just 'Q'
    short "drawMark"              = Just 'K'
    short "drawMaxLeafNodeSize"   = Nothing
    short "drawMaxNodeSize"       = Just 'A'
    short "drawNoScaleNodes"      = Just 'W'
    short "drawNodeNumber"        = Just 'N'
    short "drawPalette"           = Just 'Y'
    short "drawScaleSaturation"   = Just 'V'
    short "eigenGroup"            = Just 'B'
    short "featureColumn"         = Nothing
    short "filterThresholds"      = Just 'H'
    short "labels"                = Nothing
    short "matrixOutput"          = Nothing
    short "matrixOutputTranspose" = Nothing
    short "maxDistance"           = Just 'T'
    short "maxProportion"         = Just 'X'
    short "maxStep"               = Just 'S'
    short "minDistance"           = Nothing
    short "minDistanceSearch"     = Nothing
    short "minModularity"         = Nothing
    short "minSize"               = Just 'M'
    short "noFilter"              = Just 'F'
    short "normalization"         = Just 'z'
    short "numEigen"              = Just 'G'
    short "order"                 = Just 'O'
    short "pca"                   = Just 'a'
    short "plotOutput"            = Nothing
    short "priors"                = Just 'P'
    short "projectionFile"        = Just 'j'
    short "shiftPositive"         = Nothing
    short x                       = firstLetter x

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers
