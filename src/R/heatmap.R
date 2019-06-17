suppressPackageStartupMessages(library(cellrangerRkit))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(reshape2))
#suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(scales))

# Get paths.
args = commandArgs(TRUE)
cellrangerPath = as.character(args[1])
clusterPath = as.character(args[2])
savePath = as.character(args[3])

# Get load files.
cMat = load_cellranger_matrix(cellrangerPath)
cDf = read.csv(clusterPath)

# Get final matrix.
eDf = as.matrix(exprs(cMat))
eDf = eDf[rowSums(eDf) > 0,]
eDf = scale(eDf)

print("Combining names.")
new = setNames(as.character(cDf$cluster), as.character(cDf$cell))
colnames(eDf) = new[colnames(eDf)]

eDf = melt(eDf)
names(eDf)[names(eDf) == "Var1"] = "feature"
names(eDf)[names(eDf) == "Var2"] = "cluster"

df = eDf

# Average features in a cluster.
print("Averaging.")
df = plyr::ddply(df, c("cluster", "feature"), summarize, avgVal = mean(value))

# Plot.
print("Plotting.")

p = ggplot(df, aes(x = factor(cluster), y = factor(feature), fill = avgVal)) +
    geom_tile() +
  scale_fill_gradient2(high = muted("red"), mid = "white", low = muted("blue"), guide = guide_legend(title = "Z-score Expression")) +
    xlab("Cluster") +
    ylab("Feature") +
    theme(axis.text.x = element_text(angle = 315, hjust = 0))

# Save.
#ggsave(p, file = savePath, useDingbats = FALSE)
ggsave(p, file = savePath)
