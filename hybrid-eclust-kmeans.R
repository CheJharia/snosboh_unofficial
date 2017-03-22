library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
################################################################################
## Prepare data
################################################################################
# Read USW Raw Scores
rawScores <- tbl_df(read.csv(file = "wsu-custom-apac-pca-scores.csv"
                             ,header = T
                             ,check.names = F))
rawScores
str(rawScores)
summary(rawScores)
colnames(rawScores)

#clusterData <- rawScores
clusterData <- dplyr::select(rawScores, c(CLT,EMT,LET,TQT,UBT))
# scale the data
clusterDataScaled <- tbl_df(scale(clusterData))

distmatrix <- dist(clusterDataScaled)
save.image()
# Correlation based distance measures
typeof(iclara)
# Compute correlation matrix
gc()
res.cor.pearson <- cor(t(clusterDataScaled),  method = "pearson")
gc()
save.image()
res.cor.kendall <- cor(t(clusterDataScaled),  method = "kendall")
gc()
save.image()
res.cor.spearman <- cor(t(clusterDataScaled),  method = "spearman")
gc()
save.image()
?cor
# Compute distance matrix
dist.cor.pearson <- as.dist(1 - res.cor.pearson)
dist.cor.kendall <- as.dist(1 - res.cor.kendall)
dist.cor.spearman <- as.dist(1 - res.cor.spearman)
rm(dist.cor.pearson)
rm(dist.cor.kendall)
rm(dist.cor.spearman)
rm(res.cor.pearson)
rm(res.cor.spearman)
rm(res.cor.kendall)
gc()
save.image()
tbl_df(dist.cor.kendall)[1:5,1:5]
# Enhanced hierarchical clustering
res.hc <- eclust(distmatrix, "hclust", k = 5,
                 method = "ward.D2", graph = FALSE) 

res.hc_hclust_pearson <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                 method = "ward.D2", graph = FALSE,
                 hc_metric = 'pearson', verbose = TRUE) 
summary(res.hc_hclust_pearson)
# Dendrogram
fviz_dend(res.hc_hclust_pearson, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Cluster Dendogram : Agglomerative Hierarchical Clustering with Pearson Measure") 
table(res.hc_hclust_pearson$cluster)
# Visualize the hkmeans final clusters
fviz_cluster(res.hc_hclust_pearson, frame.type = "norm", frame.level = 0.68)

res.hc_hclust_euclidean <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                method = "ward.D2", graph = FALSE,
                                hc_metric = 'euclidean', verbose = TRUE) 
# Dendrogram
fviz_dend(res.hc_hclust_euclidean, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Cluster Dendogram : Agglomerative Hierarchical Clustering with Euclidean Measure") 

head(res.hc$cluster, 15)

hclust_euclid_pearson <- cbind(res.hc_hclust_euclidean$cluster, res.hc_hclust_pearson$cluster, clusterDataScaled)
write.csv(x = hclust_euclid_pearson
          ,file = 'hclust_euclid_pearson.csv'
          ,row.names = F)

#write.csv(x = tbl_df(res.hc_hclust_pearson$cluster)
#          ,file = 'wsu_hclust_pearson.csv'
#          ,row.names = F)
#          
#write.csv(x = tbl_df(res.hc_hclust_pearson$cluster)
#          ,file = 'wsu_custom_apac_hclust_pearson.csv'
#          ,row.names = F)
