library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
################################################################################
## Prepare data
################################################################################
# Read Utas Raw Scores
rawScores <- tbl_df(read.csv(file = "utas_custom_apac_pca_scores.csv"
                             ,header = T
                             ,check.names = F))
rawScores
str(rawScores)
summary(rawScores)
colnames(rawScores)

#clusterData <- rawScores
clusterData <- dplyr::select(rawScores, c(CLT,EMT,LET,TQT,UBT))
# scale the data
clusterDataScaled <- tbl_df(scale(clusterData, center = T, scale = T))
str(clusterDataScaled)
summary(clusterDataScaled)
colnames(clusterDataScaled)
# Enhanced hierarchical clustering
res.hc_hclust_pearson <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                method = "ward.D2", graph = F,
                                hc_metric = 'pearson', verbose = TRUE) 
summary(res.hc_hclust_pearson)
# Dendrogram
fviz_dend(res.hc_hclust_pearson, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Cluster Dendogram : Agglomerative Hierarchical Clustering with Pearson Measure") 
table(res.hc_hclust_pearson$cluster)
# Visualize the hkmeans final clusters
fviz_cluster(res.hc_hclust_pearson, frame.type = "norm", frame.level = 0.68)


# Enhanced hierarchical clustering
res.hc_hclust_spearman <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                 method = "ward.D2", graph = FALSE,
                                 hc_metric = 'spearman', verbose = TRUE) 
summary(res.hc_hclust_spearman)
# Dendrogram
fviz_dend(res.hc_hclust_spearman, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Cluster Dendogram : Agglomerative Hierarchical Clustering with Spearman Measure") 
table(res.hc_hclust_spearman$cluster)
# Visualize the hkmeans final clusters
fviz_cluster(res.hc_hclust_pearson, frame.type = "norm", frame.level = 0.68)

write.csv(x = tbl_df(res.hc_hclust_pearson$cluster)
          ,file = 'utas_custom_apac_hclust_pearson.csv'
          ,row.names = F)
