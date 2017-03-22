library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
################################################################################
## Prepare data
################################################################################
# Read USW Raw Scores
rawScores <- tbl_df(read.csv(file = "hull_custom_scores_POLRImputation.csv"
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

# Enhanced hierarchical clustering
res.hc_hclust_pearson <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                method = "ward.D2", graph = FALSE,
                                hc_metric = 'pearson', verbose = TRUE) 
#polr_hclust_pearson <- res.hc_hclust_pearson
summary(res.hc_hclust_pearson)
# Dendrogram
fviz_dend(res.hc_hclust_pearson, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Agglomerative Hierarchical Clustering with Pearson Measure:POLR Imputation") 
table(res.hc_hclust_pearson$cluster)

fviz_cluster(res.hc_hclust_pearson, frame.type = "norm", frame.level = 0.68)

write.csv(x = tbl_df(res.hc_hclust_pearson$cluster)
          ,file = 'hull_profiles_hclust_pearson_polr.csv'
          ,row.names = F)


# Enhanced hierarchical clustering
res.hc_hclust_spearman <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                 method = "ward.D2", graph = FALSE,
                                 hc_metric = 'spearman', verbose = TRUE) 
summary(res.hc_hclust_spearman)
# Dendrogram
fviz_dend(res.hc_hclust_spearman, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Agglomerative Hierarchical Clustering with Spearman Measure:POLR Imputation") 
table(res.hc_hclust_spearman$cluster)


# Enhanced hierarchical clustering
res.hc_hclust_euclidean <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                  method = "ward.D2", graph = FALSE,
                                  hc_metric = 'euclidean', verbose = TRUE) 
summary(res.hc_hclust_euclidean)
# Dendrogram
fviz_dend(res.hc_hclust_euclidean, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Agglomerative Hierarchical Clustering with Euclidean Measure:POLR Imputation") 
table(res.hc_hclust_euclidean$cluster)



fviz_cluster(res.hc_hclust_pearson, frame.type = "norm", frame.level = 0.68)
fviz_cluster(res.hc_hclust_spearman, frame.type = "norm", frame.level = 0.68)
fviz_cluster(res.hc_hclust_euclidean, frame.type = "norm", frame.level = 0.68)

RATING <- 20
clusterDataScaledRated <- tbl_df(clusterDataScaled) %>%
  dplyr::mutate(`CL percentage rank` = percent_rank(`CLT`),
                `EM percentage rank` = percent_rank(`EMT`),
                `LE percentage rank` = percent_rank(`LET`),
                `TQ percentage rank` = percent_rank(`TQT`),
                `UB percentage rank` = percent_rank(`UBT`),
                `CL Rating` = ntile(`CL percentage rank`,RATING),
                `EM Rating` = ntile(`EM percentage rank`,RATING),
                `LE Rating` = ntile(`LE percentage rank`,RATING),
                `TQ Rating` = ntile(`TQ percentage rank`,RATING),
                `UB Rating` = ntile(`UB percentage rank`,RATING))

# using hclust pearson as profiles
profiledRated<- cbind(clusterDataScaledRated, res.hc_hclust_pearson$cluster)


write.csv(x = profiledRated
          ,file = 'hull_rated_profiles_polr.csv'
          ,row.names = F)


