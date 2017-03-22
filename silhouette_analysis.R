library(cluster)
library(factoextra)


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


write.csv(x = polr_hclust_pearson$silinfo$widths
          ,file = 'silhouette_polr.csv'
          ,row.names = F)

sil_polr <- silhouette(polr_hclust_pearson$cluster, get_dist(clusterDataScaled, method = "pearson"))
fviz_silhouette(polr_hclust_pearson)
?fviz_silhouette
# Silhouette plot
plot(sil_polr, main ="Silhouette plot - POLR ")

# Summary of silhouette analysis
sil_polr.sum <- summary(sil_polr)
# Average silhouette width of each cluster
sil_polr.sum$clus.avg.widths

# The total average (mean of all individual silhouette widths)
sil_polr.sum$avg.width
# The size of each clusters
sil_polr.sum$clus.sizes

# Average silhouette width of each cluster
polr_hclust_pearson$silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
polr_hclust_pearson$silinfo$avg.width
# The size of each clusters
polr_hclust_pearson$size
summary(polr_hclust_pearson)

################################################################################


rawScores <- tbl_df(read.csv(file = "hull_custom_scores_randomForestImputation.csv"
                             ,header = T
                             ,check.names = F))
#clusterData <- rawScores
clusterData <- dplyr::select(rawScores, c(CLT,EMT,LET,TQT,UBT))
# scale the data
clusterDataScaled <- tbl_df(scale(clusterData))
write.csv(x = rf_hclust_pearson$silinfo$widths
          ,file = 'silhouette_rf.csv'
          ,row.names = F)
rf_hclust_pearson
sil_rf <- silhouette(rf_hclust_pearson$cluster, get_dist(clusterDataScaled, method = "pearson"))
fviz_silhouette(rf_hclust_pearson)

# Silhouette plot
plot(sil_rf, main ="Silhouette plot - POLR ")

# Summary of silhouette analysis
sil_rf.sum <- summary(sil_rf)
# Average silhouette width of each cluster
sil_rf.sum$clus.avg.widths

# The total average (mean of all individual silhouette widths)
sil_rf.sum$avg.width
# The size of each clusters
sil_rf.sum$clus.sizes

# Average silhouette width of each cluster
rf_hclust_pearson$silinfo$clus.avg.widths
# The total average (mean of all individual silhouette widths)
rf_hclust_pearson$silinfo$avg.width
# The size of each clusters
rf_hclust_pearson$size
summary(rf_hclust_pearson)

