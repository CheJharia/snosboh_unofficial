library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
################################################################################
## Prepare data
################################################################################
# Read USW Raw Scores
rawScores <- tbl_df(read.csv(file = "emea-usw-raw-scores.csv"
                             ,header = T
                             ,check.names = F))
rawScores
str(rawScores)
summary(rawScores)
colnames(rawScores)

#clusterData <- rawScores
clusterData <- dplyr::select(rawScores, c(CLT,EMT,LET,TQT,UBT))
# scale the data
clusterDataScaled <- scale(clusterData)

################################################################################
## Clustering - Hybrid Hierarchical K-Means
################################################################################

set.seed(1990)
# Compute hierarchical k-means clustering
res.hk <-hkmeans(clusterDataScaled, 1)
# Elements returned by hkmeans()
clust<- tbl_df(as.data.frame(res.hk$cluster))
names(clust) <- 'Clustering K = 1'
profiles <- clust
# Visualize the tree
fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)

set.seed(1990)
for (i in 2:5) {
  # Compute hierarchical k-means clustering
  res.hk <-hkmeans(clusterDataScaled, i)
  # Elements returned by hkmeans()
  clust <- tbl_df(as.data.frame(res.hk$cluster))
  names(clust) <- paste('Clustering K=', i)
  profiles <- cbind(profiles, clust)
  # Visualize the tree
  fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)
}

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

profiledRated<- cbind(clusterDataScaledRated, profiles)


write.csv(x = profiledRated
          ,file = 'USW-rated-profiles-1-to-5-clusters.csv'
          ,row.names = F)
