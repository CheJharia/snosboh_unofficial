nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")


################################################################################
## Prepare data
################################################################################
# Read USW Raw Scores
rawScores <- tbl_df(read.csv(file = "apac-pca-scores.csv"
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
?scale
library("NbClust")
set.seed(1990)
gc()
nb <- NbClust(clusterData, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
gc()
nbc <- NbClust(clusterDataScaled, distance = "euclidean", min.nc = 4,
              max.nc = 10, method = "complete", index ="all")

##########


################################################################################
## Clustering - Hybrid Hierarchical K-Means
################################################################################

set.seed(1990)
gc()
# Compute hierarchical k-means clustering
res.hk <-hkmeans(clusterDataScaled, 1)
# Elements returned by hkmeans()
clust<- tbl_df(as.data.frame(res.hk$cluster))
names(clust) <- 'Clustering K = 1'
profiles <- clust
# Visualize the tree
# fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)

set.seed(1990)
for (i in 2:5) {
  gc()
  # Compute hierarchical k-means clustering
  res.hk <-hkmeans(clusterDataScaled, i)
  # Elements returned by hkmeans()
  clust <- tbl_df(as.data.frame(res.hk$cluster))
  names(clust) <- paste('Clustering K=', i)
  profiles <- cbind(profiles, clust)
  # Visualize the tree
  #fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)
  if (i == 5) {
    fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)
  }
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

profiledRated1<- cbind(rawScores[,1],clusterDataScaledRated, profiles)


####
################################################################################
## Clustering - Hybrid Hierarchical K-Means
################################################################################

set.seed(1990)
gc()
# Compute hierarchical k-means clustering
res.hk <-hkmeans(clusterData, 1)
# Elements returned by hkmeans()
clust<- tbl_df(as.data.frame(res.hk$cluster))
names(clust) <- 'Clustering K = 1'
profiles <- clust
# Visualize the tree
fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)

set.seed(1990)
for (i in 2:5) {
  gc()
  # Compute hierarchical k-means clustering
  res.hk <-hkmeans(clusterData, i)
  # Elements returned by hkmeans()
  clust <- tbl_df(as.data.frame(res.hk$cluster))
  names(clust) <- paste('Clustering K=', i)
  profiles <- cbind(profiles, clust)
  # Visualize the tree
  #fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)
  if (i == 5) {
    fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)
  }
}

RATING <- 20
clusterDataRated <- tbl_df(clusterData) %>%
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

profiledRated2<- cbind(rawScores[,1],clusterDataRated, profiles)
