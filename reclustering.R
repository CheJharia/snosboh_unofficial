library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
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
clusterDataScaled <- as.data.frame(tbl_df(scale(clusterData)))
?scale
#http://www.sthda.com/english/wiki/how-to-choose-the-appropriate-clustering-algorithms-for-your-data-unsupervised-machine-learning
library(clValid)
clmethods <- c("hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara", "agnes")
gc()
clValidations <- c("internal","stability")
intern <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                  clMethods = clmethods, validation = clValidations)

# Summary
summary(intern)
?clValid
# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(clusterDataScaled, nClust = 5, clMethods = clmethods,
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)



library(factoextra)
set.seed(123)
fviz_nbclust(clusterDataScaled, hkmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

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


for (i in 2:10) {
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
    #fviz_dend(res.hk, rect = TRUE, show_labels = FALSE, cex = 0.9)
  }
}

RATING <- 10
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

profiledRated<- cbind(rawScores[,1],clusterDataScaledRated)
write.csv(x = profiledRated
          ,file = 'wsu_custom_apac_rated.csv'
          ,row.names = F)

write.csv(x = profiledRated
          ,file = 'APAC-rated-profiles-1-to-5-clusters.csv'
          ,row.names = F)
