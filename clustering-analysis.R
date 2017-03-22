library(cluster)
library(factoextra)


library(devtools)



################################################################################
## Prepare data
################################################################################
# score data which also include blank cells
fn <- "2 output ProfileData-2016-03-17 15_36_30-all-scored-total-scores.csv"
# score data with only complete cases i.e no blank cells
# fn <- "2 output ProfileData-2016-03-17 15_36_30-complete-scored-total-scores.csv"

library(dplyr)
scores <- tbl_df(read.csv(file = fn, header = T, check.names = F))
str(scores)
summary(scores)

# create another column 'division'
scores %<>%
  mutate(division = as.factor(substr(scores$`Global Response ID`, start = 1, stop = 4 ))) %>%
  select(everything())
scores

library(ggplot2)
library(dplyr)
cluster_data <- select(scores, c(`CL Score`, `EM Score`, `LE Score`, `TQ Score`, `UB Score`))
data.apac <- filter(scores, division == 'APAC')
data.emea <- filter(scores, division == 'EMEA')

data <- select(data.apac, c(`CL Score`, `EM Score`, `LE Score`, `TQ Score`, `UB Score`))


## Finding the optimal number of clusters
opt_clust_kmeans_wss <- fviz_nbclust(cluster_data, kmeans, method = "wss") # kmeans clustering
opt_clust_pam_wss <- fviz_nbclust(cluster_data, pam, method = "wss") # pam clustering
gc()
opt_clust_hclust_wss <- fviz_nbclust(cluster_data, hcut, method = "wss") # hierarchical clustering
# visualize them

opt_clust_kmeans_wss + geom_vline(xintercept = 5, linetype = 2) + geom_vline(xintercept = 6, linetype = 2) + theme_classic() + ggtitle("Optimal Nb of Clusters Global dataset  : K-Means method")
opt_clust_pam_wss + geom_vline(xintercept = 5, linetype = 2) + geom_vline(xintercept = 6, linetype = 2) + theme_classic() + ggtitle("Optimal Nb of Clusters Global : PAM (Partition Around Mediods) method")
opt_clust_hclust_wss + geom_vline(xintercept = 5, linetype = 2) + theme_classic() + ggtitle("Optimal Nb of Clusters Global: Hierarchical clustering method")

require(cluster)

opt_clust_kmeans_silh <- fviz_nbclust(data, kmeans, method = "silhouette", k.max = 7) 
fviz_silhouette(silhouette(pam(scale(data), k = 5)))
fviz_silhouette(silhouette(pam(scale(data), k = 6)))
plot(silhouette(pam(scale(data),6)), col = 2:5)
opt_clust_pam_silh <- fviz_nbclust(data, pam, method = "silhouette") 
opt_clust_hclust_silh <- fviz_nbclust(data, hcut, method = "silhouette", hc_method = "complete") 
opt_clust_kmeans_silh + theme_classic() + ggtitle("Optimal Nb of Clusters APAC : K-Means ")
opt_clust_pam_silh + theme_classic() + ggtitle("Optimal Nb of Clusters APAC: PAM (Partition Around Mediods) method")
opt_clust_hclust_silh + theme_classic() + ggtitle("Optimal Nb of Clusters APAC: Hierarchical clustering method")




## 6 seems to be the optimal number of cluster
cluster_data <- scores %>% filter(division == 'APAC') %>% select(c(`CL Score`, `EM Score`, `LE Score`, `TQ Score`, `UB Score`))
#cluster_data <- data.apac
km.res <- kmeans(cluster_data, 5, nstart = 25)
km.res
km.res$tot.withinss
# km.res <- pam(scale(df), 4)
#km.res
profiles_means <- aggregate(cluster_data, by=list(cluster=km.res$cluster), mean)
profiles_means %<>%
  mutate(CL = round(`CL Score`),
         EM = round(`EM Score`),
         LE = round(`LE Score`),
         TQ = round(`TQ Score`),
         UB = round(`UB Score`)) %>%
  mutate(profile = paste(CL,
                         EM,
                         LE,
                         TQ,
                         UB, sep = '-')) %>%
  select(cluster, profile, CL, EM, LE, TQ, UB)
profiles_means
#fviz_cluster(km.res, data = cluster_data, geom = c(""), frame.type = "convex", frame.level = 0.95) + theme_classic() + scale_fill_discrete(name="Profile (CL-EM-LE-TQ-UB)", labels= profiles_6means$profile) +guides(shape=guide_legend(title=NULL))+ ggtitle("Cluster K-Means Method: 2 clusters")

fviz_cluster(km.res, data = cluster_data, stand = T,geom = c("point"), frame.type = "convex", frame.level = 0.95) + theme_classic() + scale_fill_discrete(name="Profile (CL-EM-LE-TQ-UB)", labels= profiles_means$profile) + guides(shape=guide_legend(title=NULL)) + ggtitle("Cluster K-Means Method: 5 clusters")

fviz_cluster(km.res, data = cluster_data, geom = c("point"), frame = F) + theme_classic() + ggtitle("Cluster K-Means Method: 3 clusters")
km.res$centers
km.res$size
?fviz_cluster

## TRY TO USE PCA TO get the 3 dimenions data
library(rgl)
plot3d(scores[,2:4], col=km.res$cluster)



