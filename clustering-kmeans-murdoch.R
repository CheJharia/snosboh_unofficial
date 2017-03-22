# KMEANS clustering
# ++++++++++++++++++++
library(cluster)
library(factoextra)
library(fields)
?clusGap
kmeans_gap_stat <- clusGap(Dcluster_percentage_rank, FUN = kmeans, K.max = 10, B = 500, d.power = 2, verbose = T)
fviz_gap_stat(kmeans_gap_stat) # 4 clusters
print(kmeans_gap_stat)
kmeans.res1 <- eclust(Dcluster_percentage_rank, FUNcluster = "kmeans", k = 1, graph = F ,hc_metric = "spearman"
                      ,verbose = T
)
# Visualize pam clustering
fviz_cluster(kmeans.res1, geom = "point", frame.type = "convex", show.clust.cent = T, title = "KMeans Clustering K = 1")

kmeans.res2 <- eclust(Dcluster_percentage_rank, FUNcluster = "kmeans", k = 2, graph = F ,hc_metric = "spearman"
                      ,verbose = T
)
# Visualize pam clustering
fviz_cluster(kmeans.res2, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 2")

kmeans.res3 <- eclust(Dcluster_percentage_rank, FUNcluster = "kmeans", k = 3, graph = F ,hc_metric = "spearman"
                      ,verbose = T
)
# Visualize pam clustering
fviz_cluster(kmeans.res3, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 3")

kmeans.res4 <- eclust(Dcluster_percentage_rank, FUNcluster = "kmeans", k = 4, graph = F ,hc_metric = "spearman"
                      ,verbose = T
)
# Visualize pam clustering
fviz_cluster(kmeans.res4, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 4")

kmeans.res5 <- eclust(Dcluster_percentage_rank, FUNcluster = "kmeans", k = 5, graph = F ,hc_metric = "spearman"
                      ,verbose = T
)
# Visualize pam clustering
fviz_cluster(kmeans.res5, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 5")

kmeans.res6 <- eclust(Dcluster_percentage_rank, FUNcluster = "kmeans", k = 6, graph = F ,hc_metric = "spearman"
                      ,verbose = T
)

# Visualize pam clustering
fviz_cluster(kmeans.res6, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 6")

table(kmeans.res6$cluster)


kmeans_k1 <- tbl_df(kmeans.res1$cluster)
colnames(kmeans_k1) <- "kmeans_k1"
kmeans_k2 <- tbl_df(kmeans.res2$cluster)
colnames(kmeans_k2) <- "kmeans_k2"
kmeans_k3 <- tbl_df(kmeans.res3$cluster)
colnames(kmeans_k3) <- "kmeans_k3"
kmeans_k4 <- tbl_df(kmeans.res4$cluster)
colnames(kmeans_k4) <- "kmeans_k4"
kmeans_k5 <- tbl_df(kmeans.res5$cluster)
colnames(kmeans_k5) <- "kmeans_k5"
kmeans_k6 <- tbl_df(kmeans.res6$cluster)
colnames(kmeans_k6) <- "kmeans_k6"


ckmeans_k1 <- bind_cols(Dcluster, kmeans_k1)
ckmeans_k2 <- bind_cols(Dcluster, kmeans_k2)
ckmeans_k3 <- bind_cols(Dcluster, kmeans_k3)
ckmeans_k4 <- bind_cols(Dcluster, kmeans_k4)
ckmeans_k5 <- bind_cols(Dcluster, kmeans_k5)
ckmeans_k6 <- bind_cols(Dcluster, kmeans_k6)


res1 <- NULL # init
res1 <- abs(rdist(ckmeans_k1[1,1:5], t(kmeans.res1$centers[as.integer(ckmeans_k1[1,6]),]))) # 1st value
for (i in 2:nrow(ckmeans_k1)) {
  a <- abs(rdist(ckmeans_k1[i,1:5], t(kmeans.res1$centers[as.integer(ckmeans_k1[i,6]),])))
  res1 <- rbind(res1,a)
}
res1 <- tbl_df(res1)
colnames(res1) <- "Distance_from_cluster_centre_K1"

res2 <- NULL # init
res2 <- abs(rdist(ckmeans_k2[1,1:5], t(kmeans.res2$centers[as.integer(ckmeans_k2[1,6]),]))) # 1st value
for (i in 2:nrow(ckmeans_k2)) {
  a <- abs(rdist(ckmeans_k2[i,1:5], t(kmeans.res2$centers[as.integer(ckmeans_k2[i,6]),])))
  res2 <- rbind(res2,a)
}
res2 <- tbl_df(res2)
colnames(res2) <- "Distance_from_cluster_centre_K2"

res3 <- NULL # init
res3 <- abs(rdist(ckmeans_k3[1,1:5], t(kmeans.res3$centers[as.integer(ckmeans_k3[1,6]),]))) # 1st value
for (i in 2:nrow(ckmeans_k3)) {
  a <- abs(rdist(ckmeans_k3[i,1:5], t(kmeans.res3$centers[as.integer(ckmeans_k3[i,6]),])))
  res3 <- rbind(res3,a)
}
res3 <- tbl_df(res3)
colnames(res3) <- "Distance_from_cluster_centre_K3"


res4 <- NULL # init
res4 <- abs(rdist(ckmeans_k4[1,1:5], t(kmeans.res4$centers[as.integer(ckmeans_k4[1,6]),]))) # 1st value
for (i in 2:nrow(ckmeans_k4)) {
  a <- abs(rdist(ckmeans_k4[i,1:5], t(kmeans.res4$centers[as.integer(ckmeans_k4[i,6]),])))
  res4 <- rbind(res4,a)
}
res4 <- tbl_df(res4)
colnames(res4) <- "Distance_from_cluster_centre_K4"

res5 <- NULL # init
res5 <- abs(rdist(ckmeans_k5[1,1:5], t(kmeans.res5$centers[as.integer(ckmeans_k5[1,6]),]))) # 1st value
for (i in 2:nrow(ckmeans_k5)) {
  a <- abs(rdist(ckmeans_k5[i,1:5], t(kmeans.res5$centers[as.integer(ckmeans_k5[i,6]),])))
  res5 <- rbind(res5,a)
}
res5 <- tbl_df(res5)
colnames(res5) <- "Distance_from_cluster_centre_K5"


res6 <- NULL # init
res6 <- abs(rdist(ckmeans_k6[1,1:5], t(kmeans.res6$centers[as.integer(ckmeans_k6[1,6]),]))) # 1st value
for (i in 2:nrow(ckmeans_k6)) {
  a <- abs(rdist(ckmeans_k6[i,1:5], t(kmeans.res6$centers[as.integer(ckmeans_k6[i,6]),])))
  res6 <- rbind(res6,a)
}
res6 <- tbl_df(res6)
colnames(res6) <- "Distance_from_cluster_centre_K6"


DKMeansDistanceCentre <- bind_cols(res1, res2, res3, res4, res5, res6)
DKmeansCluster <- bind_cols(kmeans_k1, kmeans_k2, kmeans_k3, kmeans_k4, kmeans_k5, kmeans_k6)
DKmeans <- bind_cols(DKmeansCluster, DKMeansDistanceCentre)



write.csv(x = bind_cols(DtID_complete %>% filter(`Link Name` == 'Murdoch'), DScore)
          ,file = 'scoring-murdoch.csv'
          ,row.names = F)



write.csv(x = bind_cols(DtID_complete %>% filter(`Link Name` == 'Murdoch'), Dcluster, Dcluster_percentage_rank, DKmeans)
          ,file = 'clustering-kmeans-murdoch.csv'
          ,row.names = F)


