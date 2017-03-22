# KMEANS clustering
# ++++++++++++++++++++
library(cluster)
library(factoextra)
library(fields)
?clusGap
kmeans_gap_stat <- clusGap(Dcluster, FUN = kmeans, K.max = 30, B = 5, d.power = 2, verbose = T)
fviz_gap_stat(kmeans_gap_stat) # 4 clusters
print(kmeans_gap_stat)
kmeans.res1 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 1, graph = F ,hc_metric = "pearson"
                      ,verbose = T
                      )

fviz_cluster(kmeans.res1, geom = "point", frame.type = "convex", show.clust.cent = T, title = "KMeans Clustering K = 1")

kmeans.res2 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 2, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res2, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 2")

kmeans.res3 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 3, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res3, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 3")

kmeans.res4 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 4, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res4, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 4")

kmeans.res5 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 5, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res5, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 5")

kmeans.res6 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 6, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res6, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 6")

kmeans.res7 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 7, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res7, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 7")

kmeans.res8 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 8, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res8, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 8")

kmeans.res9 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 9, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res9, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 9")

kmeans.res10 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 10, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res10, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 10")

kmeans.res15 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 15, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res15, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 15")

kmeans.res20 <- eclust(Dcluster_scaled, FUNcluster = "kmeans", k = 20, graph = F ,hc_metric = "pearson"
                      ,verbose = T
)

fviz_cluster(kmeans.res20, geom = "point", frame.type = "norm", show.clust.cent = T, title = "KMeans Clustering K = 20")



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
kmeans_k7 <- tbl_df(kmeans.res7$cluster)
colnames(kmeans_k7) <- "kmeans_k7"
kmeans_k8 <- tbl_df(kmeans.res8$cluster)
colnames(kmeans_k8) <- "kmeans_k8"
kmeans_k9 <- tbl_df(kmeans.res9$cluster)
colnames(kmeans_k9) <- "kmeans_k9"
kmeans_k10 <- tbl_df(kmeans.res10$cluster)
colnames(kmeans_k10) <- "kmeans_k10"
kmeans_k15 <- tbl_df(kmeans.res15$cluster)
colnames(kmeans_k15) <- "kmeans_k15"
kmeans_k20 <- tbl_df(kmeans.res20$cluster)
colnames(kmeans_k20) <- "kmeans_k20"

ckmeans_k1 <- bind_cols(Dcluster, kmeans_k1)
ckmeans_k2 <- bind_cols(Dcluster, kmeans_k2)
ckmeans_k3 <- bind_cols(Dcluster, kmeans_k3)
ckmeans_k4 <- bind_cols(Dcluster, kmeans_k4)
ckmeans_k5 <- bind_cols(Dcluster, kmeans_k5)
ckmeans_k6 <- bind_cols(Dcluster, kmeans_k6)
ckmeans_k7 <- bind_cols(Dcluster, kmeans_k7)
ckmeans_k8 <- bind_cols(Dcluster, kmeans_k8)
ckmeans_k9 <- bind_cols(Dcluster, kmeans_k9)
ckmeans_k10 <- bind_cols(Dcluster, kmeans_k10)
ckmeans_k15 <- bind_cols(Dcluster, kmeans_k15)
ckmeans_k20 <- bind_cols(Dcluster, kmeans_k20)

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



colnames(Dcluster_scaled) <- paste(colnames(Dcluster_scaled),"_normalized", sep = '')
RATING <- 20
Dcluster_scaled %>%
  dplyr::mutate(CL_percentage_rank = percent_rank(Score_CL_normalized),
                CL_Rating = ntile(CL_percentage_rank,RATING))


Dcluster_scaled_rated <- Dcluster_scaled %>%
  dplyr::mutate(CL_percentage_rank = percent_rank(Score_CL_normalized),
                EM_percentage_rank = percent_rank(Score_EM_normalized),
                LE_percentage_rank = percent_rank(Score_LE_normalized),
                TQ_percentage_rank = percent_rank(Score_TQ_normalized),
                UB_percentage_rank = percent_rank(Score_UB_normalized),
                CL_Rating = ntile(CL_percentage_rank,RATING),
                EM_Rating = ntile(EM_percentage_rank,RATING),
                LE_Rating = ntile(LE_percentage_rank,RATING),
                TQ_Rating = ntile(TQ_percentage_rank,RATING),
                UB_Rating = ntile(UB_percentage_rank,RATING))
Dcluster_percentage_rank <- Dcluster_scaled_rated %>% select(contains("percentage_rank"))


DKMeansDistanceCentre <- bind_cols(res1, res2, res3, res4, res5, res6)
DKmeansCluster <- bind_cols(kmeans_k1, kmeans_k2, kmeans_k3, kmeans_k4, kmeans_k5, kmeans_k6
                            ,kmeans_k7, kmeans_k8, kmeans_k9, kmeans_k10, kmeans_k15, kmeans_k20)
DKmeans <- bind_cols(DKmeansCluster, DKMeansDistanceCentre)



write.csv(x = bind_cols(DtID_complete, DScore)
          ,file = 'scoring.csv'
          ,row.names = F)



write.csv(x = bind_cols(DtID_complete, Dcluster, Dcluster_scaled_rated, DKmeans)
          ,file = 'clustering-kmeans-20.csv'
          ,row.names = F)


