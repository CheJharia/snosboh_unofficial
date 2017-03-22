# PAM clustering
# ++++++++++++++++++++
library(cluster)
library(factoextra)
library(fields)

pam_gap_stat <- clusGap(Dcluster, FUN = pam, K.max = 20, B = 5, d.power = 2, verbose = T)
fviz_gap_stat(pam_gap_stat) # 4 clusters


pam.res1 <- pam(Dcluster, 1)
# Visualize pam clustering
fviz_cluster(pam.res1, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 1")

pam.res2 <- pam(Dcluster, 2)
# Visualize pam clustering
fviz_cluster(pam.res2, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 2")

pam.res3 <- pam(Dcluster, 3)
# Visualize pam clustering
fviz_cluster(pam.res3, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 3")

pam.res4 <- pam(Dcluster, 4)
# Visualize pam clustering
fviz_cluster(pam.res4, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 4")

pam.res5 <- pam(Dcluster, 5)
# Visualize pam clustering
fviz_cluster(pam.res5, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 5")

pam.res6 <- pam(Dcluster, 6)
# Visualize pam clustering
fviz_cluster(pam.res6, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 6")

pam.res7 <- pam(Dcluster, 7)
# Visualize pam clustering
fviz_cluster(pam.res7, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 7")

pam.res8 <- pam(Dcluster, 8)
# Visualize pam clustering
fviz_cluster(pam.res8, geom = "point", frame.type = "norm", show.clust.cent = T, title = "PAM Clustering K = 8")


pam_k1 <- tbl_df(pam.res1$clustering)
colnames(pam_k1) <- "pam_k1"
pam_k2 <- tbl_df(pam.res2$clustering)
colnames(pam_k2) <- "pam_k2"
pam_k3 <- tbl_df(pam.res3$clustering)
colnames(pam_k3) <- "pam_k3"
pam_k4 <- tbl_df(pam.res4$clustering)
colnames(pam_k4) <- "pam_k4"
pam_k5 <- tbl_df(pam.res5$clustering)
colnames(pam_k5) <- "pam_k5"
pam_k6 <- tbl_df(pam.res6$clustering)
colnames(pam_k6) <- "pam_k6"
pam_k7 <- tbl_df(pam.res7$clustering)
colnames(pam_k7) <- "pam_k7"
pam_k8 <- tbl_df(pam.res8$clustering)
colnames(pam_k8) <- "pam_k8"


cpam_k1 <- bind_cols(Dcluster, pam_k1)
cpam_k2 <- bind_cols(Dcluster, pam_k2)
cpam_k3 <- bind_cols(Dcluster, pam_k3)
cpam_k4 <- bind_cols(Dcluster, pam_k4)
cpam_k5 <- bind_cols(Dcluster, pam_k5)
cpam_k6 <- bind_cols(Dcluster, pam_k6)
cpam_k7 <- bind_cols(Dcluster, pam_k7)
cpam_k8 <- bind_cols(Dcluster, pam_k8)


res1 <- NULL # init
res1 <- abs(rdist(cpam_k1[1,1:5], t(pam.res1$medoids[as.integer(cpam_k1[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k1)){
  a <- abs(rdist(cpam_k1[i,1:5], t(pam.res1$medoids[as.integer(cpam_k1[i,6]),])))
  res1 <- rbind(res1,a)
}
res1 <- tbl_df(res1)
colnames(res1) <- "Quality_K1"

res2 <- NULL # init
res2 <- abs(rdist(cpam_k2[1,1:5], t(pam.res2$medoids[as.integer(cpam_k2[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k2)){
  a <- abs(rdist(cpam_k2[i,1:5], t(pam.res2$medoids[as.integer(cpam_k2[i,6]),])))
  res2 <- rbind(res2,a)
}
res2 <- tbl_df(res2)
colnames(res2) <- "Quality_K2"

res3 <- NULL # init
res3 <- abs(rdist(cpam_k3[1,1:5], t(pam.res3$medoids[as.integer(cpam_k3[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k3)){
  a <- abs(rdist(cpam_k3[i,1:5], t(pam.res3$medoids[as.integer(cpam_k3[i,6]),])))
  res3 <- rbind(res3,a)
}
res3 <- tbl_df(res3)
colnames(res3) <- "Quality_K3"


res4 <- NULL # init
res4 <- abs(rdist(cpam_k4[1,1:5], t(pam.res4$medoids[as.integer(cpam_k4[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k4)){
  a <- abs(rdist(cpam_k4[i,1:5], t(pam.res4$medoids[as.integer(cpam_k4[i,6]),])))
  res4 <- rbind(res4,a)
}
res4 <- tbl_df(res4)
colnames(res4) <- "Quality_K4"

res5 <- NULL # init
res5 <- abs(rdist(cpam_k5[1,1:5], t(pam.res5$medoids[as.integer(cpam_k5[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k5)){
  a <- abs(rdist(cpam_k5[i,1:5], t(pam.res5$medoids[as.integer(cpam_k5[i,6]),])))
  res5 <- rbind(res5,a)
}
res5 <- tbl_df(res5)
colnames(res5) <- "Quality_K5"

res6 <- NULL # init
res6 <- abs(rdist(cpam_k6[1,1:5], t(pam.res6$medoids[as.integer(cpam_k6[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k6)){
  a <- abs(rdist(cpam_k6[i,1:5], t(pam.res6$medoids[as.integer(cpam_k6[i,6]),])))
  res6 <- rbind(res6,a)
}
res6 <- tbl_df(res6)
colnames(res6) <- "Quality_K6"

res7 <- NULL # init
res7 <- abs(rdist(cpam_k7[1,1:5], t(pam.res7$medoids[as.integer(cpam_k7[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k7)){
  a <- abs(rdist(cpam_k7[i,1:5], t(pam.res7$medoids[as.integer(cpam_k7[i,6]),])))
  res7 <- rbind(res7,a)
}
res7 <- tbl_df(res7)
colnames(res7) <- "Quality_K7"

res8 <- NULL # init
res8 <- abs(rdist(cpam_k8[1,1:5], t(pam.res8$medoids[as.integer(cpam_k8[1,6]),]))) # 1st value
for(i in 2:nrow(cpam_k8)){
  a <- abs(rdist(cpam_k8[i,1:5], t(pam.res8$medoids[as.integer(cpam_k8[i,6]),])))
  res8 <- rbind(res8,a)
}

res8 <- tbl_df(res8)
colnames(res8) <- "Quality_K8"


DQuality <- bind_cols(res1, res2, res3, res4, res5, res6, res7, res8)
DPamClusters <- bind_cols(pam_k1, pam_k2, pam_k3, pam_k4, pam_k5, pam_k6 ,pam_k7 ,pam_k8)
DPams <- bind_cols(DPamClusters, DQuality)

write.csv(x = bind_cols(DtID_complete, DScore)
          ,file = 'scoring.csv'
          ,row.names = F)

colnames(Dcluster_scaled) <- paste(colnames(Dcluster_scaled),"_normalized", sep = '')

write.csv(x = bind_cols(DtID_complete, Dcluster, Dcluster_scaled, DPams)
          ,file = 'clustering.csv'
          ,row.names = F)
