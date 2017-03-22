clusterData <- tbl_df(read.csv(file = "MMU_raw_scores.csv"
                                 ,header = T
                                 ,check.names = F))
library(dplyr)
library(factoextra)
clusterData <- clusterData[,-c(1)]
str(clusterData)
summary(clusterData)
str(scale(x = clusterData)[,])
summary(scale(x = clusterData)[,])
# Enhanced hierarchical clustering
res.hc <- factoextra::eclust(scale(x = clusterData)[,], "hclust", k = 6) # compute hclust

diana_res.hc <- factoextra::eclust(scale(x = clusterData)[,] 
                                   ,FUNcluster = "diana"
                                   ,hc_method = "ward.D2"
                                   ,verbose = T
                                   ,nboot = 600
                                   ,k = 5
) 

fviz_dend(diana_res.hc, rect = TRUE, show_labels = F, main = "Divisive Analysis (DIANA) Clustering Dendogram", type = "triangle") # dendrogam

fviz_dend(diana_res.hc, rect = TRUE, show_labels = F, main = "Divisive Analysis (DIANA) Clustering Dendogram") # dendrogam

agnes_res.hc <- factoextra::eclust(scale(x = clusterData)[,]  
                                   ,FUNcluster = "agnes"
                                   ,hc_method = "ward.D2"
                                   ,verbose = T
                                   ,nboot = 600
                                   ,k = 5)
fviz_dend(agnes_res.hc, rect = TRUE, show_labels = F, main = "Agglomerative Nesting (AGNES) Clustering Dendogram", type = "triangle") # dendrogam
fviz_dend(agnes_res.hc, rect = TRUE, show_labels = F, main = "Agglomerative Nesting (AGNES) Clustering Dendogram") # dendrogam
gc()
clara.hc <- factoextra::eclust(scale(dplyr::select(clusterData, 1:5)) 
                               ,FUNcluster = "clara", graph = T
                               ,hc_method = "ward.D2"
                               ,verbose = T
                               ,nboot = 600
                               ,k = 5)
# Show points only
fviz_cluster(clara.hc, clusterData, geom = "point") + theme_minimal() + ggtitle("Clustering Large Applications (CLARA)")
gc()
fanny.hc <- factoextra::eclust(scale(dplyr::select(clusterData, 1:5)) 
                               ,FUNcluster = "fanny", graph = T
                               ,hc_method = "ward.D2"
                               ,verbose = T
                               ,nboot = 600
                               ,k = 5)
# Show points only
fviz_cluster(fanny.hc, clusterData, geom = "point") + theme_minimal() + ggtitle("Fuzzy Analysis Clustering (FANNY)")

gc()
pam.hc <- factoextra::eclust(scale(dplyr::select(clusterData, 1:5)) 
                             ,FUNcluster = "pam", graph = T
                             ,hc_method = "ward.D2"
                             ,verbose = T
                             ,nboot = 600
                             ,k = 6)
# Show points only
fviz_cluster(pam.hc, clusterData, geom = "point") + theme_minimal() + ggtitle("Partitioning Around Medoids (PAM)")

?eclust

gc()
kmeans.hc <- factoextra::eclust(scale(dplyr::select(clusterData, 1:5)) 
                                ,FUNcluster = "kmeans", graph = T
                                ,hc_method = "ward.D2"
                                ,verbose = T
                                ,nboot = 600
                                ,k = 5)
fviz_cluster(kmeans.hc, clusterData, geom = "point") + theme_minimal() + ggtitle("Llyod's clustering (K-MEANS)")
kmeans.hc$cluster

library(dplyr)
RATING <- 10
clusterData.scaled <- scale(clusterData)[,]
clusterDataScaledRated <- tbl_df(clusterData.scaled) %>%
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
kmeans_profile <- as.data.frame(kmeans.hc$cluster)
colnames(kmeans_profile) <- "kmeans-cluster"
pam_profile <- as.data.frame(pam.hc$cluster)
colnames(pam_profile) <- "pam-cluster"
fanny_profile <- as.data.frame(fanny.hc$cluster)
colnames(fanny_profile) <- "fanny-cluster"
clara_profile <- as.data.frame(clara.hc$cluster)
colnames(clara_profile) <- "clara-cluster"
agnes_profile <- as.data.frame(agnes_res.hc$cluster)
colnames(agnes_profile) <- "agnes-cluster"
diana_profile <- as.data.frame(diana_res.hc$cluster)
colnames(diana_profile) <- "diana-cluster"
write.csv(x = cbind(clusterDataScaledRated, kmeans_profile, pam_profile
                    ,fanny_profile
                    ,clara_profile
                    ,agnes_profile
                    ,diana_profile)
          ,file = 'mmu_profiled_rated.csv'
          ,row.names = F)
#
# Silhouhette for kmeans
fviz_silhouette(silhouette(kmeans.hc$cluster, dist(clusterData.scaled)))
# Silhouette for PAM
fviz_silhouette(silhouette(pam.hc$clustering, dist(clusterData.scaled)))
# Silhouette for hierarchical clustering
fviz_silhouette(silhouette(agnes_res.hc$cluster, dist(clusterData.scaled)))
# Silhouette for hierarchical clustering
fviz_silhouette(silhouette(diana_res.hc$cluster, dist(clusterData.scaled)))
