library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)
################################################################################
## Prepare data
################################################################################
# Read RCA Raw Scores
rawScores <- tbl_df(read.csv(file = "complete_data.csv"
                             ,header = T
                             ,check.names = F))
rawScores
str(rawScores)
summary(rawScores)
colnames(rawScores)
yarrr::pirateplot(formula = CLT ~ LET, data = rawScores)
#clusterData <- rawScores
clusterData <- dplyr::select(rawScores, c(CLT,EMT,LET,TQT,UBT))
# scale the data
# By default it the matrix returned will contain additional attributes. If you don’t want these attributes, add, [,] to the function or returned object.
#clusterDataScaled <- tbl_df(scale(clusterData)[,])
#str(as.data.frame(clusterDataScaled))
# Enhanced hierarchical clustering
res.hc_hclust_pearson <- eclust(as.data.frame(clusterData), "hclust", k = 5,
                                method = "ward.D2", graph = FALSE,
                                hc_metric = 'pearson', verbose = TRUE) 
#clusterRating <- dplyr::select(clusterDataScaledRated, c(`CL Rating`,`EM Rating`,`LE Rating`,`TQ Rating`,`UB Rating`))
#res.hc_hclust_pearson <- eclust(as.data.frame(clusterRating), "hclust", k = 5,
#                               method = "ward.D2", graph = FALSE,
#                                hc_metric = 'pearson', verbose = TRUE) 

#rf_hclust_pearson <- res.hc_hclust_pearson
summary(res.hc_hclust_pearson)
# Dendrogram
fviz_dend(res.hc_hclust_pearson, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Agglomerative Hierarchical Clustering with Pearson Measure:RF Imputation") 
table(res.hc_hclust_pearson$cluster)

fviz_cluster(res.hc_hclust_pearson, frame.type = "norm", frame.level = 0.68)

write.csv(x = tbl_df(res.hc_hclust_pearson$cluster)
          ,file = 'rca_profiles_hclust_pearson_rf_raw.csv'
          ,row.names = F)












# Enhanced hierarchical clustering
res.hc_hclust_spearman <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                 method = "ward.D2", graph = FALSE,
                                 hc_metric = 'spearman', verbose = TRUE) 
summary(res.hc_hclust_spearman)
# Dendrogram
fviz_dend(res.hc_hclust_spearman, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Agglomerative Hierarchical Clustering with Spearman Measure:RF Imputation") 
table(res.hc_hclust_spearman$cluster)


# Enhanced hierarchical clustering
res.hc_hclust_euclidean <- eclust(as.data.frame(clusterDataScaled), "hclust", k = 5,
                                  method = "ward.D2", graph = FALSE,
                                  hc_metric = 'euclidean', verbose = TRUE) 
summary(res.hc_hclust_euclidean)
# Dendrogram
fviz_dend(res.hc_hclust_euclidean, rect = TRUE, show_labels = FALSE, cex = 0.5
          , main = "Agglomerative Hierarchical Clustering with Euclidean Measure:RF Imputation") 
table(res.hc_hclust_euclidean$cluster)



fviz_cluster(res.hc_hclust_pearson, frame.type = "norm", frame.level = 0.68)
fviz_cluster(res.hc_hclust_spearman, frame.type = "norm", frame.level = 0.68)
fviz_cluster(res.hc_hclust_euclidean, frame.type = "norm", frame.level = 0.68)



profiledRated<- tbl_df(cbind(clusterDataScaled, res.hc_hclust_pearson$cluster))
str(as.data.frame(profiledRated))
colnames(profiledRated)[6] <- "profile"
yarrr::pirateplot(formula = CLT ~ profile, data = profiledRated, main = "CLT profile - Scale")
yarrr::pirateplot(formula = EMT ~ profile, data = profiledRated, main = "EMT profile - Scale")
yarrr::pirateplot(formula = LET ~ profile, data = profiledRated, main = "LET profile - Scale")
yarrr::pirateplot(formula = TQT ~ profile, data = profiledRated, main = "TQT profile - Scale")
yarrr::pirateplot(formula = UBT ~ profile, data = profiledRated, main = "UBT profile - Scale")

profiledRated<- tbl_df(cbind(rawScores[,-c(1)], res.hc_hclust_pearson$cluster))
str(as.data.frame(profiledRated))
colnames(profiledRated)[6] <- "profile"
yarrr::pirateplot(formula = CLT ~ profile, data = profiledRated, main = "CLT profile - raw")
yarrr::pirateplot(formula = EMT ~ profile, data = profiledRated, main = "EMT profile - raw")
yarrr::pirateplot(formula = LET ~ profile, data = profiledRated, main = "LET profile - raw")
yarrr::pirateplot(formula = TQT ~ profile, data = profiledRated, main = "TQT profile - raw")
yarrr::pirateplot(formula = UBT ~ profile, data = profiledRated, main = "UBT profile - raw")











RATING <- 20
sort(unique(clusterDataScaled$CLT))
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






write.csv(x = profiledRated
          ,file = 'hull_rated_profiles_rf.csv'
          ,row.names = F)

