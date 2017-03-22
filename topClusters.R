# following the indexes studied in clusterSim_rca_pgarts_combined
# we found 4 different clustering methods 
library(dplyr)
library(magrittr)
combined_data <- tbl_df(read.csv(file = "rca_iss_pgarts_combined.csv"
                                 ,header = T
                                 ,check.names = F))

clusterData <- combined_data[,-c(1,2)]
str(clusterData)
clusterData %<>% mutate(n5CLT = CLT - mean(CLT)/max(CLT-mean(CLT)),
                        n5EMT = EMT - mean(EMT)/max(EMT-mean(EMT)),
                        n5LET = LET - mean(LET)/max(LET-mean(LET)),
                        n5TQT = TQT - mean(TQT)/max(TQT-mean(TQT)),
                        n5UBT = UBT - mean(UBT)/max(UBT-mean(UBT)),
                        n1CLT = CLT - mean(CLT)/sd(CLT),
                        n1EMT = EMT - mean(EMT)/sd(EMT),
                        n1LET = LET - mean(LET)/sd(LET),
                        n1TQT = TQT - mean(TQT)/sd(TQT),
                        n1UBT = UBT - mean(UBT)/sd(UBT),
                        n3CLT = CLT - mean(CLT)/max(CLT)-min(CLT),
                        n3EMT = EMT - mean(EMT)/max(EMT)-min(EMT),
                        n3LET = LET - mean(LET)/max(LET)-min(LET),
                        n3TQT = TQT - mean(TQT)/max(TQT)-min(TQT),
                        n3UBT = UBT - mean(UBT)/max(UBT)-min(UBT))


# 1) 2, n5 normalization, manhatann, pam
n5_manhatann_pam <- pam(x = dplyr::select(clusterData, starts_with("n5"))
                        ,k = 2,
                        , metric = "manhattan"
                        ,stand = F
                        )
# 2) 2, n1 normalization, squared euclidean, single
disMatrix <- dist(x = dplyr::select(clusterData, starts_with("n1")), method = "euclidean")
n1_euclidean_single <- hclust(d = disMatrix, method = "single")
n1_euclidean_single
plot(n1_euclidean_single, cex = 0.6, hang = -1)
plot(n1_euclidean_single, cex = 0.6, hang = -1,
       main = "Dendrogram of agnes") 
factoextra::fviz_dend(n1_euclidean_single)
library(factoextra)
# Enhanced hierarchical clustering
res.hc <- factoextra::eclust(dplyr::select(clusterData, starts_with("n1")), "hclust") # compute hclust

res.hc <- factoextra::eclust(scale(dplyr::select(clusterData, 1:5)), "hclust") # compute hclust

fviz_dend(res.hc, rect = TRUE, show_labels = F) # dendrogam

# 3) 5, n1 normalization, squared eucliedean, ward
# 4) 9, n3 normalization, GDM1, average
