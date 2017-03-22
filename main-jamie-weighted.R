library(cluster)
library(factoextra)
library(fields)
library(dplyr)
library(dplyr)
library(broom)
library(tidyr)

clusterData <- tbl_df(read.csv(file = "worcester-raw-scores.csv"
                               ,header = T
                               ,check.names = F))
str(clusterData)
summary(clusterData)
str(scale(x = clusterData)[,])
summary(scale(x = clusterData)[,])


################################################################################
# FINDING OPTIMUM NUMBER OF CLUSTERS
################################################################################
# +++ Using NbClust package to determine the Best Number of Clusters in a Data Set
# It provides 30 indexes for determining the optimal number of clusters in a data set and offers
# the best clustering scheme from different results to the user.
# NbClust package provides 30 indices for determining the number of clusters and proposes to user
# the best clustering scheme from the different results obtained by varying all combinations of number
# of clusters, distance measures, and clustering methods
#
library(NbClust)

res <- NbClust(clusterData, distance = "euclidean", min.nc = 2, max.nc = 15,
               method = "ward.D2", index = "alllong")


kmeans_gap_stat <- clusGap(clusterData, FUN = kmeans, K.max = 20, B = 100, d.power = 2, verbose = T)
fviz_gap_stat(kmeans_gap_stat)

kclusts <- data.frame(k = 4:20) %>%
  group_by(k) %>%
  do(kclust = kmeans(clusterData, .$k))

clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], clusterData))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))

library(ggfortify)
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line(color = "blue", alpha = 0.5, size = 2) +
  geom_point(size = 0.8)

clusterData[] <- lapply(clusterData[], as.numeric)

set.seed(1990)
nb_k <- 5
kmeans.res <- kmeans(clusterData, nb_k)
table(kmeans.res$cluster)
kmeans.res$centers
library(ggradar)
library(scales)
tbl_df(kmeans.res$centers) %>%
  add_rownames( var = "profile") %>%
  mutate_each(funs(rescale), -profile) -> kmeans_radar
colnames(kmeans_radar)[-1] <- c("Cost of Living", "Employment"
                                ,"Lifestyle", "Teaching Quality", "University Brand")
colorpalette <- RColorBrewer::brewer.pal(nrow(kmeans_radar), "Set1")
ggradar(kmeans_radar, axis.label.size = 2.5) + ggtitle("Spider Chart - Profiles comparison") + theme(text = element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom") + scale_colour_manual(values = colorpalette)

for (pr in 1:nrow(kmeans_radar)) {
  print(pr)
  print(ggradar(kmeans_radar[pr,], axis.label.size = 2.5) + ggtitle( paste("Spider Chart - Profile ", pr, sep = '')) + theme(text = element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom") + scale_colour_manual(values = colorpalette[pr]))
}

#write.csv(x = kmeans.res$cluster
#          ,file = 'kmeans-cluster.csv'
#          ,row.names = F)

autoplot(kmeans.res, data = clusterData, size = 4, alpha = 0.7) +
  ggtitle( paste("K-Means Clustering of Worcester Score dataset N = ", nrow(clusterData), sep = '') ) +
  theme(legend.position = "top") + scale_colour_manual(values = colorpalette)

# Silhouhette for kmeans
fviz_silhouette(silhouette(kmeans.res$cluster, dist(clusterData))) + scale_color_manual(values = colorpalette)
table(kmeans.res$cluster)

# assign ISS respodents the cluster corresponding to the closest center found in kmeans.res
library(flexclust)
flexclust::predict(kmeans.res, clusterData)


##############################################################################
# Random Forest model to train classification
library(caret)
library(randomForest)
train <- bind_cols(clusterData, tbl_df(kmeans.res$cluster))
colnames(train)[6] <- "profile"
train$profile <- as.factor(train$profile)
# need to convert profile label to letters for training model below
levels(train$profile) <- letters[1:nrow(kmeans_radar)]

# Train the model using a "random forest" algorithm
model10rf <- train(profile ~ CLT+EMT+LET+TQT+UBT, # profile is a function of the themes we decided to include
                   data = train, # Use the trainSet dataframe as the training data
                   method = "rf",# Use the "random forest" algorithm
                   trControl = trainControl(method = "repeatedcv", # Use cross-validation
                                            number = 10
                                            ,verboseIter = F
                                            ,repeats = 10
                                            ,savePredictions = "all"
                                            ,classProbs = T
                                            ,selectionFunction = "oneSE")
                   ,metric = "Kappa"
)
model10rf

iss_emea <- read.table("EMEA Scores.csv", sep = ",", header = TRUE, check.names = F)
iss_emea$pred.rf = predict(model10rf, iss_emea[,-c(1,2)] * 20, "raw")
table(iss_emea$pred.rf)
levels(iss_emea$pred.rf) <- paste(1:nrow(kmeans_radar))
levels(train$profile) <- paste(1:nrow(kmeans_radar))
write.csv(x = iss_emea
          ,file = 'emea_worcester_model_rf.csv'
          ,row.names = F)
iss_emea[,-c(1,2)] %>% group_by(pred.rf) %>%
  summarise_each(funs(mean))




iss_emea[,-c(1,2)] %>% group_by(pred.rf) %>%
    summarise_each(funs(mean)) -> iss_kmeans_radar

colnames(iss_kmeans_radar) <- c("profile","Cost of Living", "Employment"
                                ,"Lifestyle", "Teaching Quality", "University Brand")

ggradar(iss_kmeans_radar, axis.label.size = 2.5) + ggtitle("Spider Chart (EMEA with Worcester Model) - Profiles comparison") + theme(text = element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom") + scale_colour_manual(values = colorpalette)

for (predpr in 1:length(levels(droplevels(iss_emea$pred.rf)))) {
  clb <- as.integer(levels(droplevels(iss_emea$pred.rf))[predpr])
  print(ggradar((iss_kmeans_radar)[predpr,],
                axis.label.size = 2.5) + ggtitle( paste("Spider Chart (EMEA with Worcester Model) - Profile ", predpr, sep = '')) + theme(text = element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom") + scale_colour_manual(values = colorpalette[clb]))
}


