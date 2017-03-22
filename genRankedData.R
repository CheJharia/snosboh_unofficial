tmp1 <- clusterGeneration::genRandomClust(numClust = 5, sepVal = 0.7182818, numNoisy = 0, numOutlier = 0, numReplicate = 3, clustszind = 2, clustSizeEq = 400, outputDatFlag = F, outputEmpirical = F, outputInfo = F, outputLogFlag = F)

c <- 0
Ddummyranked <- vector(length = 50000)
for (p in 1:5000){
  min <- 1 + c
  max <- 10 + c
  while(TRUE){
    runif(3)
    ix <- sample(1:3,1, prob = runif(3))
    vcx <- tmp1$memList[[ix]]
    nv <- unique(sample(vcx, size = 5, replace = F))
    if(length(nv) == 5) break
  }
  pbs <- runif(10)
  pbst <- pbs/sum(pbs)
  Ddummyranked[sample(x = min:max, size = 5, replace = F, prob = pbst)] <- nv
  c <- c + 10
}
Ddummyranked[is.na(Ddummyranked)] <- 0
length(Ddummyranked)
Ddummyranked <- dplyr::tbl_df(matrix(Ddummyranked, ncol = 40, byrow = T))
summary(Ddummyranked)

colnames(Ddummyranked) <- c("CP1",
                  "CP2",
                  "TQ1",
                  "TQ2",
                  "GO1",
                  "GO2",
                  "LE1",
                  "LE2",
                  "BR1",
                  "BR2",
                  "CP3",
                  "CP4",
                  "TQ3",
                  "TQ4",
                  "GO3",
                  "GO4",
                  "LE3",
                  "LE4",
                  "BR3",
                  "BR4",
                  "CP5",
                  "CP6",
                  "TQ5",
                  "TQ6",
                  "GO5",
                  "GO6",
                  "LE5",
                  "LE6",
                  "BR5",
                  "BR6",
                  "CP7",
                  "CP8",
                  "TQ7",
                  "TQ8",
                  "GO7",
                  "GO8",
                  "LE7",
                  "LE8",
                  "BR7",
                  "BR8")
Ddummyranked$CP_score <- Ddummyranked %>% dplyr::select(starts_with("CP")) %>% rowSums(.)
Ddummyranked$TQ_score <- Ddummyranked %>% dplyr::select(starts_with("TQ")) %>% rowSums(.)
Ddummyranked$GO_score <- Ddummyranked %>% dplyr::select(starts_with("GO")) %>% rowSums(.)
Ddummyranked$LE_score <- Ddummyranked %>% dplyr::select(starts_with("LE")) %>% rowSums(.)
Ddummyranked$BR_score <- Ddummyranked %>% dplyr::select(starts_with("BR")) %>% rowSums(.)
DScore <- Ddummyranked %>% dplyr::select(ends_with("score"))
DScore <- DScore / 36
DScore.scaled <- scale(DScore)[,]
library(tidyverse)
library(cluster)
library(factoextra)
library(fields)
library(ggradar)
library(scales)
library(nsprcomp)
library(dplyr)
library(broom)
library(tidyr)
library(NbClust)

kclusts <- data.frame(k = 4:10) %>%
  group_by(k) %>%
  do(kclust = kmeans(DScore, .$k))

clusters <- kclusts %>% group_by(k) %>% do(tidy(.$kclust[[1]]))
assignments <- kclusts %>% group_by(k) %>% do(augment(.$kclust[[1]], DScore))
clusterings <- kclusts %>% group_by(k) %>% do(glance(.$kclust[[1]]))


ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line(color = "blue", alpha = 0.5, size = 2) +
  geom_point(size = 0.8)


nb_k <- 5
kmeans.res <- kmeans(DScore, nb_k)
table(kmeans.res$cluster)
kmeans.res$centers
library(ggfortify)
DScore[] <- lapply(DScore[], as.numeric)


tbl_df(kmeans.res$centers) %>%
  add_rownames( var = "profile") %>%
  mutate_each(funs(rescale), -profile) -> kmeans_radar

tbl_df(kmeans.res$centers) %>%
  add_rownames( var = "profile") -> kmeans_radar


colorpalette <- RColorBrewer::brewer.pal(nrow(kmeans_radar), "Set1")
autoplot(kmeans.res, data = DScore, size = 2, alpha = 0.7) +
  ggtitle( paste("K-Means Clustering of Dummy Ranked Score dataset N = ", nrow(DScore), sep = '') ) +
  theme(legend.position = "top") + scale_colour_manual(values = colorpalette)
dev.copy(png,'scatter-plot.png', width = 1280, height = 1024, res = 200)
dev.off()

# Silhouhette for kmeans
fviz_silhouette(silhouette(kmeans.res$cluster, dist(DScore))) + scale_color_manual(values = colorpalette)
table(kmeans.res$cluster)
dev.copy(png,'silhouette-plot.png', width = 1280, height = 1024, res = 200)
dev.off()

svdata <- bind_cols(Ddummyranked, tbl_df(kmeans.res$cluster))
colnames(svdata) <- c("The course has affordable tuition fee options (e.g. payment plans)",
                      "I can easily meeting the entry requirements",
                      "The course offers high quality teaching",
                      "The course is well-ranked",
                      "The course has a high graduate employment rate",
                      "The course includes a work placement",
                      "The course has flexible hours and delivery",
                      "I will be studying with like-minded people",
                      "The course has a good reputation",
                      "The course has good student satisfaction ratings",
                      "The country has an affordable cost of living",
                      "I can get a visa to study in that country",
                      "The country has universities with high quality teaching",
                      "The country has well-ranked universities",
                      "The country has good employment options for graduates",
                      "I can get a visa to work after graduating",
                      "I have friends or family living in that country",
                      "The lifestyle in that country appeals to me",
                      "The country has a good reputation as a place to study",
                      "The country is welcoming to international students",
                      "The university has affordable halls of residence / university owned accommodation",
                      "The university offers scholarships",
                      "The university offers high quality teaching",
                      "The university is well-ranked",
                      "The university has a high graduate employment rate",
                      "The university has a good careers service and links with employers",
                      "I have friends or family who have been to that university",
                      "I will be able to make friends with people from different countries and cultures",
                      "The university has a prestigious brand",
                      "The university is welcoming to international students",
                      "The town or city has affordable private accommodation options",
                      "I can work while studying",
                      "The town or city has universities with high quality teaching",
                      "The town or city has well-ranked universities",
                      "I will have access to preferred employers in that town or city",
                      "I can get a visa to work in that country after I graduate",
                      "I have friends or family living in that city",
                      "The lifestyle and leisure opportunities in that town or city",
                      "The city has a good reputation as a place to study",
                      "The city is a safe and welcoming place for international students",
                      "Score - CP (Cost & Practicalities)",
                      "Score - TQ (Teaching Quality)",
                      "Score - GO (Graduate Outcomes)",
                      "Score - LE (Lifestyle & Connections)",
                      "Score - BR (Brand & Reputation)",
                      "Profile")

write.csv(x = svdata
          ,file = 'ranked_data_profiled.csv', row.names = T)

library(fmsb)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(grid)
library(gridBase)
library(scales)

maxmin <- data.frame(
  CP = c(36, 0),
  TQ = c(36, 0),
  GO = c(36, 0),
  LE = c(36, 0),
  BR = c(36, 0))
colnames(kmeans_radar)[-1] <- c("CP", "TQ", "GO", "LE", "BR")
test11 <- rbind(maxmin, kmeans_radar[,-1] * 36)
colnames(test11) <- c("Cost & Practicalities", "Teaching Quality"
                      ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
colorpalette <- RColorBrewer::brewer.pal(nrow(kmeans_radar), "Set1")
unlist(lapply(colorpalette, adjustcolor, 0.5))
radarchart(test11,
           pty = 32,
           axistype = 0,
           pcol = unlist(lapply(colorpalette, adjustcolor, 0.5)),
           pfcol = unlist(lapply(colorpalette, adjustcolor, 0.5)),
           plty = 1,
           plwd = 3,
           cglty = 1,
           cglcol = "gray88",
           centerzero = TRUE,
           seg = 5,
           vlcex = 0.75,
           palcex = 0.75,
           title = "Spider Charts of 5 profiles")
dev.copy(png,'5-profiles-rankedData.png', width = 1280, height = 1024, res = 200)
dev.off()

for (pr in 1:nrow(kmeans_radar)) {
  spd <- rbind(maxmin, (kmeans_radar[,-1] * 36) [pr,])
  colnames(spd) <- c("Cost & Practicalities", "Teaching Quality"
                        ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
  radarchart(spd,
             pty = 32,
             axistype = 0,
             pcol = unlist(lapply(colorpalette, adjustcolor, 0.5))[pr],
             pfcol = unlist(lapply(colorpalette, adjustcolor, 0.5))[pr],
             plty = 1,
             plwd = 3,
             cglty = 1,
             cglcol = "gray88",
             centerzero = TRUE,
             seg = 5,
             vlcex = 0.75,
             palcex = 0.75,
             title = paste("Spider Chart - Profile ", pr, sep = ''))
  dev.copy(png,paste('profile-ranked-',pr,'.png',sep = ''), width = 1280, height = 1024, res = 200)
  dev.off()
}
