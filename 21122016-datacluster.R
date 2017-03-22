
# DScore.rawSum
nb_k <- 4

kmeans.res <- factoextra::eclust(tbl_df(DScore.rawSum) , FUNcluster = "kmeans", k = nb_k, graph = F
                                 ,verbose = T
                                 ,stand = F # apply scale() to DScore
)
fviz_cluster(kmeans.res, geom = "point", frame.type = "convex", show.clust.cent = T,
             title = paste("KMeans Clustering K = ", nb_k, sep = '')
)

colorpalette <- RColorBrewer::brewer.pal(kmeans.res$nbclust, "Set1")
colorpalette.adjust <- as.vector(sapply(colorpalette, function(x) adjustcolor(x,0.5), simplify = T))

fviz_silhouette(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rawSum)))) + scale_color_manual(values = colorpalette)

tbl_df(kmeans.res$centers) %>%
  rownames_to_column( var = "profile" ) -> kmeans_radar

Dmax.themes <- tbl_df(sapply(kmeans_radar[,-1], function(x) max(x)))
maxmin <- data.frame(c(as.integer(Dmax.themes[1,1]), 0)
                     ,c(as.integer(Dmax.themes[2,1]), 0)
                     ,c(as.integer(Dmax.themes[3,1]), 0)
                     ,c(as.integer(Dmax.themes[4,1]), 0)
                     ,c(as.integer(Dmax.themes[5,1]), 0))
colnames(maxmin) <- c("Cost & Practicalities", "Teaching Quality"
                      ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
colnames(kmeans_radar)[-1] <- c("Cost & Practicalities", "Teaching Quality"
                                ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
Dradar <- as.data.frame(bind_rows(maxmin, kmeans_radar[,-1]))


M <- cor(kmeans_radar[,-1])
(order.AOE <- corrMatOrder(M, order = "AOE"))
(order.FPC <- corrMatOrder(M, order = "FPC"))
(order.hc <- corrMatOrder(M, order = "hclust"))
(order.hc2 <- corrMatOrder(M, order = "hclust", hclust.method="ward"))
M.AOE <- M[order.AOE,order.AOE ]
M.FPC <- M[order.FPC,order.FPC ]
M.hc  <- M[order.hc, order.hc ]
M.hc2 <- M[order.hc2,order.hc2]

corrplot::corrplot(M, method = "color")
corrplot::corrplot(M.AOE, method = "color")
corrplot::corrplot(M.FPC, method = "color")
corrplot::corrplot(M.hc, method = "color")
corrplot::corrplot(M.hc2, method = "color")


fmsb::radarchart(Dradar[,order.hc2],
           pty = 32,
           axistype = 0,
           pcol = colorpalette.adjust,
           pfcol = colorpalette.adjust,
           plty = 1,
           plwd = 3,
           cglty = 1,
           cglcol = "gray88",
           centerzero = TRUE,
           seg = 5,
           vlcex = 0.75,
           palcex = 0.75)

DScore.rawSum.sil <- tbl_df(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rawSum)))[,])[,c(1,3)]
colnames(DScore.rawSum.sil) <- c('DScore.rawSum.cluster','DScore.rawSum.sil')
bind_cols(DScore.rawSum, DScore.rawSum.sil)

################################################################################
# DScore.rawSum.rowScaled

nb_k <- 4

kmeans.res <- factoextra::eclust(tbl_df(DScore.rawSum.rowScaled) , FUNcluster = "kmeans", k = nb_k, graph = F
                                 ,verbose = T
                                 ,stand = F # apply scale() to DScore
)
fviz_cluster(kmeans.res, geom = "point", frame.type = "convex", show.clust.cent = T,
             title = paste("KMeans Clustering K = ", nb_k, sep = '')
)

colorpalette <- RColorBrewer::brewer.pal(kmeans.res$nbclust, "Set1")
colorpalette.adjust <- as.vector(sapply(colorpalette, function(x) adjustcolor(x,0.5), simplify = T))

fviz_silhouette(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rawSum.rowScaled)))) + scale_color_manual(values = colorpalette)

tbl_df(kmeans.res$centers) %>%
  rownames_to_column( var = "profile" ) -> kmeans_radar


Dmax.themes <- tbl_df(apply(DScore.rawSum.rowScaled, 2,function(x) max(x)))
Dmin.themes <- tbl_df(apply(DScore.rawSum.rowScaled, 2,function(x) min(x)))

maxmin <- data.frame(c(as.numeric(Dmax.themes[1,1]), as.numeric(Dmin.themes[1,1]))
                     ,c(as.numeric(Dmax.themes[2,1]), as.numeric(Dmin.themes[2,1]))
                     ,c(as.numeric(Dmax.themes[3,1]), as.numeric(Dmin.themes[3,1]))
                     ,c(as.numeric(Dmax.themes[4,1]), as.numeric(Dmin.themes[4,1]))
                     ,c(as.numeric(Dmax.themes[5,1]), as.numeric(Dmin.themes[5,1])))
colnames(maxmin) <- c("Cost & Practicalities", "Teaching Quality"
                      ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
colnames(kmeans_radar)[-1] <- c("Cost & Practicalities", "Teaching Quality"
                                ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
Dradar <- as.data.frame(bind_rows(maxmin, kmeans_radar[,-1]))


M <- cor(kmeans_radar[,-1])
(order.AOE <- corrMatOrder(M, order = "AOE"))
(order.FPC <- corrMatOrder(M, order = "FPC"))
(order.hc <- corrMatOrder(M, order = "hclust"))
(order.hc2 <- corrMatOrder(M, order = "hclust", hclust.method="ward"))
M.AOE <- M[order.AOE,order.AOE ]
M.FPC <- M[order.FPC,order.FPC ]
M.hc  <- M[order.hc, order.hc ]
M.hc2 <- M[order.hc2,order.hc2]

corrplot::corrplot(M, method = "color")
corrplot::corrplot(M.AOE, method = "color")
corrplot::corrplot(M.FPC, method = "color")
corrplot::corrplot(M.hc, method = "color")
corrplot::corrplot(M.hc2, method = "color")


fmsb::radarchart(Dradar[,order.FPC],
                 pty = 32,
                 axistype = 0,
                 pcol = colorpalette.adjust,
                 pfcol = colorpalette.adjust,
                 plty = 1,
                 plwd = 3,
                 cglty = 1,
                 cglcol = "gray88",
                 centerzero = TRUE,
                 seg = 5,
                 vlcex = 0.75,
                 palcex = 0.75)

for (i in 1:nrow(kmeans_radar)) {
  d <- bind_rows(maxmin, kmeans_radar[,-1][i,])[,order.FPC]
  fmsb::radarchart(d,
                   pty = 32,
                   axistype = 0,
                   pcol = colorpalette.adjust[i],
                   pfcol = colorpalette.adjust[i],
                   plty = 1,
                   plwd = 3,
                   cglty = 1,
                   cglcol = "gray88",
                   centerzero = TRUE,
                   seg = 5,
                   vlcex = 0.75,
                   palcex = 0.75)
}

DScore.rawSum.rowScaled.sil <- tbl_df(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rawSum.rowScaled)))[,])[,c(1,3)]
colnames(DScore.rawSum.rowScaled.sil) <- c('DScore.rawSum.rowScaled.cluster','DScore.rawSum.rowScaled.sil')
bind_cols(DScore.rawSum, DScore.rawSum.rowScaled.sil)
pp <- bind_cols(DScore.rawSum, DScore.rawSum.sil, DScore.rawSum.rowScaled.sil)

################################################################################
# Inverse rank
library(factoextra)
nb_k <- 4
kmeans.res <- factoextra::eclust(tbl_df(DScore.rank.inverse) , FUNcluster = "kmeans", k = nb_k, graph = F
                                 ,verbose = T
                                 ,stand = F # apply scale() to DScore
)
fviz_cluster(kmeans.res, geom = "point", frame.type = "convex", show.clust.cent = T,
             title = paste("KMeans Clustering K = ", nb_k, sep = '')
)

colorpalette <- RColorBrewer::brewer.pal(kmeans.res$nbclust, "Set1")
colorpalette.adjust <- as.vector(sapply(colorpalette, function(x) adjustcolor(x,0.5), simplify = T))

fviz_silhouette(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rank.inverse)))) + scale_color_manual(values = colorpalette.adjust)

fviz_silhouette(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rawSum)))) + scale_color_manual(values = colorpalette.adjust)

fviz_silhouette(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rank.inverse.high)))) + scale_color_manual(values = colorpalette.adjust)

DScore.rank.inverse.sil <- tbl_df(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rank.inverse)))[,])
colnames(DScore.rank.inverse.sil) <- c('profile','neighbour.profile','quality')

tbl_df(kmeans.res$centers) %>%
  rownames_to_column( var = "profile" ) -> kmeans_radar


(Dmax.themes <- tbl_df(apply(DScore.rank.inverse, 2,function(x) max(x))))
(Dmin.themes <- tbl_df(apply(DScore.rank.inverse, 2,function(x) min(x))-1))

maxmin <- data.frame(c(as.numeric(Dmax.themes[1,1]), as.numeric(Dmin.themes[1,1]))
                     ,c(as.numeric(Dmax.themes[2,1]), as.numeric(Dmin.themes[2,1]))
                     ,c(as.numeric(Dmax.themes[3,1]), as.numeric(Dmin.themes[3,1]))
                     ,c(as.numeric(Dmax.themes[4,1]), as.numeric(Dmin.themes[4,1]))
                     ,c(as.numeric(Dmax.themes[5,1]), as.numeric(Dmin.themes[5,1])))
colnames(maxmin) <- c("Cost & Practicalities", "Teaching Quality"
                      ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
colnames(kmeans_radar)[-1] <- c("Cost & Practicalities", "Teaching Quality"
                                ,"Graduate Outcomes", "Lifestyle & Connections", "Brand & Reputation")
Dradar <- as.data.frame(bind_rows(maxmin, kmeans_radar[,-1]))

library(corrplot)
M <- cor(kmeans_radar[,-1])
(order.AOE <- corrMatOrder(M, order = "AOE"))
(order.FPC <- corrMatOrder(M, order = "FPC"))
(order.hc <- corrMatOrder(M, order = "hclust"))
(order.hc2 <- corrMatOrder(M, order = "hclust", hclust.method="ward"))
M.AOE <- M[order.AOE,order.AOE ]
M.FPC <- M[order.FPC,order.FPC ]
M.hc  <- M[order.hc, order.hc ]
M.hc2 <- M[order.hc2,order.hc2]

corrplot::corrplot(M, method = "color")
corrplot::corrplot(M.AOE, method = "color")
corrplot::corrplot(M.FPC, method = "color")
corrplot::corrplot(M.hc, method = "color")
corrplot::corrplot(M.hc2, method = "color")


fmsb::radarchart(Dradar,
                 pty = 32,
                 axistype = 0,
                 pcol = colorpalette.adjust,
                 pfcol = colorpalette.adjust,
                 plty = 1,
                 plwd = 3,
                 cglty = 1,
                 cglcol = "gray88",
                 centerzero = TRUE,
                 seg = 5,
                 vlcex = 0.75,
                 palcex = 0.75)


order.AOE
order.FPC
order.hc
order.hc2

for (i in 1:nrow(kmeans_radar)) {
  d <- bind_rows(maxmin, kmeans_radar[,-1][i,])
  fmsb::radarchart(d[,order.hc2],
                   pty = 32,
                   axistype = 0,
                   pcol = colorpalette.adjust[i],
                   pfcol = colorpalette.adjust[i],
                   plty = 1,
                   plwd = 3,
                   cglty = 1,
                   cglcol = "gray88",
                   centerzero = TRUE,
                   seg = 5,
                   vlcex = 0.75,
                   palcex = 0.75)
}

pp <- bind_cols(DScore.rawSum, DScore.rank.inverse, DScore.rank.inverse.sil)

################################################################################
library(flexclust)
vol.ch <- DScore.rank.inverse.high.disj
vol.mat <- as.matrix(vol.ch)
fc_cont <- new("flexclustControl")
fc_cont@tolerance <- 0.1
fc_cont@iter.max <- 29
fc_cont@verbose <- 1
fc_family <- "ejaccard"

num_clusters <- 5
vol.cl <- kcca(vol.mat, k = num_clusters, save.data = TRUE,
               control = fc_cont, family = kccaFamily(fc_family))
vol.pca <- prcomp(vol.mat)
plot(vol.cl, data = vol.mat, project = vol.pca, main = 'DScore.rank.inverse.high.disj')
barchart(vol.cl, strip.prefix = "#", shade = TRUE,
         layout = c(vol.cl@k, 1), main = 'DScore.rank.inverse.high.disj')
vol.cl@cluster

colorpalette <- RColorBrewer::brewer.pal(vol.cl@k, "Set1")
colorpalette.adjust <- as.vector(sapply(colorpalette, function(x) adjustcolor(x,0.5), simplify = T))

fviz_silhouette(cluster::silhouette(vol.cl@cluster, dist(as.matrix(DScore.rank.inverse.high.disj)))) + scale_color_manual(values = colorpalette.adjust)

fviz_silhouette(cluster::silhouette(vol.cl@cluster, dist(as.matrix(DScore.rank.inverse)))) + scale_color_manual(values = colorpalette.adjust)
