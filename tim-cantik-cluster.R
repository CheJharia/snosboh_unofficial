maxB <- 120
minB <- 80

DScore_pcaScaledRated_r_q %>% filter(value >= minB & value <= maxB) %>% select(ends_with('Rating')) -> DScore_pcaScaledRated.bis

bind_cols(DtID_complete,DScore_pcaScaledRated_r_q) %>% mutate(g = (value >= minB & value <= maxB)) %>% filter(g == T) -> DScore_pcaScaledRated.bis_ID

nrow(DScore_pcaScaledRated.bis)
nb_k <- 5
kmeans.res <- eclust(tbl_df(DScore_pcaScaledRated.bis) %>% dplyr::select(ends_with('Rating')), FUNcluster = "kmeans", k = nb_k, graph = F
                     ,verbose = T
                     ,stand = F # apply scale() to DScore
)
nrow(kmeans.res$data)

fviz_cluster(kmeans.res, geom = "point", frame.type = "convex", show.clust.cent = T,
             title = paste("KMeans Clustering K = ", nb_k, sep = '')
)
table(kmeans.res$cluster)
kc <- tbl_df(kmeans.res$centers)

kc %>%
  rownames_to_column( var = "profile" ) %>%
  mutate_each(funs(rescale), -profile)  -> kmeans_radar


colnames(kmeans_radar)[-1] <- c("Cost of Living", "Employment"
                                ,"Lifestyle", "Teaching Quality", "University Brand")
colorpalette <- RColorBrewer::brewer.pal(nrow(kmeans_radar), "Set1")
str(kmeans_radar)
ggradar(kmeans_radar, axis.label.size = 2.5) + ggtitle("Radar Chart - Profile comparisons") + theme(text = element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom") + scale_colour_manual(values = colorpalette)

tbl_df(silhouette(kmeans.res$cluster, dist(as.matrix(DScore_pcaScaledRated.bis)))[,])

fviz_silhouette(silhouette(kmeans.res$cluster, dist(as.matrix(DScore_pca.rank)))) + scale_color_manual(values = colorpalette)

for (pr in 1:nrow(kmeans_radar)) {
  paste(pr)
  print(ggradar(kmeans_radar[pr,], axis.label.size = 2.5) + scale_colour_manual(values = colorpalette[pr]) + ggtitle(paste("Radar Chart - Profile ", pr, sep='')) + theme(text =       element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom"))
}

Dprofiled <- bind_cols(DScore_pcaScaledRated.bis, tbl_df(silhouette(kmeans.res$cluster, dist(as.matrix(DScore_pcaScaledRated.bis)))[,]))
Dprofiled %>% select(-neighbor)

resss <- bind_cols(DScore_pcaScaledRated.bis_ID, tbl_df(silhouette(kmeans.res$cluster, dist(as.matrix(DScore_pcaScaledRated.bis)))[,]))
write.csv(x = resss, file = 'res.csv')
