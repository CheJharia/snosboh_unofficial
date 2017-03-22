
species_col <- rev(colorpalette)[as.numeric(kmeans.res$cluster)]
DScore_pca.rank.sil <- tbl_df(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore_pca.rank)))[,])
species_col.quality <- bind_cols(tbl_df(species_col), tbl_df(DScore_pca.rank.sil$sil_width))
colnames(species_col.quality) <- c('color','quality')

v <- NULL
for(i in 1:nrow(species_col.quality)){
  v <- c(v, adjustcolor(as.character(species_col.quality[i,1]),as.numeric(species_col.quality[i,2])))
}
species_col.quality$colormod <- v

bind_cols(tbl_df(DtCL_complete %>% rowSums()),
tbl_df(DtEM_complete %>% rowSums()),
tbl_df(DtLE_complete %>% rowSums()),
tbl_df(DtTQ_complete %>% rowSums()),
tbl_df(DtUB_complete %>% rowSums()))

MASS::parcoord(bind_cols(tbl_df(scale(tbl_df(DtCL_complete %>% rowSums()))[,]),
                         tbl_df(scale(tbl_df(DtEM_complete %>% rowSums()))[,]),
                         tbl_df(scale(tbl_df(DtLE_complete %>% rowSums()))[,]),
                         tbl_df(scale(tbl_df(DtTQ_complete %>% rowSums()))[,]),
                         tbl_df(scale(tbl_df(DtUB_complete %>% rowSums()))[,])), col = species_col.quality$colormod, var.label =TRUE, lwd = 2)

species_col <- (colorpalette)[as.numeric(kmeans.res$cluster)]
species_col.quality <- bind_cols(tbl_df(species_col), tbl_df(DScore_pca.rank.sil$sil_width))
colnames(species_col.quality) <- c('color','quality')
species_labels <- as.factor(kmeans.res$cluster)
species_col.quality$profile <- species_labels
species_col.quality$quality.rescale <- scales::rescale(species_col.quality$quality,c(0,1))

v <- NULL
for(i in 1:nrow(species_col.quality)){
  v <- c(v, adjustcolor(as.character(species_col.quality[i,1]),as.numeric(species_col.quality[i,2])))
}

w <- NULL
for(i in 1:nrow(species_col.quality)){
  w <- c(w, adjustcolor(as.character(species_col.quality[i,1]),as.numeric(species_col.quality[i,3])))
}

species_col.quality$colormod <- v
species_col.quality$colormod.rescale <- w

quality.threshold <- 0.3

tmp <- species_col.quality
tmp[tmp$profile != 1,]$colormod.rescale <- adjustcolor('#d3d3d3', alpha.f = 0.4)
tmp[tmp$profile == 1,]$colormod <- tmp[tmp$profile == 1,]$color


MASS::parcoord(DScore_old.rank, col = species_col.quality$colormod, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore_old.rank, 2, function(x) jitter(x)), col = species_col.quality$colormod, var.label = TRUE, lwd = 2)

colnames(DScore_old.rank) <-c("Cost of Living", "Employment"
                              ,"Lifestyle", "Teaching Quality", "University Brand")
MASS::parcoord(apply(DScore_old.rank[,c(2,5,3,4,1)][species_col.quality$quality > quality.threshold,], 2, function(x) jitter(x, amount = 0.1)), col = tmp[species_col.quality$quality > quality.threshold,]$colormod.rescale, var.label = TRUE, lwd = 2)
