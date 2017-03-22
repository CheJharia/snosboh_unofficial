library(dendextend)
library(colorspace) # get nice colors


species_col <- (colorpalette)[as.numeric(kmeans.res$cluster)]
species_col.quality <- bind_cols(tbl_df(species_col), tbl_df(DScore.rank.inverse.sil$quality))
colnames(species_col.quality) <- c('color','quality')
species_col.quality$quality.rescale <- scales::rescale(species_col.quality$quality,c(0,1))

summary(species_col.quality)


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
species_labels <- as.factor(kmeans.res$cluster)
species_col.quality$profile <- species_labels
pp <- bind_cols(DScore.rawSum, DScore.rank.inverse, DScore.rank.inverse.sil,species_col.quality)
write.csv(pp, 'fsf.csv')
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = .8)
MASS::parcoord(DScore.rawSum, col = species_col.quality$colormod, var.label = TRUE, lwd = 2)
MASS::parcoord(DScore.rawSum, col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)
MASS::parcoord(apply(DScore.rank.inverse, 2, function(x) jitter(x)), col = species_col.quality$colormod, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore.rank.inverse, 2, function(x) jitter(x, factor = .5, amount = .1)), col = species_col.quality$colormod, var.label = TRUE, lwd = 2)

tmp <- species_col.quality
prof <- 3
tmp[tmp$profile != prof,]$colormod.rescale <- adjustcolor('#d3d3d3', alpha.f = 0.4)
tmp[tmp$profile == prof,]$colormod <- tmp[tmp$profile == 1,]$color

quality.threshold <- as.numeric(quantile(species_col.quality[species_col.quality$profile == prof,]$quality,0.95))

species_col.quality$quality > quality.threshold

MASS::parcoord(apply(DScore.rank.inverse[,c(2,5,3,1,4)][species_col.quality$quality > quality.threshold,], 2, function(x) jitter(x, amount = 0.1)), col = tmp[species_col.quality$quality > quality.threshold,]$colormod.rescale, var.label = TRUE, lwd = 2)


MASS::parcoord(apply(DScore.rank.inverse[,c(1,3,5,4,2)], 2, function(x) jitter(x, amount = 0.1)), col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)


MASS::parcoord(DScore.rank.inverse[,c(1,3,5,4,2)], col = tmp$colormod.rescale, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore.rank.inverse[,order.AOE], 2, function(x) jitter(x)), col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore.rank.inverse[,order.FPC], 2, function(x) jitter(x)), col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore.rank.inverse[,order.hc ], 2, function(x) jitter(x)), col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore.rank.inverse[,order.hc2], 2, function(x) jitter(x)), col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore.rank.inverse, 2, function(x) jitter(x)), col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)


MASS::parcoord(apply(DScore.rank.inverse, 2, function(x) jitter(x, amount = 0)), col = species_col.quality$colormod, var.label = TRUE, lwd = 2)

MASS::parcoord(apply(DScore.rank.inverse, 2, function(x) jitter(x, amount = 0)), col = species_col.quality$colormod.rescale, var.label = TRUE, lwd = 2)


################################################################################

