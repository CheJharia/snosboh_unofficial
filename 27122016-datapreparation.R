library(tidyverse)
# read raw data
Draw <- readr::read_csv(file = '20161228040745-SurveyExport.csv'
                        ,progress = T)
# filter only prospective international students with complete answers
Draw.intprospect <- Draw %>% dplyr::filter(`Are you studying overseas or planning to study overseas?` == "Yes - I am planning to study overseas" & Status == 'Complete' )


# get a quality summary for each variable
dplyr::tbl_df(funModeling::df_status(data = Draw.intprospect ,print_results = F)) %>%
  mutate(var_original_pos = row_number()) %>%
  arrange(desc(p_na)) -> Dvar.status

# ID variables: for unicity, and relevant questions for segmentation
Dvar.status %>% filter(variable %in% c('Response ID', 'Longitude','Latitude','Country','City','Link Name'
                                       ,'What nationality are you?'
                                       ,'What gender are you?'
                                       ,'Field:What are you planning to study?'
                                       ,'Subject:What are you planning to study?'
                                       ,'At what level are you currently planning to study?'))
# Extract profiling variables
Draw.intprospect %>% dplyr::select(dplyr::contains("What five things are most important")) %>%
  dplyr::select(-dplyr::contains("recommended")) -> Draw.profiling
# rename columns
c(paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(1,2),5), sep = ''),
  paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(3,4),5), sep = ''),
  paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(5,6),5), sep = ''),
  paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(7,8),5), sep = '')) -> colnames(Draw.profiling)

# convert rank into simple scores by inversely mapping each rank
# note that, NA rank is defined as 0 score
tbl_df(sapply(Draw.profiling, function(x) plyr::mapvalues(x, c(NA,seq(1:5)), c(0,seq(from = 5, to = 1, by = -1))))) -> Draw.profiling.score

# create another copy of dataframe that has each variable as factor variable
Draw.profiling.score %>%
  mutate_each_(funs(factor(.)),colnames(Draw.profiling.score)) -> Draw.profiling.score.factor
summary(Draw.profiling.score.factor)
summary(Draw.profiling.score)

# Burt table
tbl_df(amap::burt(Draw.profiling.score.factor)) -> Draw.profiling.score.factor.burt

# Disjonctif table
ade4::acm.disjonctif(as.matrix(Draw.profiling.score.factor)) -> Draw.profiling.score.factor.disj
# rename columns
colnames(Draw.profiling.score.factor.disj) <- colnames(Draw.profiling.score.factor.burt)

# Basic Sum Score scheme
bind_cols(tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("CP")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("TQ")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("GO")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("LE")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("BR")) %>% rowSums(.))) -> DScore.rawSum
colnames(DScore.rawSum) <- c('CP.Score.rawSum','TQ.Score.rawSum','GO.Score.rawSum','LE.Score.rawSum','BR.Score.rawSum')

# rescale sums score to 0:36
DScore.Sum.rescaled <- tbl_df(data.frame(matrix(unlist(lapply(DScore.rawSum, function(x) plotrix::rescale(x,c(0,36)))), nrow=nrow(DScore.rawSum), byrow = F)))
colnames(DScore.Sum.rescaled) <- c('CP.Score.rawSum.rescaled','TQ.Score.rawSum.rescaled','GO.Score.rawSum.rescaled','LE.Score.rawSum.rescaled','BR.Score.rawSum.rescaled')

# normalize score by column
DScore.Sum.rescaled.cnorm <- tbl_df(scale(DScore.Sum.rescaled)[,])

# normalize score by row
DScore.Sum.rescaled.crnorm <- tbl_df(scrime::rowScales(DScore.Sum.rescaled.cnorm))


# Inverse Rank scheme
(DScore.rank.inverse <- floor(tbl_df(t(apply(DScore.Sum.rescaled.crnorm, MARGIN = 1, FUN = function(x) rank(x))))))
colnames(DScore.rank.inverse) <- c('CP.rank.inverse', 'TQ.rank.inverse', 'GO.rank.inverse', 'LE.rank.inverse', 'BR.rank.inverse')

# Preference separation quality measure
Dpref.sep.quality <- c()
for (i in 1:nrow(DScore.Sum.rescaled)){
  Dpref.sep.quality <- c(Dpref.sep.quality, sum(dist(t(DScore.Sum.rescaled[i,]))))
}
Dpref.sep.quality <- tbl_df(Dpref.sep.quality)
colnames(Dpref.sep.quality) <- 'Pref.Separation.Quality'


Dcluster <- DScore.rank.inverse
#################################################################################
# Clustering
library(factoextra)
nb_k <- 5 # heuristic
kmeans.res <- factoextra::eclust(tbl_df(DScore.rank.inverse) , FUNcluster = "kmeans", k = nb_k, graph = F
                                 ,verbose = T
                                 ,stand = F # apply scale() to DScore
)
fviz_cluster(kmeans.res, geom = "point", frame.type = "convex", show.clust.cent = T,
             title = paste("KMeans Clustering K = ", nb_k, sep = '')
)

# get as many color as cluster from Set1
colorpalette <- RColorBrewer::brewer.pal(kmeans.res$nbclust, "Set1")
colorpalette.adjust <- as.vector(sapply(colorpalette, function(x) adjustcolor(x,0.1), simplify = T))

fviz_silhouette(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rank.inverse)))) + scale_color_manual(values = colorpalette.adjust)

DScore.rank.inverse.sil <- tbl_df(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore.rank.inverse)))[,])
colnames(DScore.rank.inverse.sil) <- c('profile','neighbour.profile','quality')

library(colorspace) # get nice colors

# define colors for each respondent
#colorpalette.custom <- c('#393b70','#637939','#8c6d31','#843c39','#7b4173')
profile_col <- (colorpalette)[as.numeric(kmeans.res$cluster)]
#profile_col <- (colorpalette.adjust)[as.numeric(kmeans.res$cluster)]
#profile_col <- (colorpalette.custom)[as.numeric(kmeans.res$cluster)]
# create a table with sil_width as quality
profile_col.quality <- bind_cols(tbl_df(profile_col), tbl_df(DScore.rank.inverse.sil$quality))
colnames(profile_col.quality) <- c('color','quality')
# rescale quality to 0 1, for adjustcolor()
profile_col.quality$quality.rescale <- scales::rescale(profile_col.quality$quality,c(0,1))

# create subtle colors by matching color and quality using adjustcolor func
v <- NULL
for (i in 1:nrow(profile_col.quality)){
  v <- c(v, adjustcolor(as.character(profile_col.quality[i,1]),as.numeric(profile_col.quality[i,2])))
}
# create subtle colors by matching color and quality.rescale using adjustcolor func
w <- NULL
for (i in 1:nrow(profile_col.quality)){
  w <- c(w, adjustcolor(as.character(profile_col.quality[i,1]),as.numeric(profile_col.quality[i,3])))
}
profile_col.quality$colormod <- v
profile_col.quality$colormod.rescale <- w
profile_labels <- plyr::mapvalues(as.factor(kmeans.res$cluster), 1:kmeans.res$nbclust,paste(rep('profile',kmeans.res$nbclust), 1:kmeans.res$nbclust, sep = '_'))
profile_col.quality$profile <- profile_labels

################################################################################
# Parallel Coordinates

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# plot highlights for a given profile
profile_col.quality.bis <- profile_col.quality
# define which profile number
prof.nb <- 1
# order y axis in increasing order of cluster centers
parcoord.y.order <- order(rank(kmeans.res$centers[prof.nb,], ties.method = 'last'))
# create profile label
prof.lb <- paste('profile',prof.nb, sep = '_')
# greyed out other profiles
profile_col.quality.bis[profile_col.quality.bis$profile != prof.lb,]$colormod <- adjustcolor('#e4e4e4', alpha.f = 0.4)
# define color quality threshold
quality.threshold <- as.numeric(quantile(profile_col.quality[profile_col.quality$profile == prof.lb,]$quality,0.9))

# Create median table for cleaner parallel coord plot
bind_cols(DScore.rank.inverse, profile_col.quality) %>% dplyr::select(dplyr::contains('inverse'),profile) %>%
  group_by(profile) %>% summarise_each(funs(median)) -> DScore.rank.inverse.profiles.median

DScore.rank.inverse.profiles.median[,-1] <- ceiling(DScore.rank.inverse.profiles.median[,-1])

bind_cols(DScore.rank.inverse, profile_col.quality) %>% dplyr::select(dplyr::contains('inverse'),profile) -> DScore.rank.inverse.median
# create a table with repeated median scores
for (i in 1:kmeans.res$nbclust) {
  DScore.rank.inverse.median[DScore.rank.inverse.median$profile == levels(profile_labels)[i],][,1:5] <- DScore.rank.inverse.profiles.median[i,2:6]
}
colnames(DScore.rank.inverse.median)[1:5] <- c('Cost Practicalities','Teaching Quality','Graduate Outcomes', 'Lifestyle','Branding Reputation')
# define median order
median.order <- order(rank(as.matrix(DScore.rank.inverse.profiles.median[prof.nb,][,2:6]), ties.method = 'last'))


# Create mode table for cleaner parallel coord plot
bind_cols(DScore.rank.inverse, profile_col.quality) %>% dplyr::select(dplyr::contains('inverse'),profile) %>%
  group_by(profile) %>% summarise_each(funs(Mode)) -> DScore.rank.inverse.profiles.mode


bind_cols(DScore.rank.inverse, profile_col.quality) %>% dplyr::select(dplyr::contains('inverse'),profile) -> DScore.rank.inverse.mode
# create a table with repeated mode scores
for (i in 1:kmeans.res$nbclust) {
  DScore.rank.inverse.mode[DScore.rank.inverse.mode$profile == levels(profile_labels)[i],][,1:5] <- DScore.rank.inverse.profiles.mode[i,2:6]
}
colnames(DScore.rank.inverse.mode)[1:5] <- c('Cost Practicalities','Teaching Quality','Graduate Outcomes', 'Lifestyle','Branding Reputation')
# define median order
mode.order <- order(rank(as.matrix(DScore.rank.inverse.profiles.mode[prof.nb,][,2:6]), ties.method = 'last'))



# plot raw scores
MASS::parcoord(DScore.rawSum[,parcoord.y.order], col = profile_col.quality$colormod, lwd = 2, main = 'Parallel coordinate plot - Profiles 5 Scores')
MASS::parcoord(DScore.rawSum[,parcoord.y.order], col = profile_col.quality$colormod.rescale, lwd = 2, main = 'Parallel coordinate plot - Profiles 5 Scores')

# plot rank inverse
MASS::parcoord(DScore.rank.inverse[,parcoord.y.order], col = profile_col.quality$colormod, lwd = 7, main = 'Parallel coordinate plot - Profiles preference rank')

# plot rank inverse with jitter

MASS::parcoord(apply(DScore.rank.inverse[,parcoord.y.order], 2, function(x) jitter(x, amount = 0.1)), col = profile_col.quality$colormod.rescale, lwd = exp(1), main = 'Parallel coordinate plot - Profiles preference rank with jitter')

MASS::parcoord(apply(DScore.rank.inverse[,parcoord.y.order], 2, function(x) jitter(x)), col = profile_col.quality$colormod,  lwd = exp(1), main = 'Parallel coordinate plot - Profiles preference rank with jitter and quality-adjusted color')
MASS::parcoord(apply(DScore.rank.inverse[,parcoord.y.order], 2, function(x) jitter(x, factor = .5, amount = .1)), col = profile_col.quality$colormod, var.label = TRUE, lwd = 2)

# plot chosen profile with other profiles greyed out
MASS::parcoord(apply(DScore.rank.inverse[,parcoord.y.order][profile_col.quality$quality > quality.threshold,], 2, function(x) jitter(x, amount = 0.1)), col = profile_col.quality.bis[profile_col.quality$quality > quality.threshold,]$colormod,  lwd = 8)

# plot chosen profile with others as well
MASS::parcoord(apply(DScore.rank.inverse[,parcoord.y.order][profile_col.quality$quality > quality.threshold,], 2, function(x) jitter(x, amount = 0.1)), col = profile_col.quality.bis[profile_col.quality$quality > quality.threshold,]$colormod.rescale, lwd = 8)


MASS::parcoord(DScore.rank.inverse.median[,median.order][profile_col.quality$quality > quality.threshold,], col = profile_col.quality.bis[profile_col.quality$quality > quality.threshold,]$colormod.rescale, lwd = 5, main = 'Parallel coordinate plot - Profiles preference median rank')

MASS::parcoord(DScore.rank.inverse.median[,median.order][profile_col.quality$quality > quality.threshold,], col = profile_col.quality.bis[profile_col.quality$quality > quality.threshold,]$colormod, lwd = 5, main = 'Parallel coordinate plot - Profiles preference median rank : Profile 1')


MASS::parcoord(DScore.rank.inverse.mode[,mode.order][profile_col.quality$quality > quality.threshold,], col = profile_col.quality.bis[profile_col.quality$quality > quality.threshold,]$colormod.rescale, lwd = 5, main = 'Parallel coordinate plot - Profiles preference mode rank')

MASS::parcoord(DScore.rank.inverse.mode[,mode.order][profile_col.quality$quality > quality.threshold,], col = profile_col.quality.bis[profile_col.quality$quality > quality.threshold,]$colormod, lwd = 5, main = 'Parallel coordinate plot - Profiles preference mode rank : Profile 1')

# need to have axis on the same scale

###############################################################################

# build contingency table using along clusters
Dcluster.contingency <- bind_cols(tbl_df(kmeans.res$cluster), Draw.profiling.score)
Draw.profiling.score %>% summarise_each(funs(sum)) %>% t() -> Dvariables.totSum
colnames(Dvariables.totSum) <- 'tot.Sum'
order(-Dvariables.totSum) -> Ovariable.order
Dcluster.contingency %>% group_by(value) %>% summarise_each(funs(sum)) %>% t() -> Dcluster.contingency
Dcluster.contingency
# remove first row
Dcluster.contingency[-1,] -> Dcluster.contingency
# add column names
colnames(Dcluster.contingency) <- paste(rep('profile',kmeans.res$nbclust), 1:kmeans.res$nbclust, sep = '_')

library("gplots")
# 1. convert the data as a table
Dcluster.contingency.rowScale <- scrime::rowScales(Dcluster.contingency)


for (i in 1:nrow(Dcluster.contingency.rowScale)) {
  print(Dcluster.contingency.rowScale[i,] <- plotrix::rescale(Dcluster.contingency.rowScale[i,], c(0,1)))
}
# 2. Graph
# plot balloonplot and sort rowname alphabetically
balloonplot(t(as.table(Dcluster.contingency.rowScale[order(row.names(Dcluster.contingency.rowScale)),])), main = "Profiles and Profiling Questions", xlab = "", ylab= "",
            label = F, show.margins = F)


library("graphics")

mosaicplot(as.table(as.matrix(Dcluster.contingency[order(row.names(Dcluster.contingency.rowScale)),])), shade = TRUE, las = 2,
           main = "Segmentation: Mosaic Plot - Order alphabetically")

mosaicplot(as.table(as.matrix(Dcluster.contingency[Ovariable.order,])), shade = TRUE, las = 2,
           main = "Segmentation: Mosaic Plot - Order by Variable Frequency")

mosaicplot(as.table(as.matrix(Dcluster.contingency[Ovariable.order,])), shade = F, las = 2,
           main = "Segmentation: Mosaic Plot - Colored by Profiles", color = colorpalette)

###############################################################################
# Correspondence Analysis
library(FactoMineR)
res.ca <- CA(Dcluster.contingency, graph = T)
print(res.ca)
# summary of the first 2 dimensions
summary(res.ca, nb.dec = 2, ncp = 2)
summary.CA(res.ca)
eig <- get_eigenvalue(res.ca)
trace <- sum(eig$eigenvalue)
cor.coef <- sqrt(trace)
cor.coef

eigenvalues <- get_eigenvalue(res.ca)
head(round(eigenvalues, 2))

fviz_screeplot(res.ca)

fviz_screeplot(res.ca) +
  geom_hline(yintercept=25, linetype=2, color="red")

fviz_ca_biplot(res.ca)

# Change the theme
fviz_ca_biplot(res.ca) +
  theme_minimal()


# Default plot
fviz_ca_row(res.ca)

library("corrplot")
row <- get_ca_row(res.ca)
row
corrplot(row$contrib, is.corr=FALSE)

# Contributions of rows on Dim.1
fviz_contrib(res.ca, choice = "row", axes = 1)
# Contributions of rows on Dim.2
fviz_contrib(res.ca, choice = "row", axes = 2)
# Total contribution on Dim.1 and Dim.2
fviz_contrib(res.ca, choice = "row", axes = 1:2)
fviz_contrib(res.ca, choice = "row", axes = 1, top = 5)

# Control row point colors using their contribution
# Possible values for the argument col.row are :
# "cos2", "contrib", "coord", "x", "y"
fviz_ca_row(res.ca, col.row = "contrib")

# Change the gradient color
fviz_ca_row(res.ca, col.row="contrib")+
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.5)+theme_minimal()

# Control the transparency of rows using their contribution
# Possible values for the argument alpha.var are :
# "cos2", "contrib", "coord", "x", "y"
fviz_ca_row(res.ca, alpha.row="contrib")+
  theme_minimal()

# Select the top 5 contributing rows
fviz_ca_row(res.ca, alpha.row="contrib", select.row=list(contrib=20))

library("corrplot")
corrplot(row$cos2, is.corr=FALSE)

fviz_ca_col(res.ca)

# Control column point colors using their contribution
# Possible values for the argument col.col are :
# "cos2", "contrib", "coord", "x", "y"
fviz_ca_col(res.ca, col.col="contrib")+
  scale_color_gradient2(low="white", mid = "blue",
                        high="red", midpoint = 1)+theme_minimal()

fviz_ca_biplot(res.ca)+
  theme_minimal()

fviz_ca_biplot(res.ca, map ="rowprincipal", arrow = c(TRUE, TRUE))

fviz_ca_biplot(res.ca, map ="colgreen",
               arrow = c(TRUE, FALSE)) + theme_minimal()

plot(res.ca)

fviz_ca_biplot(res.ca) +
  theme_minimal()

fviz_ca_biplot(res.ca, invisible = c("row.sup", "col.sup") ) +
  theme_minimal()


