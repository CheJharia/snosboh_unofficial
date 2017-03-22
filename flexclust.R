
#http://files.meetup.com/3182622/Customer%20Preference%20Segments%20with%20flexclust%20-%20volunteers%202015061.html
library(flexclust)
# rename colnames for simplicity
colnames(completeData) <- iss_2016_good_vars[-c(1:5,41,42,69),]$new_var_id

vol_ch <- scale(completeData
                ,center = T
                ,scale = T)
vol.mat <- as.matrix(vol_ch)

fc_cont <- new("flexclustControl") ## holds "hyperparameters"
fc_cont@tolerance <- 1
fc_cont@iter.max <- 100
fc_cont@verbose <- 1 ## verbose > 0 will show iterations
fc_family <- "angle"

fc_seed <- 1990 
num_clusters <- 3 ## Simple example – only three clusters
set.seed(fc_seed)
vol.cl <- kcca(vol.mat, k = num_clusters, save.data = TRUE,
               control = fc_cont, family = kccaFamily(fc_family))
summary(vol.cl)

vol.pca <- prcomp(vol.mat) ## plot on first two principal components
plot(fc_reorder(x = vol.cl,
                orderby = "decending size"), data = vol.mat, project = vol.pca, main = "ISS 2016 reclustering")


barchart(vol.cl, strip.prefix = "#", shade = TRUE,
         layout = c(vol.cl@k, 1), main = "ISS 2016 reclustering")
library(dplyr)
for (i in 2:10) {
  fc_rclust(x = vol.mat
            ,k = i
            ,fc_cont = fc_cont
            ,nrep = 100
            ,plotme = T
            ,fc_family = "angle"
            ,verbose = T)
}
############################################################################

num_clusters <- 3 ## Stable
set.seed(fc_seed)
vol.cl <- kcca(vol.mat, k = num_clusters, save.data = TRUE,
               control = fc_cont, family = kccaFamily(fc_family))
summary(vol.cl)

vol.pca <- prcomp(vol.mat) ## plot on first two principal components
plot(fc_reorder(x = vol.cl,
                orderby = "decending size"), data = vol.mat, project = vol.pca, main = "Volunteers Stated Preferences Survey - Segment Seperation Plot")


barchart(fc_reorder(x = vol.cl,
                    orderby = "decending size"), strip.prefix = "#", shade = TRUE,
         layout = c(vol.cl@k, 1), main = "Volunteers Stated Preferences Survey - Segment Profile Plot")

