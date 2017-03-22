library(dplyr)
library(magrittr)
library(factoextra)
################################################################################
# Read input data
# - filter to APAC data
################################################################################

# score data which also include blank cells
fn <- "2 output ProfileData-2016-03-17 15_36_30-all-scored-total-scores.csv"
# score data with only complete cases i.e no blank cells
# fn <- "2 output ProfileData-2016-03-17 15_36_30-complete-scored-total-scores.csv"
# latest data
#fn <- "2 output ProfileData-2016-03-31 22_19_21-all-scored-total-scores.csv"
scores <- tbl_df(read.csv(file = fn, header = T, check.names = F))
str(scores)
summary(scores)

# create another column 'division'
scores %<>%
  mutate(division = as.factor(substr(scores$`Global Response ID`, start = 1, stop = 4 ))) %>%
  select(everything())
scores

library(ggplot2)

data.apac <- droplevels(filter(scores, division == 'APAC'))
str(data.apac)

data.emea <- droplevels(filter(scores, division == 'EMEA'))
str(data.apac)

# combine apac emea
data <- rbind(data.apac[1:1000,], data.emea)

data <- scores

data <- select(scores, c(`CL Score`, `EM Score`, `LE Score`, `TQ Score`, `UB Score`))
## Finding the optimal number of clusters
gc()
opt_clust_kmeans_wss <- fviz_nbclust(data, kmeans, method = "wss", k.max = 7) # kmeans clustering
gc()
opt_clust_pam_wss <- fviz_nbclust(data, pam, method = "wss") # pam clustering
gc()
opt_clust_hclust_wss <- fviz_nbclust(data, hcut, method = "wss") # hierarchical clustering
gc()
opt_clust_kmeans_silh <- fviz_nbclust(data, kmeans, method = "silhouette") 
gc()
opt_clust_pam_silh <- fviz_nbclust(data, pam, method = "silhouette", k.max = 7) 
gc()
opt_clust_hclust_silh <- fviz_nbclust(data, hcut, method = "silhouette", hc_method = "complete") 
gc()
opt_clust_kmeans_wss + geom_vline(xintercept = 5, linetype = 2) + geom_vline(xintercept = 6, linetype = 2) + theme_classic() + ggtitle("Optimal Nb of Clusters APAC : K-Means method")
opt_clust_pam_wss + geom_vline(xintercept = 6, linetype = 2) + geom_vline(xintercept = 7, linetype = 2)+ theme_classic() + ggtitle("Optimal Nb of Clusters APAC : PAM (Partition Around Mediods) method")
opt_clust_hclust_wss + geom_vline(xintercept = 5, linetype = 2) + theme_classic() + ggtitle("Optimal Nb of Clusters APAC: Hierarchical clustering method")

#opt_clust_kmeans_silh + theme_classic() + ggtitle("Optimal Nb of Clusters APAC : K-Means ")
opt_clust_pam_silh + theme_classic() + ggtitle("Optimal Nb of Clusters APAC: PAM (Partition Around Mediods) method")
opt_clust_hclust_silh + theme_classic() + ggtitle("Optimal Nb of Clusters APAC: Hierarchical clustering method")




# Compute the gap statistic
gap_stat_kmeans <- clusGap(data, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 500) 
# Plot the result
fviz_gap_stat(gap_stat_kmeans) + theme_classic() + ggtitle('Optimal Nb of Clusters APAC: GAP statistics method') + geom_vline(xintercept = 5, linetype = 2, colour='blue' )


gc()
# Enhanced hierarchical clustering

cluster_data <- data
remove(data)
res.hc1 <- eclust(cluster_data, "hclust", k = 1,
                  method = "complete", graph = FALSE) 
gc()
res.hc2 <- eclust(cluster_data, "hclust", k = 2,
                  method = "complete", graph = FALSE) 
gc()
res.hc3 <- eclust(cluster_data, "hclust", k = 3,
                  method = "complete", graph = FALSE) 
gc()
res.hc4 <- eclust(cluster_data, "hclust", k = 4,
                  method = "complete", graph = FALSE) 
gc()
res.hc5 <- eclust(cluster_data, "hclust", k = 5,
                 method = "complete", graph = FALSE) 

head(res.hc5$cluster, 15)
?eclust
# Dendrogram
fviz_dend(res.hc1, rect = TRUE, show_labels = FALSE, k_colors = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"), main = "Hierarchical Clustering: 1 Profile") 
fviz_dend(res.hc2, rect = TRUE, show_labels = FALSE, k_colors = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"), main = "Hierarchical Clustering: 2 Profiles") 
fviz_dend(res.hc3, rect = TRUE, show_labels = FALSE, k_colors = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"), main = "Hierarchical Clustering: 3 Profiles") 
fviz_dend(res.hc4, rect = TRUE, show_labels = FALSE, k_colors = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"), main = "Hierarchical Clustering: 4 Profiles") 
fviz_dend(res.hc5, rect = TRUE, show_labels = FALSE, k_colors = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"), main = "Hierarchical Clustering: 5 Profiles") 
global-hclust-5-profiles-nice-color
write.csv(cbind(scores, res.hc5$cluster), file = 'global_data_5_profiles.csv', row.names = F)
write.csv(cbind(data.emea, res.hc4$cluster), file = 'emea_data_4_profiles.csv', row.names = F)
################################################################################
# Check scores distribution
# 
################################################################################


ggplot(data , aes(x = `CL Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon", position = "identity", fill = "#8DD3C7") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Cost of Living Score distribution")

ggplot(data , aes(x = `EM Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon", position = "identity", fill = "#8DD3C7") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Employment Score distribution")

ggplot(data , aes(x = `LE Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon", position = "identity", fill = "#8DD3C7") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Lifestyle Score distribution")

ggplot(data , aes(x = `TQ Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon", position = "identity", fill = "#8DD3C7") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Teaching Quality Score distribution")

ggplot(data , aes(x = `UB Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon", position = "identity", fill = "#8DD3C7") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("University Brand Score distribution")

