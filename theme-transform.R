# score data which also include blank cells
fn <- "2 output ProfileData-2016-03-17 15_36_30-all-scored-total-scores.csv"
# score data with only complete cases i.e no blank cells
# fn <- "2 output ProfileData-2016-03-17 15_36_30-complete-scored-total-scores.csv"

library(dplyr)


scores <- tbl_df(read.csv(file = fn, header = T, check.names = F))
str(scores)
summary(scores)

# create another column 'division'
scores %<>%
  mutate(division = as.factor(substr(scores$`Global Response ID`, start = 1, stop = 4 ))) %>%
  select(everything())
scores

library(ggplot2)
data <- scores
library(e1071)

################################################################################
# Cost of Living score transform
# The original scores are negatively skewed
################################################################################
skewness(sqrt(21.2-scores$`CL Score`))
ggplot(data , aes(x = sqrt(21.2-scores$`CL Score`))) +
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

################################################################################
# Employment score transform
# The original scores are negatively skewed
################################################################################

skewness(scores$`EM Score`)
skewness(sqrt(30-scores$`EM Score`))
ggplot(data , aes(x = sqrt(30-scores$`EM Score`))) +
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

################################################################################
# Lifestyle score transform
# The original scores are negatively skewed
################################################################################
skewness(scores$`LE Score`)
skewness(log10(scores$`LE Score`+4.5))
ggplot(data , aes(x = log10(scores$`LE Score`+4.5))) +
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


################################################################################
# Teaching Quality score transform
# The original scores are negatively skewed
################################################################################
skewness(scores$`TQ Score`)
skewness(log10(36-scores$`TQ Score`))
ggplot(data , aes(x = log10(36-scores$`TQ Score`))) +
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


################################################################################
# University Brand score transform
# The original scores are negatively skewed
################################################################################
skewness(scores$`UB Score`)
skewness(sqrt(23-scores$`UB Score`))
ggplot(data , aes(x = sqrt(23-`UB Score`))) +
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


transformed_cluster_data <- tbl_df(as.data.frame(cbind(sqrt(21.2-scores$`CL Score`),
                                  sqrt(30-scores$`EM Score`),
                                  log10(scores$`LE Score`+4.5),
                                  log10(36-scores$`TQ Score`),
                                  sqrt(23-scores$`UB Score`))))
transformed_cluster_data
colnames(transformed_cluster_data) <- c('CL Score','EM Score','LE Score','TQ Score','UB Score')








################################################################################
# K-Means on transformed data
# check whether clustering gets better
################################################################################



(km.res <- kmeans(transformed_cluster_data, centers = 6, nstart = 25, iter.max = 5))
km.res$betweenss
km.res$tot.withinss
km.res

# km.res <- pam(scale(df), 4)
#km.res
profiles_means <- aggregate(transformed_cluster_data, by=list(cluster=km.res$cluster), mean)
profiles_means %<>%
  mutate(CL = round(`CL Score`),
         EM = round(`EM Score`),
         LE = round(`LE Score`),
         TQ = round(`TQ Score`),
         UB = round(`UB Score`)) %>%
  mutate(profile = paste(CL,
                         EM,
                         LE,
                         TQ,
                         UB, sep = '-')) %>%
  select(cluster, profile, CL, EM, LE, TQ, UB)
profiles_means
#fviz_cluster(km.res, data = transformed_cluster_data, geom = c(""), frame.type = "convex", frame.level = 0.95) + theme_classic() + scale_fill_discrete(name="Profile (CL-EM-LE-TQ-UB)", labels= profiles_6means$profile) +guides(shape=guide_legend(title=NULL))+ ggtitle("Cluster K-Means Method: 2 clusters")

fviz_cluster(km.res, data = transformed_cluster_data, stand = T,geom = c("point"), frame.type = "convex", frame.level = 0.95) + theme_classic() + scale_fill_discrete(name="Profile (CL-EM-LE-TQ-UB)", labels= profiles_means$profile) + guides(shape=guide_legend(title=NULL)) + ggtitle("Cluster K-Means Method: 4 clusters")

fviz_cluster(km.res, data = transformed_cluster_data, geom = c("point"), frame = F) + theme_classic() + ggtitle("Cluster K-Means Method: 4 clusters")
