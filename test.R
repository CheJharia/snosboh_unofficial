# Enhanced hierarchical clustering

transformed_cluster_data

res.hc <- eclust(transformed_cluster_data, "hclust",k.max = 5) # compute hclust
fviz_dend(res.hc, rect = TRUE) # dendrogam

res.hc <- eclust(transformed_cluster_data, "hclust",k.max = 6) # compute hclust
fviz_dend(res.hc, rect = TRUE) # dendrogam


# Enhanced hierarchical clustering
res.hc <- 
res.hc <- eclust(cluster_data, "hclust", k = 2)
#+ ggtitle('Global ISS 2016 Cluster K = 1') # compute hclust
res.hc$cluster
res.hct <- eclust(cluster_data, "hclust", k = 5)  # compute hclust

cluster_data_labelled <- cbind(cluster_data, res.hc$cluster)
colnames(cluster_data_labelled)[6] <- 'profile'
colnames(cluster_data_labelled)
cluster_data_labelled <- tbl_df(cluster_data_labelled)
cluster_data_labelled

profiles_character <- cluster_data_labelled %>%
  group_by(profile) %>%
  summarise_each(funs(mean)) 

profiles_character %>%
  mutate(`CL Score` = round(`CL Score`,0),
         `EM Score` = round(`EM Score`,0),
         `LE Score` = round(`LE Score`,0),
         `TQ Score` = round(`TQ Score`,0),
         `UB Score` = round(`UB Score`,0))
## how high is high?
test <- quantile(cluster_data_labelled$`CL Score`)
test[5]
quantile(cluster_data_labelled$`EM Score`)
quantile(cluster_data_labelled$`LE Score`)
quantile(cluster_data_labelled$`TQ Score`)
quantile(cluster_data_labelled$`UB Score`)

# combine with data apac
data.apac_labelled <- cbind(data.apac, res.hc$cluster)
colnames(data.apac_labelled)[9] <- 'profile'
data.apac_labelled <- tbl_df(data.apac_labelled)
data.apac_labelled
fviz_dend(res.hc, rect = TRUE) # dendrogam
fviz_silhouette(res.hc)
clarax <- clara(transformed_cluster_data, 6, samples = 1000)

fviz_cluster(clarax, stand = FALSE, geom = "point",pointsize = 1)
fviz_silhouette(silhouette(clarax))  
?clara
