

# The variable Species (index = 5) is removed before PCA
library("FactoMineR")
library(dplyr)
library(factoextra)

fname <- 'ProfileData (UTAS) 2016-08-30-complete LE.csv'
pca_data <- dplyr::tbl_df(read.csv(file = fname
                                   ,header = T
                                   ,check.names = F))
colnames(pca_data)
pca_data_scaled <- scale(pca_data[, -(1:4)])
#write.csv(x = cbind(pca_data[,1],pca_data_scaled)
#          ,file = 'ProfileData (APAC) 2016-04-22-complete UB scaled.csv'
#          ,row.names = F)
# the variable Metadata_Global Response ID is removed before PCA
res.pca <- PCA(pca_data_scaled, graph = T)

# Extract eigenvalues/variances
eig_table <- as.data.frame(get_eig(res.pca))

# Visualize eigenvalues/variances
fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3)+
  theme_minimal()

# Extract the results for variables
var <- get_pca_var(res.pca)
var

# Coordinates of variables
var$coord

# Contribution of variables
(var_contrib <- as.data.frame(var$contrib))

# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "steelblue")

# Control variable colors using their contributions
# Use gradient color
fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint = 96) +
  theme_minimal()


# Variable contributions on axis 1
fviz_contrib(res.pca, choice="var", axes = 1 )+
  labs(title = "Contributions to Dim 1")
# Variable contributions on axes 1 + 2
fviz_contrib(res.pca, choice="var", axes = 1:2)+
  labs(title = "Contributions to Dim 1+2")

# Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind


# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
#fviz_pca_ind(res.pca, repel = TRUE, col.ind = "cos2")+
#  scale_color_gradient2(low="blue", mid="white",
#                        high="red", midpoint=0.6)+
#  theme_minimal()


# 1. Loading and preparing data

df <- scale(pca_data[, -1])
# 2. Compute k-means
set.seed(123)
gc()
km.res <- kmeans(pca_data[, -1], 5, nstart = 25)
# 3. Visualize
gc()
library("factoextra")
five_colors <- c("#e41a1c",
                 "#377eb8",
                 "#4daf4a",
                 "#984ea3",
                 "#ff7f00")
fviz_cluster(km.res, data = df)+theme_minimal()+
  scale_color_manual(values = five_colors)+
  scale_fill_manual(values = five_colors) +
  labs(title= "Partitioning Clustering Plot")


library("factoextra")
# Compute hierarchical clustering and cut into 5 clusters
res <- hcut(df, k = 5, stand = TRUE)
# Visualize
fviz_dend(res, rect = TRUE, show_labels = FALSE, cex = 0.5,
          k_colors = five_colors)
