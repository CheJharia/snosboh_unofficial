library(Boruta)
library(dplyr)
library(magrittr)
library(pca3d)
traindata <- read.csv("UTAS-raw-scores-for-boruta-package.csv", header = T, stringsAsFactors = F)
traindata %<>% filter(traindata$Profileable == 1)
str(traindata)
nrow(traindata)
names(traindata) <- gsub("\\.", " ", names(traindata))
summary(traindata)

#traindata[traindata == ""] <- NA

#traindata <- traindata[complete.cases(traindata),]

names(traindata)[1:4]
convert <- c(1:4)
traindata[,convert] <- data.frame(apply(traindata[convert], 2, as.factor))



################################################################################
# install.packages("FactoMineR")
# install.packages("devtools")
# devtools::install_github("kassambara/factoextra")
library(dplyr)
############################################################ Cost of living PCA
names(traindata)[5:11]
CL <- c(5:11)
CL_pca_data <- tbl_df(traindata[,c(1,CL)])
library("FactoMineR")
CL_res.pca <- PCA(CL_pca_data[,2:8], graph = TRUE)
print(CL_res.pca)

eigenvalues <- CL_res.pca$eig
head(eigenvalues[, 1:2])

library("factoextra")
fviz_screeplot(CL_res.pca, ncp=10, addlabels = T) + theme_minimal() + labs(title = "Principal Component Analysis - Cost of Living  ",
                                                                         x = "Principal Components", y = "% of variances")
?fviz_screeplot
# Coordinates of variables
head(CL_res.pca$var$coord)
fviz_pca_var(CL_res.pca)


head(CL_res.pca$var$cos2)

fviz_pca_var(CL_res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.80) + theme_minimal()


# Contributions of variables on PC1
fviz_contrib(CL_res.pca, choice = "var", axes = 1) +  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Cost of Living Variables Contributions to the 1st Dimension ",
                                                   x = "Variables", y = "Contributions (%)")
?fviz_contrib
# Contributions of variables on PC2
fviz_contrib(CL_res.pca, choice = "var", axes = 2)+  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Cost of Living Variables Contributions to the 2nd Dimension ",
                                                    x = "Variables", y = "Contributions (%)")

# Total contribution on PC1 and PC2
fviz_contrib(CL_res.pca, choice = "var", axes = 1:2)

head(CL_res.pca$var$contrib)
cont_table <- CL_res.pca$var$contrib
cont_table[,2] + cont_table[,1]
fviz_contrib(CL_res.pca, choice = "var", axes = 1, top = 3)

# Control variable colors using their contributions
fviz_pca_var(CL_res.pca, col.var="contrib")


# Change the gradient color
fviz_pca_var(CL_res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=90) + theme_minimal()

fviz_pca_ind(CL_res.pca)

fviz_pca_ind(CL_res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

fviz_pca_ind(CL_res.pca, col.ind="cos2", geom = "point") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)+ theme_minimal()

# Color by the contributions
fviz_pca_ind(CL_res.pca, col.ind="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=4)

# Control automatically the color of individuals using the cos2
#################################################################################
#################################################################################
#################################################################################
fviz_pca_biplot(CL_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5) +
  theme_minimal()  + labs(title = "Cost of Living - Principal Component Analysis")

fviz_pca_biplot(CL_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=10, height = 15), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$`profile new` ) +
  theme_minimal() 
library(pca3d)
colnames(CL_pca_data)
pca <- prcomp( CL_pca_data[,2:8], scale.= TRUE )

pca3d(pca, group=traindata$`profile new`
      ,col = traindata$warna)
makeMoviePCA()
#################################################################################
#################################################################################
#################################################################################

############################################################ Employment PCA

names(traindata)[14:19]
EM <- c(14:19)
EM_pca_data <- tbl_df(traindata[,c(1,EM)])
library("FactoMineR")
EM_res.pca <- PCA(EM_pca_data[,2:7], graph = TRUE)
print(EM_res.pca)

eigenvalues <- EM_res.pca$eig
head(eigenvalues[, 1:2])

library("factoextra")
fviz_screeplot(EM_res.pca, ncp=10, addlabels = T) + theme_minimal() + labs(title = "Principal Component Analysis - Employment",
                                                                           x = "Principal Components", y = "% of variances")
?fviz_screeplot
# Coordinates of variables
head(EM_res.pca$var$coord)
fviz_pca_var(EM_res.pca)


head(EM_res.pca$var$cos2)

fviz_pca_var(EM_res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.80) + theme_minimal()


# Contributions of variables on PC1
fviz_contrib(EM_res.pca, choice = "var", axes = 1) +  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 1st Dimension ",
                                                    x = "Variables", y = "Contributions (%)")
?fviz_contrib
# Contributions of variables on PC2
fviz_contrib(EM_res.pca, choice = "var", axes = 2)+  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 2nd Dimension ",
                                                    x = "Variables", y = "Contributions (%)")

# Total contribution on PC1 and PC2
fviz_contrib(EM_res.pca, choice = "var", axes = 1:2)

head(EM_res.pca$var$contrib)
cont_table <- EM_res.pca$var$contrib
cont_table[,2] + cont_table[,1]
fviz_contrib(EM_res.pca, choice = "var", axes = 1, top = 3)

# Control variable colors using their contributions
fviz_pca_var(EM_res.pca, col.var="contrib")


# Change the gradient color
fviz_pca_var(EM_res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=90) + theme_minimal()

fviz_pca_ind(EM_res.pca)

fviz_pca_ind(EM_res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

fviz_pca_ind(EM_res.pca, col.ind="cos2", geom = "point") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)+ theme_minimal()

# Color by the contributions
fviz_pca_ind(EM_res.pca, col.ind="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=4)

# Control automatically the color of individuals using the cos2
fviz_pca_biplot(EM_res.pca, label ="var", col.ind="cos2") +
  theme_minimal()

fviz_pca_biplot(EM_res.pca, label ="var", col.ind="contrib") +
  theme_minimal()

fviz_pca_biplot(EM_res.pca, label ="var", col.ind="coord") +
  theme_minimal()

# Change the color by groups, add ellipses
fviz_pca_biplot(EM_res.pca, label="var", habillage=traindata$Status,
                addEllipses=TRUE, ellipse.level=0.95)


fviz_pca_biplot(EM_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5) +
  theme_minimal() 

fviz_pca_biplot(EM_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$profile ) +
  theme_minimal() 

fviz_pca_biplot(EM_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$`profile new`, ellipse.level = 0.9 ) +
  theme_minimal() 


names(traindata)[14:19]
EM <- c(14:19)
EM_pca_data <- tbl_df(traindata[,c(1,EM)])
library("FactoMineR")
EM_res.pca <- PCA(EM_pca_data[,2:7], graph = TRUE)
print(EM_res.pca)

eigenvalues <- EM_res.pca$eig
head(eigenvalues[, 1:2])

#################################################################################
#################################################################################
#################################################################################
fviz_pca_biplot(EM_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5) +
  theme_minimal()   + labs(title = "Employment - Principal Component Analysis")

fviz_pca_biplot(EM_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=10, height = 15), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$`profile new` ) +
  theme_minimal() 

pca <- prcomp( EM_pca_data[,2:7], scale.= TRUE )
pca3d(pca, group=traindata$`profile new`,col = traindata$warna)
makeMoviePCA()
#################################################################################
#################################################################################
#################################################################################

############################################################ Lifestyle PCA
names(traindata)[22:26]
LE <- c(22:26)
LE_pca_data <- tbl_df(traindata[,c(1,LE)])
library("FactoMineR")
LE_res.pca <- PCA(LE_pca_data[,2:6], graph = TRUE)
print(LE_res.pca)

eigenvalues <- LE_res.pca$eig
head(eigenvalues[, 1:2])

library("factoextra")
fviz_screeplot(LE_res.pca, ncp=10, addlabels = T) + theme_minimal() + labs(title = "Principal Component Analysis - Lifestyle",
                                                                           x = "Principal Components", y = "% of variances")
?fviz_screeplot
# Coordinates of variables
head(LE_res.pca$var$coord)
fviz_pca_var(LE_res.pca)


head(EM_res.pca$var$cos2)

fviz_pca_var(LE_res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.80) + theme_minimal()


# Contributions of variables on PC1
fviz_contrib(LE_res.pca, choice = "var", axes = 1) +  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 1st Dimension ",
                                                    x = "Variables", y = "Contributions (%)")
?fviz_contrib
# Contributions of variables on PC2
fviz_contrib(LE_res.pca, choice = "var", axes = 2)+  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 2nd Dimension ",
                                                    x = "Variables", y = "Contributions (%)")

# Total contribution on PC1 and PC2
fviz_contrib(LE_res.pca, choice = "var", axes = 1:2)

head(LE_res.pca$var$contrib)
cont_table <- LE_res.pca$var$contrib
cont_table[,2] + cont_table[,1]
fviz_contrib(LE_res.pca, choice = "var", axes = 1, top = 3)

# Control variable colors using their contributions
fviz_pca_var(LE_res.pca, col.var="contrib")


# Change the gradient color
fviz_pca_var(LE_res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=90) + theme_minimal()

fviz_pca_ind(LE_res.pca)

fviz_pca_ind(LE_res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

fviz_pca_ind(LE_res.pca, col.ind="cos2", geom = "point") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)+ theme_minimal()

# Color by the contributions
fviz_pca_ind(LE_res.pca, col.ind="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=4)

# Control automatically the color of individuals using the cos2
fviz_pca_biplot(LE_res.pca, label ="var", col.ind="cos2") +
  theme_minimal()


pca <- prcomp( LE_pca_data, scale.= TRUE )
pca3d(pca, group=traindata$`profile new`)
makeMoviePCA()



profile_habillage <- read.csv(file = "profile-habillage.csv", header = T, stringsAsFactors = F)

#################################################################################
#################################################################################
#################################################################################
fviz_pca_biplot(LE_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5) +
  theme_minimal()   + labs(title = "Lifestyle - Principal Component Analysis")

fviz_pca_biplot(LE_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=10, height = 15), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$`profile new` ) +
  theme_minimal() 

pca <- prcomp( LE_pca_data[,2:6], scale.= TRUE )
pca3d(pca, group=traindata$`profile new`,col = traindata$warna)
makeMoviePCA()
#################################################################################
#################################################################################
#################################################################################
############################################################ Teaching Quality PCA
names(traindata)[29:32]
TQ <- c(29:32)
TQ_pca_data <- tbl_df(traindata[,c(1,TQ)])
library("FactoMineR")
TQ_res.pca  <- PCA(TQ_pca_data[,2:5], graph = TRUE)
print(TQ_res.pca)

eigenvalues <- TQ_res.pca$eig
head(eigenvalues[, 1:2])

library("factoextra")
fviz_screeplot(TQ_res.pca, ncp=10, addlabels = T) + theme_minimal() + labs(title = "Principal Component Analysis - Teaching Quality",
                                                                           x = "Principal Components", y = "% of variances")
?fviz_screeplot
# Coordinates of variables
head(TQ_res.pca$var$coord)
fviz_pca_var(TQ_res.pca)


head(TQ_res.pca$var$cos2)

fviz_pca_var(TQ_res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.80) + theme_minimal()


# Contributions of variables on PC1
fviz_contrib(TQ_res.pca, choice = "var", axes = 1) +  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 1st Dimension ",
                                                    x = "Variables", y = "Contributions (%)")
?fviz_contrib
# Contributions of variables on PC2
fviz_contrib(TQ_res.pca, choice = "var", axes = 2)+  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 2nd Dimension ",
                                                    x = "Variables", y = "Contributions (%)")

# Total contribution on PC1 and PC2
fviz_contrib(TQ_res.pca, choice = "var", axes = 1:2)

head(TQ_res.pca$var$contrib)
cont_table <- TQ_res.pca$var$contrib
cont_table[,2] + cont_table[,1]
fviz_contrib(TQ_res.pca, choice = "var", axes = 1, top = 3)

# Control variable colors using their contributions
fviz_pca_var(TQ_res.pca, col.var="contrib")


# Change the gradient color
fviz_pca_var(TQ_res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=90) + theme_minimal()

fviz_pca_ind(TQ_res.pca)

fviz_pca_ind(TQ_res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

fviz_pca_ind(TQ_res.pca, col.ind="cos2", geom = "point") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)+ theme_minimal()

# Color by the contributions
fviz_pca_ind(TQ_res.pca, col.ind="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=4)


pca <- prcomp( TQ_pca_data, scale.= TRUE )
pca3d(pca, group=traindata$`profile new`)
makeMoviePCA()


# Control automatically the color of individuals using the cos2
fviz_pca_biplot(TQ_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=4, height = 10), repel = T, col.var = 'black', habillage = traindata$profile) +
  theme_minimal() 
+ scale_color_gradient2(low="white", mid="blue",
                                          high="red", midpoint=0.6)


# Change the color by groups, add ellipses
fviz_pca_biplot(LE_res.pca, label="var", habillage=traindata$profile,
                addEllipses=TRUE, ellipse.level=0.95)

# Change the color by groups, add ellipses
fviz_pca_biplot(res.pca, label="var", habillage=iris$Species,
                addEllipses=TRUE, ellipse.level=0.95) 

# Change the color by groups, add ellipses
fviz_pca_biplot(TQ_res.pca, label="var", habillage=traindata$Status,
                addEllipses=TRUE, ellipse.level=0.95)

#################################################################################
#################################################################################
#################################################################################
fviz_pca_biplot(TQ_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5) +
  theme_minimal()   + labs(title = "Teaching Quality - Principal Component Analysis")

fviz_pca_biplot(TQ_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=10, height = 15), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$`profile new` ) +
  theme_minimal() 

pca <- prcomp( TQ_pca_data[,2:5], scale.= TRUE )
pca3d(pca, group=traindata$`profile new`,col = traindata$warna)
makeMoviePCA()
#################################################################################
#################################################################################
#################################################################################

############################################################ University Brand PCA
names(traindata)[35:38]
UB <- c(35:38)
UB_pca_data <- tbl_df(traindata[,c(1,UB)])
library("FactoMineR")
UB_res.pca <- PCA(UB_pca_data[,2:5], graph = TRUE)
print(UB_res.pca)

eigenvalues <- UB_res.pca$eig
head(eigenvalues[, 1:2])

library("factoextra")
fviz_screeplot(UB_res.pca, ncp=10, addlabels = T) + theme_minimal() + labs(title = "Principal Component Analysis - Teaching Quality",
                                                                            x = "Principal Components", y = "% of variances")
?fviz_screeplot
# Coordinates of variables
head(UB_res.pca$var$coord)
fviz_pca_var(UB_res.pca)


head(UB_res.pca$var$cos2)

fviz_pca_var(UB_res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.80) + theme_minimal()


# Contributions of variables on PC1
fviz_contrib(UB_res.pca, choice = "var", axes = 1) +  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 1st Dimension ",
                                                    x = "Variables", y = "Contributions (%)")
?fviz_contrib
# Contributions of variables on PC2
fviz_contrib(UB_res.pca, choice = "var", axes = 2)+  theme_minimal() +
  theme(axis.text.x = element_text(angle=90))+ labs(title = "Employment Variables Contributions to the 2nd Dimension ",
                                                    x = "Variables", y = "Contributions (%)")

# Total contribution on PC1 and PC2
fviz_contrib(UB_res.pca, choice = "var", axes = 1:2)

head(UB_res.pca$var$contrib)
cont_table <- UB_res.pca$var$contrib
cont_table[,2] + cont_table[,1]
fviz_contrib(UB_res.pca, choice = "var", axes = 1, top = 3)

# Control variable colors using their contributions
fviz_pca_var(UB_res.pca, col.var="contrib")


# Change the gradient color
fviz_pca_var(UB_res.pca, col.var="contrib") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=90) + theme_minimal()

fviz_pca_ind(UB_res.pca)

fviz_pca_ind(UB_res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

fviz_pca_ind(UB_res.pca, col.ind="cos2", geom = "point") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)+ theme_minimal()

# Color by the contributions
fviz_pca_ind(UB_res.pca, col.ind="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=4)

# Control automatically the color of individuals using the cos2
fviz_pca_biplot(UB_res.pca, label ="var", col.ind="cos2") +
  theme_minimal()

# Change the color by groups, add ellipses
fviz_pca_biplot(UB_res.pca, label="var", habillage=traindata$Status,
                addEllipses=TRUE, ellipse.level=0.95)


#################################################################################
#################################################################################
#################################################################################
fviz_pca_biplot(UB_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5) +
  theme_minimal()   + labs(title = "University Brand - Principal Component Analysis")

fviz_pca_biplot(UB_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=10, height = 15), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$`profile new` ) +
  theme_minimal() 

pca <- prcomp( UB_pca_data[,2:5], scale.= TRUE )
pca3d(pca, group=traindata$`profile new`,col = traindata$warna)
makeMoviePCA()
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

## display a qualitative palette
RColorBrewer::display.brewer.pal(7,"profile-habillage.csv")

defaultPalettePCA3D(n = 3)

pca_fun_only <- tbl_df(read.csv(file = 'utas-pca-3d-fun.csv',header = T, check.names = F))
pca <- prcomp( pca_fun_only[1:5], scale.= TRUE )
pca3d(pca, group=pca_fun_only$profile,col = traindata$warna)
makeMoviePCA()
snapshotPCA3d(file = "test.png")


library("FactoMineR")
utas_res.pca <- PCA(pca_fun_only[1:5], graph = TRUE)

fviz_pca_biplot(utas_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=1, height = 10), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5) +
  theme_minimal()   + labs(title = "University Brand - Principal Component Analysis")

fviz_pca_biplot(utas_res.pca, label ="var", col.ind="cos2", jitter = list(what = c("b"), width=10, height = 15), repel = T, col.var = 'black',addEllipses = TRUE, alpha.ind = 0.5, labelsize = 4.5,habillage = traindata$`profile new` ) +
  theme_minimal() 
