library("corrplot")
cor.mat <- round(cor(completeData),2)
head(cor.mat[, 1:6])
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

library("FactoMineR")
res.pca <- PCA(completeData, graph = FALSE)
eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

library("factoextra")
fviz_pca_var(res.pca, col.var="contrib")
fviz_pca_var(res.pca, alpha.var="contrib")+
  theme_minimal()
head(res.pca$var$contrib)
