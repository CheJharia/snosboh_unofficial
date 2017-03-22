install.packages("FactoMineR")
library("FactoMineR")
library("devtools")
install_github("kassambara/factoextra")
library("factoextra")

install.packages("corrplot")
library("corrplot")
cor.mat <- round(cor(filteredFeatures),2)
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(training[,-c(1)], histogram=TRUE)

library("FactoMineR")
training <- training[complete.cases(training),]
res.pca <- PCA(training[,-c(1,length(training))], graph = FALSE)
print(res.pca)


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
fviz_screeplot(res.pca, ncp=10)
# Change color and theme
fviz_pca_var(res.pca, col.var="steelblue")+
  theme_minimal()

# Change the gradient color
fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=55)+theme_bw()

fviz_pca_var(res.pca, alpha.var="contrib")+
  theme_minimal()


fviz_pca_ind(res.pca, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)


fviz_pca_ind(res.pca, label="none", habillage=training$StudentStatus)
