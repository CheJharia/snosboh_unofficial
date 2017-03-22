data(iris)
# this is a little tweak so that things line up nicely later on
iris$Species <- factor(iris$Species,
                       levels = c("versicolor","virginica","setosa"))
head(iris)

round(cor(iris[,1:4]), 2)

pc <- princomp(iris[,1:4], cor=TRUE, scores=TRUE)

plot(pc,type="lines")

biplot(pc)


library(rgl)
plot3d(pc$scores[,1:3], col = rainbow(3))

text3d(pc$scores[,1:3],texts=rownames(iris))
text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)


set.seed(42)
cl <- kmeans(iris[,1:4],3)
iris$cluster <- as.factor(cl$cluster)


plot3d(pc$scores[,1:3], col=iris$cluster, main="k-means clusters")
plot3d(pc$scores[,1:3], col=iris$Species, main="actual species")
