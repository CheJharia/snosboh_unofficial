library(ClustOfVar)


#quantitative variables
data(decathlon)
Dworcester.num <- Dworcester[-c(1:3, ncol(Dworcester))]
as.matrix(Dworcester.num)
tree <- hclustvar(X.quanti = as.matrix(Dworcester.num))
plot(tree)

tree <- kmeansvar(X.quanti = as.matrix(Dworcester.num), init = 5)
summary(tree)
