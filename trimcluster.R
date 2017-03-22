install.packages('trimcluster')
library(trimcluster)
set.seed(10001)
n1 <-6000
n2 <-6000
n3 <-7000
n0 <-1000
nn <- n1+n2+n3+n0
pp <- 2
X <- matrix(rep(0,nn*pp),nrow=nn)
ii <-0
for (i in 1:n1){
  ii <-ii+1
  X[ii,] <- c(5,-5)+rnorm(2)
}
for (i in 1:n2){
  ii <- ii+1
  X[ii,] <- c(5,5)+rnorm(2)*0.75
}
for (i in 1:n3){
  ii <- ii+1
  X[ii,] <- c(-5,-5)+rnorm(2)*0.75
}
for (i in 1:n0){
  ii <- ii+1
  X[ii,] <- rnorm(2)*8
}
tkm1 <- trimkmeans(X,k=5,trim=0.1,runs=3)
# runs=3 is used to save computing time.
print(tkm1)
plot(tkm1,X)
###############################################################################

trimkmeans_murdoch_5 <-trimkmeans(DScore_pca, k=5,trim=0.1,runs=19, scaling = T, printcrit = T
           , maxit = 2*nrow(DScore_pca))
print(trimkmeans_murdoch_5)
plot(trimkmeans_murdoch_5,as.matrix(DScore_pca))
