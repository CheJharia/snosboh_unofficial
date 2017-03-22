library(MASS)

# p is the number of variables
p<-11

# simulates 1000 rows with
# means=5 and std deviations=1.5
x<-mvrnorm(n=1000,rep(5,p),diag((1.5^2),p))
summary(x)
apply(x, 2, sd)

# calculate correlation matrix
R<-cor(x)
# correlations after columns centered
# i.e., column means now 0's not 5's
x2<-scale(x, scale=FALSE)
summary(x2)
apply(x2, 2, sd)
R2<-cor(x2)
round(R2-R,8) # identical matrices

# checks correlation matrix singularity
solve(R)

# row center the ratings
x_rowcenter<-x-apply(x, 1, mean)
RC<-cor(x_rowcenter)
round(RC-R,8) # uniformly negative

# row-centered correlations singular
solve(RC)

# orginal row means normally distributed
hist(round(apply(x,1,mean),5))

# row-centered row means = 0
table(round(apply(x_rowcenter,1,mean),5))


# mean lower triangular correlation
mean(R[lower.tri(R, diag = FALSE)])
mean(RC[lower.tri(R, diag = FALSE)])

# average correlation = -1/(p-1)
# for independent variables
# where p = # columns
-1/(p-1)
