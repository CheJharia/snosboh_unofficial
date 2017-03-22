library(pmr)

#-------------------------------------------------------------------------------
# ahp analytic hierarchy process
#-------------------------------------------------------------------------------
## create an artificial A matrix abc, example taken from Koczkodaj et al. (1997)
abc <- matrix(data = 1:16, nrow = 4, ncol = 4, byrow = TRUE)
abc[1,1] <- 1
abc[1,2] <- 2
abc[1,3] <- 5
abc[1,4] <- 4
abc[2,1] <- 1/2
abc[2,2] <- 1
abc[2,3] <- 3
abc[2,4] <- 1.9
abc[3,1] <- 1/5
abc[3,2] <- 1/3
abc[3,3] <- 1
abc[3,4] <- 0.7
abc[4,1] <- 1/4
abc[4,2] <- 1/1.9
abc[4,3] <- 1/0.7
abc[4,4] <- 1
## compute the weights, Saaty's and Koczkodaj's inconsistencies
ahp(abc)


data(big4)
str(big4) ; plot(big4)
data(breasttissue)


## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3,n)
## fit the distance-based model with Spearman's rho distance
dbm(dset = breasttissue, dtype = "rho")
destat(as.data.frame(dplyr::count(breasttissue, adi, car, con, new)))

## create an artificial dataset
X1 <- c(1,1,2,2,3,3)
X2 <- c(2,3,1,3,1,2)
X3 <- c(3,2,3,1,2,1)
n <- c(6,5,4,3,2,1)
test <- data.frame(X1,X2,X3,n)
## compute the descriptive statistics of the artificial dataset
destat(test)
bb[sample(x = 1:10, size = 5, replace = F)] <- unique(sample(vc, size = 5, replace = F))
tmp1 <- clusterGeneration::genRandomClust(numClust = 5, sepVal = 0.7182818, numNoisy = 0, numOutlier = 0, numReplicate = 200, clustszind = 2, clustSizeEq = 400, outputDatFlag = F, outputEmpirical = F, outputInfo = F, outputLogFlag = F)

vc <- tmp1$memList[[200]]
table(vc)
for (i in 2:200) {
  tmp <- tmp1$memList[[i]]
  vc <- c(vc, tmp)
}
length(vc)
table(vc)
subset(c(1,2,3,4,5))
setdiff(c(1,2,3,4,5), c(1))
setdiff(c(1),c(1,2,3,4,5))



Dfake <- dplyr::tbl_df(matrix( data = vc[1:round(length(vc) / 40) * 40], ncol = 40, nrow = round(length(vc) / 40), byrow = T))
write.csv(x = Dfake
          ,file = 'genRandomClust.csv'
          ,row.names = F)
sample(1:10, 5, replace = F)

for (i in 1:10) {
  for (j in 1:4) {
      sample(x = 1:10, size = 5, replace = F)
      sample(x = 11:20, size = 5, replace = F)
      sample(x = 21:30, size = 5, replace = F)
      sample(x = 31:40, size = 5, replace = F)
  }
}
c <- 0
bb <- vector(length = 400000)
for (p in 1:40000){
  min <- 1 + c
  max <- 10 + c
  sample(x = min:max, size = 5, replace = F)
  while(TRUE){
    nv <- unique(sample(vc, size = 5, replace = F))
    if(length(nv) == 5) break
  }
  bb[sample(x = min:max, size = 5, replace = F)] <- nv
  c <- c + 10
}
bb[is.na(bb)] <- 0
length(bb)
bb <- dplyr::tbl_df(matrix(bb, ncol = 40, byrow = T))
table(bb$V1)
table(bb$V2)
table(bb$V11)
table(bb$V12)

bb[sample(x = 1:10, size = 5, replace = F)] <- unique(sample(vc, size = 5, replace = F))
table(vc)
sample(c(1:10), replace = F)
vc <- sample(c(1:10), replace = F)
for (i in 2:4e+05) {
  vc <- c(vc, sample(c(1:10), replace = F))
}
unique(sample(vc, size = 5, replace = F))
Dfake <- dplyr::tbl_df(matrix( data = vc[1:round(length(vc) / 40) * 40], ncol = 40, nrow = round(length(vc) / 40), byrow = T))
Dfake <- dplyr::tbl_df(matrix(vc, ncol = 40, byrow = T))
Dfake %>%
Dfake %>% mutate(x = replace(x, x>5, 0))

Dfake %>% mutate_each(funs(replace(., . > 5, 0))) -> Dfaken
table(Dfaken$V40)
