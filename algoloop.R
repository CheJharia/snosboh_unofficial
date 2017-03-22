library(clValid)
clmethods <- c("hierarchical")
gc()
clValidations <- c("internal","stability")
ihierarchical <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                  clMethods = clmethods, validation = clValidations)
?clValid
# Summary
summary(ihierarchical)
save.image()

# 2

clmethods <- c("kmeans")
gc()
clValidations <- c("internal","stability")
ikmeans <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(ikmeans)

# 3 - too long: abandoned

clmethods <- c("diana")
gc()
clValidations <- c("internal","stability")
idiana <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(idiana)

# 4 : membership are all very close to 1/k
clmethods <- c("fanny")
gc()
clValidations <- c("internal","stability")
ifanny <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(ifanny)
save.image()
# 5
clmethods <- c("som")
gc()
clValidations <- c("internal","stability")
isom <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(isom)

# 6
clmethods <- c("model")
gc()
clValidations <- c("internal","stability")
imodel <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(imodel)

# 7
clmethods <- c("sota")
gc()
clValidations <- c("internal","stability")
isota <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(isota)

# 8

clmethods <- c("pam")
gc()
clValidations <- c("internal","stability")
ipam <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(ipam)

# 9
clmethods <- c("clara")
gc()
clValidations <- c("internal","stability")
iclara <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(iclara)
save.image()
# 10
clmethods <- c("agnes")
gc()
clValidations <- c("internal","stability")
iagnes <- clValid(as.data.frame(clusterDataScaled), nClust = 5, maxitems = 5, verbose = TRUE,
                        clMethods = clmethods, validation = clValidations)

# Summary
summary(iagnes)
indicates the degree of connectedness of the clusters, as determined by the k-nearest neighbors. The connectivity has a value between 0 and infinity and should be minimized. 
