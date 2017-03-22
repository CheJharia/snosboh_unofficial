library(clValid)

# Compute clValid
gc()
clmethods <- c("hierarchical","kmeans","pam","sota","agnes","model","clara","som","fanny")
intern <- clValid(as.data.frame(clusterDataScaled), nClust = 2:10,
                  clMethods = clmethods, validation = "internal")
summary(intern)
