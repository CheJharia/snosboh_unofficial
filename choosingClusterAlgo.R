library(clValid)

# Compute clValid
gc()
clmethods <- c("hierarchical","kmeans","pam","sota","agnes","model","clara","som","fanny")
intern <- clValid(clusterDataScaled, nClust = 4:6,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)
?clValid


