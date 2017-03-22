library(irlba)
t1 = proc.time()
L  = irlba(as.matrix(completeData), nv=3, tol=1e-5, right_only=TRUE, work=4)
dt = proc.time() - t1

library(threejs)
scatterplot3js(L$v, size=0.5, grid=FALSE)
