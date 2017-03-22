library(kcirt)


DD <- NULL
DD <- dd

for(i in 1:1000){
  constructMap.ls <- list(
    c(seq(1:10)),
    c(seq(1:10)),
    c(seq(1:10)),
    c(seq(1:10))
  )
  qTypes <- rep("R", length(constructMap.ls))
  dt <- sample(c(1,2),1)
  mod <- kcirt.model(constructMap.ls=constructMap.ls, qTypes=qTypes, mu = runif(10), mxLambda=matrix(rnorm( sum(mod$ns)^2, 0, 0.3 ), sum(mod$ns), sum(mod$ns)), deltaType = dt)
  N <- 10
  mod <- kcirt.sim(model=mod, N=N, type = 'Eta')
  (dd <- t(ikcirt.rndData1(N=N, qTypes=qTypes, mxDelta=mod$mxDelta, ns=mod$ns)))
  dd[dd > 5] <- 0
  dd <- tbl_df(dd)
  DD <- bind_rows(DD,dd)
}
summary(DD)

matrix(rnorm( sum(mod$ns)^2, 0, 0.3 ), sum(mod$ns), sum(mod$ns))
