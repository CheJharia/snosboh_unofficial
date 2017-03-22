
# ref: https://www.r-bloggers.com/predictability-in-network-models/
# read data from the net
data <- read.csv('http://psychosystems.org/wp-content/uploads/2014/10/Wenchuan.csv')
data <- na.omit(data)
p <- ncol(data)
dim(data)

## Estimate Network Model

library(mgm)
fit_obj <- mgmfit(data = data, 
                  type = rep('g', p),
                  lev = rep(1, p),
                  rule.reg = 'OR'
                  ,ret.warn = F)

## Compute Predictability of Nodes
pred_obj <- predict(fit_obj, data, 
                    error.continuous = 'VarExpl')
pred_obj$error

## Visualize Network & Predictability

library(qgraph)

qgraph(fit_obj$wadj, # weighted adjacency matrix as input
       layout = 'spring', 
       pie = pred_obj$error$Error, # provide errors as input
       pieColor = rep('#377EB8',p),
       node.color = fit_obj$edgecolor,
       labels = colnames(data))


