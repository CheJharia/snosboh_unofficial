library(caret)
library(randomForest)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe.train <- rfe(sctraindata[,-ncol(sctraindata)], sctraindata[,ncol(sctraindata)], sizes=1:12, rfeControl=control)
rfe.train
plot(rfe.train, type=c("g", "o"), cex = 1.0, col = 1:ncol(sctraindata))
