library(caret)


nzv <- nearZeroVar(features[,-c(1,2)])

filteredFeatures <- features[,-c(1,2)][, -nzv]
dim(filteredFeatures)
str(filteredFeatures)

descrCor <-  cor(filteredFeatures)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
descrCor <- cor(filteredFeatures)
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredFeatures <- filteredFeatures[,-highlyCorDescr]
descrCor2 <- cor(filteredFeatures)
summary(descrCor2[upper.tri(descrCor2)])

install.packages('cluster')
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
plotSubset <- cbind(features[,c(2)],data.frame(scale(filteredFeatures)))
str(plotSubset)
xyplot(Enrolled ~ CommFreqDay,
       data = plotSubset,
       groups = Enrolled, 
       auto.key = list(columns = 2))  
