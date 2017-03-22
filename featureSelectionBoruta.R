# https://www.r-bloggers.com/feature-selection-all-relevant-selection-with-the-boruta-package/
# https://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
library(Boruta)
traindata <- dtraining[,-c(1)]
str(traindata)
summary(traindata)


traindata <- traindata[complete.cases(traindata),]
# using normalised data
sctraindata <- cbind(as.data.frame(scale(traindata[, -c(ncol(traindata))])), as.data.frame(traindata[,ncol(traindata)] ))
colnames(sctraindata)[ncol(sctraindata)] <- "StudentStatus"
sctraindata <- as.data.frame(sctraindata)
boruta.train <- Boruta(StudentStatus~., data = sctraindata, doTrace = 2, maxRuns = 200
                       , mcAdj = TRUE )
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n", main = "Feature importance")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
?Boruta



final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)
write.csv(x = boruta.df
          ,file = "final_boruta_stats.csv"
)


plot(final.boruta, xlab = "", xaxt = "n", main = "Feature importance")
lz<-lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=3,labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

