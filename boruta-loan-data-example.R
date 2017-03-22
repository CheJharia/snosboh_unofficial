

# http://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii
# http://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/
install.packages("Boruta")
library(Boruta)

traindata <- read.csv("train_load_data.csv", header = T, stringsAsFactors = F)
str(traindata)
names(traindata) <- gsub("_", "", names(traindata))
summary(traindata)
