
library(mice)

na_data <- training_2
# missing value in each dataset
md.pattern(na_data)
dplyr::tbl_df(md.pattern(na_data))
# there are 316 observations with no missing values.
# there are 31 observations with mising values in LE2
# there are 104 observations with mising values in TQ1

#install.packages("VIM")
library(VIM)

mice_plot <- aggr(na_data[,-c(1)], col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(na_data), cex.axis=.5, prop = T,
                  gap=3, ylab=c("Missing data","Pattern"))
?aggr
str(na_data)
library(magrittr)

thedata<- na_data
typeof(thedata)

col_names <- names(thedata)
# convert all variables to factor type
thedata[,col_names] <- lapply(thedata[,col_names] , factor)
colnames(thedata)
summary(thedata) 
gc()
library(dplyr)

set.seed(1990)
imputed_Data_rf <- mice(thedata, m=1, maxit = 5, method = 'rf', seed = 500
                        ,printFlag = T, visitSequence = 'revmonotone')
summary(imputed_Data_rf)

?complete
#get complete data ( 1nd out of 10)
completeData_rf <- complete(imputed_Data_rf, 1)

write.csv(x = completeData_rf
          ,file = 'missingValues_randomForest.csv'
          ,row.names = F)

summary(completeData_rf)
