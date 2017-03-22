# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
#install.packages("mice")
library(mice)
list.files()
na_data <- read.csv(file = "NA_data.csv"
                    , header = T
                    ,check.names = F
                    )

# missing value in each dataset
md.pattern(na_data)
dplyr::tbl_df(md.pattern(na_data))
# there are 316 observations with no missing values.
# there are 31 observations with mising values in LE2
# there are 104 observations with mising values in TQ1

#install.packages("VIM")
library(VIM)
mice_plot <- aggr(na_data[,-c(1:3)], col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(na_data[,-c(1:3)]), cex.axis=.9, prop = T,
                    gap=3, ylab=c("Missing data","Pattern"))
?aggr
str(na_data[,-c(1:3)])
thedata<- na_data[,-c(1:3)]
na_data[,c(1:3)]
typeof(thedata)

col_names <- names(thedata)
thedata[,col_names] <- lapply(thedata[,col_names] , factor)

gc()
imputed_Data_polr <- mice(thedata, m=5, maxit = 10, method = 'polr', seed = 500
                     ,printFlag = T, visitSequence = 'monotone')
imputed_Data_rf <- mice(thedata, m=5, maxit = 10, method = 'rf', seed = 500
                          ,printFlag = T, visitSequence = 'monotone')
summary(imputed_Data_polr)
summary(imputed_Data_rf)

#get complete data ( 1st out of 5)
completeData_rf <- complete(imputed_Data_rf,1)
#get complete data ( 1st out of 5)
completeData_polr <- complete(imputed_Data_polr,1)

write.csv(x = cbind(na_data[,c(1:3)], completeData_rf)
          ,file = 'missingValues_randomForest.csv'
          ,row.names = F)
write.csv(x = cbind(na_data[,c(1:3)], completeData_polr)
          ,file = 'missingValues_proportionalOddsModel.csv'
          ,row.names = F)

?mice

