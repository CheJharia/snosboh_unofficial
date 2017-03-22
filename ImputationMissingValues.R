# https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
#install.packages("mice")
library(mice)
#list.files()
na_data <- read.csv(file = "UTAS-2-missingValues.csv"
                    , header = T
                    ,check.names = F
)
summary(na_data)
# missing value in each dataset
md.pattern(na_data)
dplyr::tbl_df(md.pattern(na_data))

#install.packages("VIM")
library(VIM)
mice_plot <- aggr(na_data[,-c(1:5)], col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(na_data[,-c(1:5)]), cex.axis=.9, prop = T,
                  gap=3, ylab=c("Missing data","Pattern"))
?aggr
str(na_data[,-c(1:5)])
thedata<- na_data[,-c(1:5)]
na_data[,c(1:5)]
typeof(thedata)

col_names <- names(thedata)
thedata[,col_names] <- lapply(thedata[,col_names] , factor)

gc()
# Classification and regression trees (cart) method
imputed_Data_cart <- mice(thedata, m=1, maxit = 1, method = 'cart', seed = 1990
                          ,printFlag = T, visitSequence = 'monotone')

summary(imputed_Data_cart)


#get complete data ( 1st out of 1)
completeData_cart <- complete(imputed_Data_cart,1)

write.csv(x = cbind(na_data[,c(1:5)], completeData_cart)
          ,file = 'missingValues_cart.csv'
          ,row.names = F)

