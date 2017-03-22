library(VIM)
source(file = "main.R")
mice_plot <- aggr(iss_2016_international_good_vars_cluster_data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iss_2016_international_good_vars_cluster_data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

?mice
numNAs <- apply(X = iss_2016_international_good_vars_cluster_data
                , MARGIN = 1
                , FUN = function(z) sum(is.na(z)))
# remove rows with more than 3 NAs
iss_2016_international_good_vars_cluster_data_3na <- iss_2016_international_good_vars_cluster_data[!(numNAs >  3),]

imputed_Data <- mice(iss_2016_international_good_vars_cluster_data_3na
                     , m = 1
                     , maxit = 1
                     , method = 'rf', seed = 1990)
completeData <- complete(imputed_Data,1)

rm(list= ls()[!(ls() %in% c('iss_2016_international_good_vars_cluster_data'
                            ,'iss_2016_good_vars'
                            ,'iss_2016'
                            ,'completeData'))])
warnings()
