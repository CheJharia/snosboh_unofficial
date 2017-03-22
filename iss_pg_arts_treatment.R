library(cluster)
library(factoextra)
library(dplyr)
library(ggplot2)

################################################################################
## Prepare data
################################################################################
# Read iss pg arts
list.files()
iss_pg_arts <- tbl_df(read.csv(file = "ISS Profiling PG Arts.csv"
                             ,header = T
                             ,check.names = F))
summary(iss_pg_arts)
library(mice)

# missing value in each dataset
md.pattern(iss_pg_arts)
dplyr::tbl_df(md.pattern(iss_pg_arts))
library(VIM)
mice_plot <- aggr(iss_pg_arts, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iss_pg_arts), cex.axis=.9, prop = T,
                  gap=3, ylab=c("Missing data","Pattern"))
# convert all variables to factor type
col_names <- names(iss_pg_arts)
iss_pg_arts[,col_names] <- lapply(iss_pg_arts[,col_names] , factor)
