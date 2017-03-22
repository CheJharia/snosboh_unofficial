# load all data
library(dplyr)
library(magrittr)
library(data.table)
list.files()


apac_iss_2016 <- tbl_df(fread(input = "20160602030951-SurveyExport.csv"
                              ,sep = ","
                              ,verbose = T
                              ,check.names = F
                              ))

apac_iss_2016 <- tbl_df(read.csv(file = "20160602030951-SurveyExport.csv"
                                 ,header = T
                                 ,check.names = F
                                 ))

# filter to griffith subset
griffith_iss_2016 <- apac_iss_2016 %>%
  filter(`Link Name` == 'Griffith')
levels(apac_iss_2016$`Link Name`)

# http://www.r-bloggers.com/package-funmodeling-data-cleaning-importance-variable-analysis-and-model-perfomance/
# selecting important variables
# http://www.analyticsvidhya.com/blog/2016/03/select-important-variables-boruta-package/

library(funModeling)
griffith_iss_2016_status <- df_status(griffith_iss_2016)
apac_iss_2016_status <- df_status(apac_iss_2016)
#
