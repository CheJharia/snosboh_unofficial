
library(dplyr)
library(data.table)
library(magrittr)
###########################################################################
# Prepare Training Data
###########################################################################
list.files()
fn_training <- "data_global_profiled_augmented.csv"
training_data <- tbl_df(fread(fn_training, header = T, check.names = F))
training_data
str(training_data)
training_data %<>%
  mutate(`Global Response ID` = as.factor(`Global Response ID`),
         division = as.factor(division),
         profile = as.factor(profile))
str(training_data)
summary(training_data)

###########################################################################
# Prepare Test Data
###########################################################################

fn_test <- "2 output ProfileData-2016-03-31 22_19_21-all-scored-total-scores.csv"
test_data <- tbl_df(fread(fn_test, header = T, check.names = F))
test_data
str(test_data)
test_data %<>%
  mutate(`Global Response ID` = as.factor(`Global Response ID`))
str(test_data)
summary(test_data)



install.packages(c("class", "mlbench", "chemometrics"))
library(class)
library(mlbench)
