library(dplyr)
library(funModeling) # https://cran.r-project.org/web/packages/funModeling/vignettes/funModeling.html
library(magrittr)
library(class)
library(gmodels)
library(caret)
library(ggthemes)
# Read Final Global data Raw Scores
final_complete_global_data <- tbl_df(read.csv(file = '2 output ProfileData-2016-03-17 15_36_30-all-scored-total-scores-23738 records.csv'
                                     ,header = T
                                     ,sep = ','
                                     ,check.names = F))
# Inspect data
str(final_complete_global_data)
summary(final_complete_global_data)

# Checking NA, zeros, data type and unique values
final_complete_global_data_status <- df_status(final_complete_global_data)
final_complete_global_data_status

# Building K-Nearest Neighbour Classifier

# load global profile data (remember this global dataset is a subset of the previous one)
global_data_profiled <- tbl_df(read.csv(file = 'global_data_5_profiles.csv'
                                        ,header = T
                                        ,sep = ','
                                        ,check.names = F))
# Rename column
colnames(global_data_profiled)[9] <- 'profile'
global_data_profiled
colnames(global_data_profiled)

# diff the two datasets to get non-profiled respondents
non_profiled_global_id <- as.data.frame(setdiff(final_complete_global_data$`Global Response ID`
        , global_data_profiled$`Global Response ID`))
colnames(non_profiled_global_id) <- 'Global Response ID'
colnames(non_profiled_global_id)
# Verification

nrow(non_profiled_global_id) + nrow(global_data_profiled) == nrow(final_complete_global_data)
# get the dataset of non profiled prospects
non_profiled_global_dataset <- final_complete_global_data[which(final_complete_global_data$`Global Response ID` %in% non_profiled_global_id$`Global Response ID`),]

global_data_profiled.scores <- select(global_data_profiled
                                      ,`CL Score`
                                      ,`EM Score`
                                      ,`LE Score`
                                      ,`TQ Score`
                                      ,`UB Score`)

library(caret)
library(klaR)

# define an 60%/40% train/test split of the dataset
# Partition the data into 60% training and 40% test dataset.
# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# http://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/

# Set a seed for reproducibility
set.seed(2016)

split=0.60
trainIndex <- createDataPartition(global_data_profiled$profile, p=split, list=FALSE)
profiles_training_set <- global_data_profiled.scores[ trainIndex,]
profiles_test_set <- global_data_profiled.scores[-trainIndex,]



profiles_test_classify <- knn(train = profiles_training_set
                              , test = profiles_test_set
                              ,cl = global_data_profiled$profile[trainIndex],
                              k=1)
confusion_matrix_knn <- CrossTable(x = global_data_profiled$profile[-trainIndex], y = profiles_test_classify, prop.chisq = F)
# Model Accuracy
sum(diag(confusion_matrix_knn$t))/sum(confusion_matrix_knn$t)

################################################
# 
################################################

non_profiled_global_dataset_test_set <- as.data.frame(dplyr::select(non_profiled_global_dataset
                                                          ,`CL Score`
                                                          ,`EM Score`
                                                          ,`LE Score`
                                                          ,`TQ Score`
                                                          ,`UB Score`))

profiles_non_profiled_classify <- knn(train = global_data_profiled.scores
                                , test = non_profiled_global_dataset_test_set
                                ,cl = global_data_profiled$profile,
                                k=5)
new_profiled <- cbind(non_profiled_global_id,non_profiled_global_dataset_test_set, profiles_non_profiled_classify)
colnames(new_profiled)[7] <- 'profile'
head(new_profiled)
## Check classification quality
ggplot(new_profiled , aes(x = `CL Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(new_profiled , aes(x = `EM Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip()  + ggtitle("Employment Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(new_profiled , aes(x = `LE Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Lifestyle Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(new_profiled , aes(x = `TQ Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip()  + ggtitle("Teaching Quality Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(new_profiled , aes(x = `UB Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("University Brand Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


# create another column 'division'
new_profiled %<>%
  mutate(division = as.factor(substr(new_profiled$`Global Response ID`, start = 1, stop = 4 )))
# save the newly profiled prospects
write.csv(x = new_profiled
          ,file = 'new_profiled.csv', row.names = F)
