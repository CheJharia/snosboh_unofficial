library(dplyr)
library(funModeling) # https://cran.r-project.org/web/packages/funModeling/vignettes/funModeling.html
library(magrittr)
library(class)
library(gmodels)
library(caret)
library(ggthemes)
library(dplyr)
library(data.table)
library(magrittr)
###########################################################################
# Prepare Training Data : Using APAC 5 profiles data
###########################################################################
list.files()
fn_training <- "apac_data_5_profiles.csv"
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
# Prepare Test Data : Using VU Wellington raw data
# latest raw : global_data_5_profiles-28229.csv
###########################################################################

# read vuw
vuw.raw.scores <- tbl_df(read.csv(file = 'VUW-New-Scores.csv'
                                  ,header = T
                                  ,sep = ','
                                  ,check.names = F))

# Inspect data
str(vuw.raw.scores)
summary(vuw.raw.scores)

# Checking NA, zeros, data type and unique values
vuw.raw.scores_status <- df_status(vuw.raw.scores)
vuw.raw.scores_status

###########################################################################
# Build Knn-Classifier
###########################################################################
library(caret)
library(klaR)


apac_data_profiled.scores <- dplyr::select(training_data
                                           ,`CL Score`
                                           ,`EM Score`
                                           ,`LE Score`
                                           ,`TQ Score`
                                           ,`UB Score`)


# define an 60%/40% train/test split of the dataset
# Partition the data into 60% training and 40% test dataset.
# http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# http://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
split=0.60
trainIndex <- createDataPartition(training_data$profile, p=split, list=FALSE)
profiles_training_set <- apac_data_profiled.scores[ trainIndex,]
profiles_test_set <- apac_data_profiled.scores[-trainIndex,]

profiles_test_classify <- knn(train = profiles_training_set
                              , test = profiles_test_set
                              ,cl = training_data$profile[trainIndex],
                              k=1)
confusion_matrix_knn <- CrossTable(x = training_data$profile[-trainIndex], y = profiles_test_classify, prop.chisq = F)
# Model Accuracy
sum(diag(confusion_matrix_knn$t))/sum(confusion_matrix_knn$t)


################################################
# Apply Knn Model to agent scores
################################################

vuw.raw.scores_test_set <- as.data.frame(dplyr::select(vuw.raw.scores
                                                       ,`CL-Transform`
                                                       ,`EM-Transform`
                                                       ,`LE-Transform`
                                                       ,`TQ-Transform`
                                                       ,`UB-Transform`))

vuw_classify <- knn(train = apac_data_profiled.scores
                    , test = vuw.raw.scores_test_set
                    ,cl = training_data$profile,
                    k=5)
vuw_profiled <- cbind(vuw.raw.scores, vuw_classify)
colnames(vuw_profiled)[10] <- 'profile'


################################################
# Check classification quality
################################################

ggplot(vuw_profiled , aes(x = `CL-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(training_data , aes(x = `CL Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(vuw_profiled , aes(x = `EM-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip()  + ggtitle("Employment Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(vuw_profiled , aes(x = `LE-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Lifestyle Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(vuw_profiled , aes(x = `TQ-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip()  + ggtitle("Teaching Quality Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(vuw_profiled , aes(x = `UB-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("University Brand Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


#write.csv(x = vuw_profiled
#          ,file = 'vuw_reprofiled_apac_model.csv'
#          ,row.names = F)
