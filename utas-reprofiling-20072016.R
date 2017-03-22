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
# Prepare Test Data : Using Grow Wellington raw data
# latest raw : 20160627003426-SurveyExport.csv
###########################################################################

# read utas
utas.raw.scores <- tbl_df(read.csv(file = 'UTAS-New-PCA-Scores.csv'
                                   ,header = T
                                   ,sep = ','
                                   ,check.names = F))

utas.raw.scores <- tbl_df(read.csv(file = 'UTAS-New-PCA-Scores - 2.csv'
                                   ,header = T
                                   ,sep = ','
                                   ,check.names = F))

# Inspect data
str(utas.raw.scores)
summary(utas.raw.scores)

# Checking NA, zeros, data type and unique values
utas.raw.scores_status <- df_status(utas.raw.scores)
utas.raw.scores_status

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

utas.raw.scores_test_set <- as.data.frame(dplyr::select(utas.raw.scores
                                                        ,`CL-Transform`
                                                        ,`EM-Transform`
                                                        ,`LE-Transform`
                                                        ,`TQ-Transform`
                                                        ,`UB-Transform`))

utas_agent_classify <- knn(train = apac_data_profiled.scores
                           , test = utas.raw.scores_test_set
                           ,cl = training_data$profile,
                           k=5)
utas_profiled <- cbind(utas.raw.scores, utas_agent_classify)
colnames(utas_profiled)[11] <- 'profile'


################################################
# Check classification quality
################################################

ggplot(utas_profiled , aes(x = `CL-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(training_data , aes(x = `CL Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(utas_profiled , aes(x = `EM-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip()  + ggtitle("Employment Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(training_data , aes(x = `EM Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(utas_profiled , aes(x = `LE-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Lifestyle Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(training_data , aes(x = `LE Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Lifestyle Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(utas_profiled , aes(x = `TQ-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip()  + ggtitle("Teaching Quality Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(training_data , aes(x = `TQ Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))


ggplot(utas_profiled , aes(x = `UB-Transform`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("University Brand Score distribution")+ facet_grid(. ~ profile, margins = T) + theme_tufte()+ scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))

ggplot(training_data , aes(x = `UB Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = profile, fill = profile),
               geom = "ribbon", position = "identity") +
  coord_flip() + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ profile, margins = T) + theme_tufte() + scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1")) + scale_color_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051", "#00D1C1"))



utas_profiled$profile <- as.factor(utas_profiled$profile)
# Rename all levels
levels(utas_profiled$profile) <- c("Profile-A","Profile-B",
                                        "Profile-C", "Profile-D",
                                        "Profile-E")
droplevels(utas_profiled)

utas_profiled_augmented <- utas_profiled %>%
  mutate(`CL percentage rank` = percent_rank(`CL-Transform`),
         `EM percentage rank` = percent_rank(`EM-Transform`),
         `LE percentage rank` = percent_rank(`LE-Transform`),
         `TQ percentage rank` = percent_rank(`TQ-Transform`),
         `UB percentage rank` = percent_rank(`UB-Transform`),
         `CL Rating` = ntile(`CL percentage rank`,3),
         `EM Rating` = ntile(`EM percentage rank`,3),
         `LE Rating` = ntile(`LE percentage rank`,3),
         `TQ Rating` = ntile(`TQ percentage rank`,3),
         `UB Rating` = ntile(`UB percentage rank`,3))


library(dplyr)
names(utas_profiled_augmented)
spider_data_CL <- utas_profiled_augmented %>%
  dplyr::select(`CL Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`CL Rating`)) %>%
  mutate(theme = 'CL')
spider_data_CL %<>%
  mutate(CL = median) %>%
  dplyr::select(profile, CL)

spider_data_EM <- utas_profiled_augmented %>%
  dplyr::select(`EM Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`EM Rating`)) %>%
  mutate(theme = 'EM')

spider_data_EM %<>%
  mutate(EM = median) %>%
  dplyr::select(EM)

spider_data_LE <- utas_profiled_augmented %>%
  dplyr::select(`LE Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`LE Rating`)) %>%
  mutate(theme = 'LE')

spider_data_LE %<>%
  mutate(LE = median) %>%
  dplyr::select(LE)

spider_data_TQ <- utas_profiled_augmented %>%
  dplyr::select(`TQ Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`TQ Rating`)) %>%
  mutate(theme = 'TQ')

spider_data_TQ %<>%
  mutate(TQ = median) %>%
  dplyr::select(TQ)

spider_data_UB <- utas_profiled_augmented %>%
  dplyr::select(`UB Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`UB Rating`)) %>%
  mutate(theme = 'UB')

spider_data_UB %<>%
  mutate(UB = median) %>%
  dplyr::select(UB)


spider_data <- cbind(spider_data_CL,
                     spider_data_EM,
                     spider_data_LE,
                     spider_data_TQ,
                     spider_data_UB)

library(ggradar)

library(scales)

spider_data$profile <- as.character(spider_data$profile)
colnames(spider_data)[2:6] <- c("Cost of Living", "Employment", "Lifestyle", 
                                "Teaching Quality", "University Brand")

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3), axis.label.size = 3) + ggtitle("Radar Chart - Profile comparisons") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[1,], axis.label.size = 3) + ggtitle("Radar Chart - Profile A") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[2,], axis.label.size = 3) + ggtitle("Radar Chart - Profile B") + scale_colour_manual(values = "#FFB400") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[3,], axis.label.size = 3) + ggtitle("Radar Chart - Profile C") + scale_colour_manual(values = "#007A87") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[4,], axis.label.size = 3) + ggtitle("Radar Chart - Profile D")+ scale_colour_manual(values = "#8CE071") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[5,], axis.label.size = 3) + ggtitle("Radar Chart - Profile E")+ scale_colour_manual(values = "#7B0051") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

#write.csv(x = utas_profiled_augmented
#          ,file = 'utas_reprofiled_apac_model_PCA-Scores-augmented-2.csv'
#          ,row.names = F)
#utas_profiled_augmented <- read.csv(file = 'utas_reprofiled_apac_model_PCA-Scores-augmented-2.csv', header = T,check.names = F)
