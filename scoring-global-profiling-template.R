
library(dplyr)
library(magrittr)

profile_data_global_22_04 <- tbl_df(read.csv(file = 'ProfileData (Global) 2016-04-22.csv'
         ,header = T
         ,check.names = F))

profile_data_global_22_04

# subset to good records only: NumMandatoryFieldsCompleted == 19
profile_data_global_22_04_complete_scores <- profile_data_global_22_04 %>%
  filter(NumMandatoryFieldsCompleted == 19)

#summary(profile_data_global_22_04_complete_scores)
# all are assigned to a global division

# subset apac and drop factor levels
APAC_profile_data_global_22_04_complete_scores <- droplevels(profile_data_global_22_04_complete_scores %>%
  filter(`University_Global Division` == 'APAC'))
#summary(APAC_profile_data_global_22_04_complete_scores) # verif


#####################################################################################
# Scoring APAC
#####################################################################################

# Scoring Cost of Living
CL1_multiplier <- 3
CL2_multiplier <- 1
CL3_multiplier <- 2
CL4_multiplier <- 1
CL5_multiplier <- 1
CL_total_multiplier <- CL1_multiplier + CL2_multiplier + CL3_multiplier + CL4_multiplier + CL5_multiplier
CL1_normalized <- CL1_multiplier / CL_total_multiplier
CL2_normalized <- CL2_multiplier / CL_total_multiplier
CL3_normalized <- CL3_multiplier / CL_total_multiplier
CL4_normalized <- CL4_multiplier / CL_total_multiplier
CL5_normalized <- CL5_multiplier / CL_total_multiplier
# Verif
CL1_normalized + CL2_normalized + CL3_normalized + CL4_normalized + CL5_normalized == 1


APAC_profile_data_global_22_04_complete_scores %<>%
  mutate(CL1 = as.numeric(CL1)
         ,CL2 = as.numeric(CL2)
         ,CL3 = as.numeric(CL3)
         ,CL4 = as.numeric(CL4)
         ,CL5 = as.numeric(CL5)
         ,CL1_score = CL1/max(CL1)*CL1_normalized
         ,CL2_score = CL2/max(CL2)*CL2_normalized
         ,CL3_score = CL3/max(CL3)*CL3_normalized
         ,CL4_score = CL4/max(CL4)*CL4_normalized
         ,CL5_score = CL5/max(CL5)*CL5_normalized
         ,CL_tot_score = (CL1_score + CL2_score + CL3_score + CL4_score + CL5_score)*20)


# Scoring Employment
EM1_multiplier <- 3
EM2_multiplier <- 1
EM3_multiplier <- 2
EM4_multiplier <- 1
EM_total_multiplier <- EM1_multiplier + EM2_multiplier + EM3_multiplier + EM4_multiplier
EM1_normalized <- EM1_multiplier / EM_total_multiplier
EM2_normalized <- EM2_multiplier / EM_total_multiplier
EM3_normalized <- EM3_multiplier / EM_total_multiplier
EM4_normalized <- EM4_multiplier / EM_total_multiplier
# Verif

EM1_normalized + EM2_normalized + EM3_normalized + EM4_normalized == 1

#APAC_profile_data_global_22_04_complete_scores[9516:9530,c('EM1','EM1_score', 'EM_tot_score')]
levels(APAC_profile_data_global_22_04_complete_scores$EM1) <- c('6','5','4','3','2','1','0')

APAC_profile_data_global_22_04_complete_scores %<>%
  mutate(EM1 = as.numeric(as.character(EM1)) # converting factors to numeric
         ,EM2 = as.numeric(EM2)
         ,EM3 = as.numeric(EM3)
         ,EM4 = as.numeric(EM4)
         ,EM1_score = EM1/6*5*EM1_normalized
         ,EM2_score = EM2*EM2_normalized
         ,EM3_score = EM3*EM3_normalized
         ,EM4_score = EM4*EM4_normalized
         ,EM_tot_score = (EM1_score + EM2_score + EM3_score + EM4_score)*4)
# verif
#APAC_profile_data_global_22_04_complete_scores[9516:9530,c('EM1','EM1_score', 'EM_tot_score')]


# Scoring Lifestyle
LE1_multiplier <- 1
LE2_multiplier <- 2
LE3_multiplier <- 1
LE4_multiplier <- 1
LE_total_multiplier <- LE1_multiplier + LE2_multiplier + LE3_multiplier + LE4_multiplier
LE1_normalized <- LE1_multiplier / LE_total_multiplier
LE2_normalized <- LE2_multiplier / LE_total_multiplier
LE3_normalized <- LE3_multiplier / LE_total_multiplier
LE4_normalized <- LE4_multiplier / LE_total_multiplier
# Verif

LE1_normalized + LE2_normalized + LE3_normalized + LE4_normalized == 1

#APAC_profile_data_global_22_04_complete_scores[9516:9530,c('LE1','LE1_score', 'LE_tot_score')]


levels(APAC_profile_data_global_22_04_complete_scores$LE1) <- c('6','5','4','3','2','1','0')
levels(APAC_profile_data_global_22_04_complete_scores$LE2) <- c('6','5','4','3','2','2','1')

APAC_profile_data_global_22_04_complete_scores %<>%
  mutate(LE1 = as.numeric(as.character(LE1)) # converting factors to numeric
         ,LE2 = as.numeric(as.character(LE2)) # converting factors to numeric
         ,LE3 = as.numeric(LE3)
         ,LE4 = as.numeric(LE4)
         ,LE1_score = LE1/6*5*LE1_normalized
         ,LE2_score = LE2/6*5*LE2_normalized
         ,LE3_score = LE3*LE3_normalized
         ,LE4_score = LE4*LE4_normalized
         ,LE_tot_score = (LE1_score + LE2_score + LE3_score + LE4_score)*4) 
# verif
#APAC_profile_data_global_22_04_complete_scores[9516:9530,c('LE1','LE1_score', 'LE_tot_score')]
#summary(APAC_profile_data_global_22_04_complete_scores)


# Scoring Teaching Quality
TQ1_multiplier <- 3
TQ2_multiplier <- 1
TQ3_multiplier <- 1
TQ4_multiplier <- 1
TQ_total_multiplier <- TQ1_multiplier + TQ2_multiplier + TQ3_multiplier + TQ4_multiplier
TQ1_normalized <- TQ1_multiplier / TQ_total_multiplier
TQ2_normalized <- TQ2_multiplier / TQ_total_multiplier
TQ3_normalized <- TQ3_multiplier / TQ_total_multiplier
TQ4_normalized <- TQ4_multiplier / TQ_total_multiplier
# Verif
# weird that 1 == 1 is false
TQ1_normalized + TQ2_normalized + TQ3_normalized + TQ4_normalized == 1



APAC_profile_data_global_22_04_complete_scores %<>%
  mutate(TQ1 = as.numeric(TQ1) # converting factors to numeric
         ,TQ2 = as.numeric(TQ2) # converting factors to numeric
         ,TQ3 = as.numeric(TQ3)
         ,TQ4 = as.numeric(TQ4)
         ,TQ1_score = TQ1*TQ1_normalized
         ,TQ2_score = TQ2*TQ2_normalized
         ,TQ3_score = TQ3*TQ3_normalized
         ,TQ4_score = TQ4*TQ4_normalized
         ,TQ_tot_score = (TQ1_score + TQ2_score + TQ3_score + TQ4_score)*4) 
# verif
#APAC_profile_data_global_22_04_complete_scores[9516:9530,c('TQ1','TQ1_score', 'TQ_tot_score')]
#summary(APAC_profile_data_global_22_04_complete_scores)


# Scoring University Brand
UB1_multiplier <- 3
UB2_multiplier <- 2
UB3_multiplier <- 1
UB4_multiplier <- 1
UB_total_multiplier <- UB1_multiplier + UB2_multiplier + UB3_multiplier + UB4_multiplier
UB1_normalized <- UB1_multiplier / UB_total_multiplier
UB2_normalized <- UB2_multiplier / UB_total_multiplier
UB3_normalized <- UB3_multiplier / UB_total_multiplier
UB4_normalized <- UB4_multiplier / UB_total_multiplier
# Verif
# weird that 1 == 1 is false
UB1_normalized + UB2_normalized + UB3_normalized + UB4_normalized == 1
#str(APAC_profile_data_global_22_04_complete_scores)
# replace blanks with 0
levels(APAC_profile_data_global_22_04_complete_scores$UB3) <- c('0','1','2','3','4','5')
levels(APAC_profile_data_global_22_04_complete_scores$UB4) <- c('1','2','3','4','5')


APAC_profile_data_global_22_04_complete_scores %<>%
  mutate(UB1 = as.numeric(UB1) # converting factors to numeric
         ,UB2 = as.numeric(UB2) # converting factors to numeric
         ,UB3 = as.numeric(as.character(UB3))
         ,UB4 = as.numeric(as.character(UB4))
         ,UB1_score = UB1*UB1_normalized
         ,UB2_score = UB2/18*5*UB2_normalized
         ,UB3_score = UB3*UB3_normalized
         ,UB4_score = UB4*UB4_normalized
         ,UB_tot_score = (UB1_score + UB2_score + UB3_score + UB4_score)*4) 
# verif
#APAC_profile_data_global_22_04_complete_scores[9516:9530,c('UB1','UB2','UB3','UB4','UB1_score','UB2_score','UB3_score','UB4_score', 'UB_tot_score')]
#summary(APAC_profile_data_global_22_04_complete_scores)

APAC_scores <- APAC_profile_data_global_22_04_complete_scores %>% 
  select(`University_Global Division`
         ,`Metadata_Global Response ID`
         , CL_tot_score
         , EM_tot_score
         , LE_tot_score
         , TQ_tot_score
         , UB_tot_score)


#####################################################################################
# Scoring EMEA
#####################################################################################

# subset emea and drop factor levels
EMEA_profile_data_global_22_04_complete_scores <- droplevels(profile_data_global_22_04_complete_scores %>%
                                                               filter(`University_Global Division` == 'EMEA'))
#summary(EMEA_profile_data_global_22_04_complete_scores) # verif


# Scoring Cost of Living
CL1_multiplier <- 3
CL2_multiplier <- 1
CL3_multiplier <- 2
CL4_multiplier <- 1
CL5_multiplier <- 1
CL_total_multiplier <- CL1_multiplier + CL2_multiplier + CL3_multiplier + CL4_multiplier + CL5_multiplier
CL1_normalized <- CL1_multiplier / CL_total_multiplier
CL2_normalized <- CL2_multiplier / CL_total_multiplier
CL3_normalized <- CL3_multiplier / CL_total_multiplier
CL4_normalized <- CL4_multiplier / CL_total_multiplier
CL5_normalized <- CL5_multiplier / CL_total_multiplier
# Verif
CL1_normalized + CL2_normalized + CL3_normalized + CL4_normalized + CL5_normalized == 1


EMEA_profile_data_global_22_04_complete_scores %<>%
  mutate(CL1 = ifelse(CL1 == 'Agree',3,ifelse(CL1 == 'Disagree',1,2))
         ,CL2 = ifelse(CL2 == 'Agree',3,ifelse(CL2 == 'Disagree',1,2))
         ,CL3 = as.numeric(CL3)
         ,CL4 = as.numeric(CL4)
         ,CL5 = as.numeric(CL5)
         ,CL1_score = CL1*CL1_normalized
         ,CL2_score = CL2*CL2_normalized
         ,CL3_score = CL3/5*4*CL3_normalized
         ,CL4_score = CL4/5*4*CL4_normalized
         ,CL5_score = CL5/5*4*CL5_normalized
         ,CL_tot_score = (CL1_score + CL2_score + CL3_score + CL4_score + CL5_score)/3.6*20)

#select(EMEA_profile_data_global_22_04_complete_scores,`Metadata_Global Response ID`,contains('CL'))[6860:6900,]



# Scoring Employment
EM1_multiplier <- 3
EM2_multiplier <- 1
EM3_multiplier <- 2
EM4_multiplier <- 1

EM_total_multiplier <- EM1_multiplier + EM2_multiplier + EM3_multiplier + EM4_multiplier 
EM1_normalized <- EM1_multiplier / EM_total_multiplier
EM2_normalized <- EM2_multiplier / EM_total_multiplier
EM3_normalized <- EM3_multiplier / EM_total_multiplier
EM4_normalized <- EM4_multiplier / EM_total_multiplier

# Verif
EM1_normalized + EM2_normalized + EM3_normalized + EM4_normalized  == 1

levels(EMEA_profile_data_global_22_04_complete_scores$EM1) <- c('6','5','4','3','2','1')

EMEA_profile_data_global_22_04_complete_scores %<>%
  mutate( EM1 = as.numeric(as.character(EM1))
         ,EM2 = ifelse(EM2 == 'Agree',3,ifelse(EM2 == 'Disagree',1,2))
         ,EM3 = ifelse(EM3 == 'Agree',3,ifelse(EM3 == 'Disagree',1,2))
         ,EM4 = as.numeric(EM4)
         ,EM1_score = EM1/6*5*EM1_normalized
         ,EM2_score = EM2/3*5*EM2_normalized
         ,EM3_score = EM3/3*5*EM3_normalized
         ,EM4_score = EM4*EM4_normalized
         ,EM_tot_score = (EM1_score + EM2_score + EM3_score + EM4_score)*4)

#select(EMEA_profile_data_global_22_04_complete_scores,`Metadata_Global Response ID`,contains('EM'))[6860:6900,]



# Scoring Lifestyle
LE1_multiplier <- 1
LE2_multiplier <- 2
LE3_multiplier <- 1
LE4_multiplier <- 1

LE_total_multiplier <- LE1_multiplier + LE2_multiplier + LE3_multiplier + LE4_multiplier 
LE1_normalized <- LE1_multiplier / LE_total_multiplier
LE2_normalized <- LE2_multiplier / LE_total_multiplier
LE3_normalized <- LE3_multiplier / LE_total_multiplier
LE4_normalized <- LE4_multiplier / LE_total_multiplier

# Verif
LE1_normalized + LE2_normalized + LE3_normalized + LE4_normalized  == 1

levels(EMEA_profile_data_global_22_04_complete_scores$LE1) <- c('6','5','4','3','2','1')
levels(EMEA_profile_data_global_22_04_complete_scores$LE2) <- c('6','5','4','3','2','1')
EMEA_profile_data_global_22_04_complete_scores %<>%
  mutate( LE1 = as.numeric(as.character(LE1))
          ,LE2 = as.numeric(as.character(LE2))
          ,LE3 = as.numeric(LE3)
          ,LE4 = as.numeric(LE4)
          ,LE1_score = LE1/6*5*LE1_normalized
          ,LE2_score = LE2/6*5*LE2_normalized
          ,LE3_score = LE3*LE3_normalized
          ,LE4_score = LE4*LE4_normalized
          ,LE_tot_score = (LE1_score + LE2_score + LE3_score + LE4_score)*4)

#select(EMEA_profile_data_global_22_04_complete_scores,`Metadata_Global Response ID`,contains('LE'))[6860:6900,]


# Scoring Teaching Quality
TQ1_multiplier <- 3
TQ2_multiplier <- 1
TQ3_multiplier <- 1
TQ4_multiplier <- 1

TQ_total_multiplier <- TQ1_multiplier + TQ2_multiplier + TQ3_multiplier + TQ4_multiplier 
TQ1_normalized <- TQ1_multiplier / TQ_total_multiplier
TQ2_normalized <- TQ2_multiplier / TQ_total_multiplier
TQ3_normalized <- TQ3_multiplier / TQ_total_multiplier
TQ4_normalized <- TQ4_multiplier / TQ_total_multiplier

# Verif
TQ1_normalized + TQ2_normalized + TQ3_normalized + TQ4_normalized  == 1

levels(EMEA_profile_data_global_22_04_complete_scores$TQ1) <- c('6','5','4','3','2','1')

EMEA_profile_data_global_22_04_complete_scores %<>%
  mutate( TQ1 = ifelse(TQ1 == 'Agree',3,ifelse(TQ1 == 'Disagree',1,2))
          ,TQ2 = as.numeric(TQ2)
          ,TQ3 = as.numeric(TQ3)
          ,TQ4 = as.numeric(TQ4)
          ,TQ1_score = TQ1/3*5*TQ1_normalized
          ,TQ2_score = TQ2*TQ2_normalized
          ,TQ3_score = TQ3*TQ3_normalized
          ,TQ4_score = TQ4*TQ4_normalized
          ,TQ_tot_score = (TQ1_score + TQ2_score + TQ3_score + TQ4_score)*4)

#select(EMEA_profile_data_global_22_04_complete_scores,`Metadata_Global Response ID`,contains('TQ'))[6860:6900,]


# Scoring University Brand
UB1_multiplier <- 3
UB2_multiplier <- 2
UB3_multiplier <- 1
UB4_multiplier <- 1
UB_total_multiplier <- UB1_multiplier + UB2_multiplier + UB3_multiplier + UB4_multiplier
UB1_normalized <- UB1_multiplier / UB_total_multiplier
UB2_normalized <- UB2_multiplier / UB_total_multiplier
UB3_normalized <- UB3_multiplier / UB_total_multiplier
UB4_normalized <- UB4_multiplier / UB_total_multiplier
# Verif
# weird that 1 == 1 is false
UB1_normalized + UB2_normalized + UB3_normalized + UB4_normalized == 1
#str(EMEA_profile_data_global_22_04_complete_scores)

levels(EMEA_profile_data_global_22_04_complete_scores$UB3) <- c('0','20','11','10','9','8','7','6','5','4','3','2','19','1','18','17','16','16','15','14','13','12')



EMEA_profile_data_global_22_04_complete_scores %<>%
  mutate(UB1 = as.numeric(UB1) # converting factors to numeric
         ,UB2 = as.numeric(UB2) # converting factors to numeric
         ,UB3 = as.numeric(as.character(UB3))
         ,UB4 = ifelse(UB4 == 'Agree',3,ifelse(UB4 == 'Disagree',1,2))
         ,UB1_score = UB1*UB1_normalized
         ,UB2_score = UB2/22*5*UB2_normalized
         ,UB3_score = UB3/20*5*UB3_normalized
         ,UB4_score = UB4/3*5*UB4_normalized
         ,UB_tot_score = (UB1_score + UB2_score + UB3_score + UB4_score)*4) 
# verif

#select(EMEA_profile_data_global_22_04_complete_scores,`Metadata_Global Response ID`,contains('UB'))[6860:6900,]

EMEA_scores <- EMEA_profile_data_global_22_04_complete_scores %>% 
  select(`University_Global Division`
         ,`Metadata_Global Response ID`
         , CL_tot_score
         , EM_tot_score
         , LE_tot_score
         , TQ_tot_score
         , UB_tot_score)

# Combine EMEA and APAC
global_new_scores <-rbind(EMEA_scores, APAC_scores)
#############################################################################################################
# Profiling those who are not yet profiled

# read the profiled dataset ( Training Set of 18,776 )

global_data_profiled <- tbl_df(read.csv(file = 'global_data_5_profiles-18776- TRAINING SET.csv'
                                        ,check.names = F
                                        ,header = T))

# Rename column
colnames(global_data_profiled)[9] <- 'profile'
global_data_profiled
colnames(global_data_profiled)

# diff the two datasets to get non-profiled respondents
non_profiled_global_id <- as.data.frame(setdiff(global_new_scores$`Metadata_Global Response ID`
                                                , global_data_profiled$`Global Response ID`))
# 2 respondents, probably need to check why
#non_profiled_global_id <- as.data.frame(setdiff(global_data_profiled$`Global Response ID`,global_new_scores$`Metadata_Global Response ID`))
colnames(non_profiled_global_id) <- 'Global Response ID'
colnames(non_profiled_global_id)

# get the dataset of non profiled prospects
non_profiled_global_dataset <- global_new_scores[which(global_new_scores$`Metadata_Global Response ID` %in% non_profiled_global_id$`Global Response ID`),]


global_data_profiled.scores <- select(global_data_profiled
                                      ,`CL Score`
                                      ,`EM Score`
                                      ,`LE Score`
                                      ,`TQ Score`
                                      ,`UB Score`)

non_profiled_global_dataset_test_set <- tbl_df(as.data.frame(dplyr::select(non_profiled_global_dataset
                                                                    ,`CL_tot_score`
                                                                    ,`EM_tot_score`
                                                                    ,`LE_tot_score`
                                                                    ,`TQ_tot_score`
                                                                    ,`UB_tot_score`)))
library(caret)
library(klaR)
library(class)
library(gmodels)
library(ggthemes)
# Set a seed for reproducibility
set.seed(2016)
profiles_non_profiled_classify <- knn(train = global_data_profiled.scores
                                      , test = non_profiled_global_dataset_test_set
                                      ,cl = global_data_profiled$profile,
                                      k=5)
new_profiled <- tbl_df(cbind(non_profiled_global_id,non_profiled_global_dataset_test_set, profiles_non_profiled_classify))
new_profiled
colnames(new_profiled)[7] <- 'profile'
head(new_profiled)
colnames(new_profiled)[2:6] <- c("CL Score"
                                 ,"EM Score"
                                 ,"LE Score"
                                 ,"TQ Score"
                                 ,"UB Score")
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


write.csv(new_profiled, file = 'new_profiled_22-04-dataset.csv', row.names = F)

