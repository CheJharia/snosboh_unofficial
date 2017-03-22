library(tidyverse)
library(magrittr)
library(sqldf)

### Read CRMS Communication ----------------------------------------------------
crms_comm <- readr::read_csv(file = "25102016_HCD40_crms_communication.csv"
                             ,progress = T
                             ,col_names = c("client_id"
                                            ,"crms_number"
                                            ,"date_opened"
                                            ,"date_sent"
                                            ,"minuteToOpen"
                                            ,"hourToOpen"
                                            ,"dayToOpen"
                                            ,"in_out"
                                            ,"type"
                                            ,"class"
                                            ,"bounce_back")
                             , col_types =  c("ciTTiiiccci")
                             ,skip = 1)
crms_comm$in_out <- as.factor(crms_comm$in_out)
str(crms_comm)
summary(crms_comm)


features_crms_comm <-crms_comm %>%
  group_by(crms_number) %>%
  summarise(CommDurationDay = round(difftime(last(date_opened),first(date_opened), unit='day'),2),
            CommCount = n(),
            CommFreqDay= round(CommDurationDay / CommCount,2))
summary(features_crms_comm)
features_crms_comm$CommDurationDay <- as.integer(features_crms_comm$CommDurationDay)
# 3)Number of class
nb_class<- sqldf::sqldf("select crms_number, class, count(*) as nb
                        from crms_comm
                        group by crms_number, class")
NB_class <- reshape2::dcast(nb_class, crms_number ~ class, value.var = "nb" )
NB_class[is.na(NB_class)] <- 0
colnames(NB_class)[-1] <- paste("CommunicationClass",colnames(NB_class)[-1],sep = "")
features_crms_comm <- left_join(features_crms_comm,NB_class)
rm(nb_class, NB_class)
summary(features_crms_comm)

features_crms_comm <-left_join(features_crms_comm,features_crms_comm %>%
  group_by(crms_number) %>%
  summarise(CommEmailFreq= round(CommunicationClassEmail / CommCount,2),
            CommPhoneFreq= round(CommunicationClassPhone / CommCount,2),
            CommWebFreq= round(CommunicationClassWeb / CommCount,2)))
### Read CRMS Communication ----------------------------------------------------
crms_student_status_history <- readr::read_csv(file = "25102016_HCD40_crms_student_status_history.csv"
                                               ,progress = T
                                               ,col_names = c("client_id"
                                                              ,"crms_number"
                                                              ,"date_modified"
                                                              ,"student_status_mod"
                                                              ,"intake_status_mod")
                                               , col_types =  c("ciTcc")
                                               ,skip = 1)
crms_student_status_history$student_status_mod <- as.factor(crms_student_status_history$student_status_mod)
crms_student_status_history$intake_status_mod <- as.factor(crms_student_status_history$intake_status_mod)
str(crms_student_status_history)
summary(crms_student_status_history)
levels(crms_student_status_history$student_status_mod)
features_crms_student_status_history <-crms_student_status_history %>%
  group_by(crms_number) %>%
  summarise(Enrolled= ifelse('Enrolled' %in% student_status_mod,1,0))


# 3)Number of Status
library(stringr)
levels(crms_student_status_history$student_status_mod)<-stringr::str_replace_all(levels(crms_student_status_history$student_status_mod)," ","")
nb_student_status_mod<- sqldf::sqldf("select crms_number, student_status_mod, count(*) as nb
                        from crms_student_status_history
                        group by crms_number, student_status_mod")
NB_student_status_mod <- reshape2::dcast(nb_student_status_mod, crms_number ~ student_status_mod, value.var = "nb" )
NB_student_status_mod[is.na(NB_student_status_mod)] <- 0
colnames(NB_student_status_mod)[-1] <- paste("StudentStatusMod",colnames(NB_student_status_mod)[-1],sep = "")
features_crms_comm <- left_join(features_crms_comm,NB_student_status_mod)
rm(nb_student_status_mod, NB_student_status_mod)

length(unique(features_crms_student_status_history$crms_number)) # 104716
str(features_crms_student_status_history)
str(features_crms_comm)
features <- left_join(features_crms_student_status_history
                      , features_crms_comm)
# rearrange crms_number & StatusEnrolled
refcols <- c("crms_number")
features <- features[, c(refcols, setdiff(names(features), refcols))]
features <- features[complete.cases(features),]
str(features)





library(funModeling)


correlation_table(data = features[,-c(1)], 
                  str_target="Enrolled")
# generate all plots and cross with has_heart_disease

cross_enrolled=cross_plot(features[,-c(1)], str_target="Enrolled", path_out = "cross_Enrolled", auto_binning = TRUE)

plotar(data=features[,-c(1)], str_target="Enrolled", plot_type = "boxplot", path_out = "boxplot_Enrolled")

plotar(data=features[,-c(1)], str_target="Enrolled", plot_type = "histdens", path_out = "density_Enrolled")



## Loading funModeling
suppressMessages(library(funModeling)) 
str(features)

correlation_table(data = features[,-c(1)], 
                  str_target="Enrolled")

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq + CommCount 
            + CommunicationClassEmail + CommDurationDay, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq + CommCount 
            , data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq + CommCount 
            + CommunicationClassEmail, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

