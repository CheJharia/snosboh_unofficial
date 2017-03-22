# Start the clock!

ptm <- proc.time()

# Load libraries
library(tidyverse)
library(magrittr)
library(sqldf)

data_crms_communication <- "HCD40_crms_communication.csv"
data_crms_student_status_history <- "HCD40_crms_student_status_history.csv"
data_enrolment <- "HCD40_enrolment_data.csv"
### Read CRMS Communication ----------------------------------------------------
crms_comm <- readr::read_csv(file = data_crms_communication
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
# Convert in out as factor
crms_comm$in_out <- as.factor(crms_comm$in_out)

# Calculate CommDurationDay, CommCount, CommFreqDay
# CommDurationDay: Days of communication from first communication date to the
#                  last communication date
# CommCount: Total number of communication for each prospect
# CommFreqDay: Frequency of communication in days
features_crms_comm <-crms_comm %>%
  group_by(crms_number) %>%
  summarise(CommDurationDay = ifelse(round(difftime(last(date_opened),first(date_opened), unit='day'),2) < 1, 1,
                                     round(difftime(last(date_opened),first(date_opened), unit='day'),2)),
            CommCount = n(),
            CommFreqDay= round( CommCount/ CommDurationDay,2))
features_crms_comm$CommDurationDay <- as.integer(features_crms_comm$CommDurationDay)


# Calculate number of communication by communication class
library(stringr)
crms_comm$class <- as.factor(crms_comm$class)
levels(crms_comm$class)<-stringr::str_replace_all(levels(crms_comm$class)," ","")
nb_class<- sqldf::sqldf("select crms_number, class, count(*) as nb
                        from crms_comm
                        group by crms_number, class")
NB_class <- reshape2::dcast(nb_class, crms_number ~ class, value.var = "nb" )
NB_class[is.na(NB_class)] <- 0
colnames(NB_class)[-1] <- paste("CommunicationClass",colnames(NB_class)[-1],sep = "")
features_crms_comm <- left_join(features_crms_comm,NB_class)
rm(nb_class, NB_class)

# Calculate number of Communication In and Communication OUT
nb_in_out <- sqldf::sqldf("select crms_number, in_out, count(*) as nb
                          from crms_comm
                          group by crms_number, in_out")

NB_in_out <- reshape2::dcast(nb_in_out, crms_number ~ in_out, value.var = "nb" )
NB_in_out[is.na(NB_in_out)] <- 0
colnames(NB_in_out)[-1] <- c("Communication_IN","Communication_OUT")
features_crms_comm <- left_join(features_crms_comm, NB_in_out)
rm(nb_in_out, NB_in_out)


# Calculate Comm<TYPE>Ratio, Comm<CLASS>FreqDay
features_crms_comm <-left_join(features_crms_comm,features_crms_comm %>%
                                 group_by(crms_number) %>%
                                 summarise(CommEmailRatio= round(CommunicationClassEmail / CommCount, 2),
                                           CommPhoneRatio= round(CommunicationClassPhone / CommCount, 2),
                                           CommWebRatio= round(CommunicationClassWeb / CommCount, 2),
                                           CommOutRatio = round(Communication_OUT/CommCount, 2),
                                           CommInRatio = round(Communication_IN/CommCount, 2),
                                           
                                           CommEmailFreqDay = round(CommunicationClassEmail /CommDurationDay , 2),
                                           CommPhoneFreqDay = round(CommunicationClassPhone /CommDurationDay , 2),
                                           CommWebFreqDay = round(CommunicationClassWeb /CommDurationDay , 2),
                                           CommOutFreqDay = round(Communication_OUT /CommDurationDay , 2),
                                           CommInFreqDay = round(Communication_IN /CommDurationDay , 2)))

# Calculate number of Phone IN and Phone OUT
ph_in_out <- sqldf::sqldf("select crms_number, in_out, class, count(*) as nb
                          from crms_comm where class = 'Phone' group by crms_number, in_out, class")
PH_in_out <- reshape2::dcast(ph_in_out, crms_number ~ in_out+class, value.var = "nb" )
PH_in_out[is.na(PH_in_out)] <- 0
summary(ph_in_out)
colnames(PH_in_out)[-1] <- c("Comm_Phone_IN","Comm_Phone_OUT")
features_crms_comm <- left_join(features_crms_comm, PH_in_out)
features_crms_comm[is.na(features_crms_comm)] <- 0
features_crms_comm <-left_join(features_crms_comm,features_crms_comm %>% group_by(crms_number) %>%
                                 summarise(Comm_Phone_IN_ratio = ifelse(CommunicationClassPhone > 0, round(Comm_Phone_IN / CommunicationClassPhone, 2),0) ,
                                           Comm_Phone_OUT_ratio = ifelse(CommunicationClassPhone > 0, round(Comm_Phone_OUT / CommunicationClassPhone, 2),0)))
rm(ph_in_out, PH_in_out)

### Read CRMS Student Status History ----------------------------------------------------
crms_student_status_history <- readr::read_csv(file = data_crms_student_status_history
                                               ,progress = T
                                               ,col_names = c("client_id"
                                                              ,"crms_number")
                                               , col_types =  c("ci")
                                               ,skip = 1)
crms_student_status_history <- crms_student_status_history %>% group_by(crms_number) %>% filter(row_number(crms_number) ==1)

### Read Enrolment Data ----------------------------------------------------
enrolment_data <- readr::read_csv(file = data_enrolment
                                  ,progress = T
                                  ,col_names = c("Client_BK"
                                                 ,"CRMSNumber"
                                                 ,"Comm Class"
                                                 ,"Channel"
                                                 ,"Class"
                                                 ,"cClass")
                                  , col_types =  c("cicccc")
                                  ,skip = 1)
enrolment_data$`Comm Class` <- as.factor(enrolment_data$`Comm Class`)
enrolment_data$Channel <- as.factor(enrolment_data$Channel)
enrolment_data$Class <- as.factor(enrolment_data$Class)
enrolment_data$cClass <- as.factor(enrolment_data$cClass)
# remove NULL enrolment class
enrolment_data <- droplevels(enrolment_data[!enrolment_data$cClass == 'NULL',])
summary(enrolment_data)
crms_student_status <- full_join(crms_student_status_history, enrolment_data, by = c("crms_number" ="CRMSNumber") )
nEnrol <- crms_student_status[is.na(crms_student_status$Client_BK),]
nEnrol$Status <- "Not Enrolled"
Enrol <- crms_student_status[!is.na(crms_student_status$Client_BK),]
Enrol$Status <- "Enrolled"
crms_student_status <- rbind(nEnrol, Enrol) %>% arrange(crms_number) %>% select(client_id, crms_number, Status)

features <- inner_join(crms_student_status
                       , features_crms_comm)
colnames(features)[3] <- "Is_Enrolled"

features$Is_Enrolled <- as.factor(features$Is_Enrolled)
features$Is_Enrolled <- factor(features$Is_Enrolled, levels = c("Not Enrolled", "Enrolled"))
library(funModeling)

correlation_table(data = features %>% ungroup%>% select(-crms_number)
                  ,str_target="Is_Enrolled")

# Generate Cross, Box, Density plots using Is_Enrolled as target variable 
# to select best variables
# http://livebook.datascienceheroes.com/selecting_best_variables
# 
cross_enrolled=cross_plot(features[,-c(1,2)], str_target="Is_Enrolled", path_out = "cross_Enrolled_Enr_data", auto_binning = TRUE)

plotar(data=features[,-c(1,2)], str_target="Is_Enrolled", plot_type = "boxplot", path_out = "boxplot_Enrolled_Enr_data")

plotar(data=features[,-c(1,2)], str_target="Is_Enrolled", plot_type = "histdens", path_out = "density_Enrolled_Enr_data")

###############################################################################
# Treating Outliers:
# http://livebook.datascienceheroes.com/data_preparation/outliers_treatment.html
#  Stopping variables at a certain value, 1% for example, you are telling to the model: consider all extremes values as if they are on the 99% percentile, this value is already high enough
###############################################################################

# Treating Top 1% and Bot 1% as outliers

featuresTB1pct <- tbl_df(cbind(as.data.frame(features[,c(1,2,3)])
                               , as.data.frame(tbl_df(prep_outliers(data = features[,-c(1,2,3)], type='stop'
                                                                    , top_percent = 0.01
                                                                    , bottom_percent = 0.01)))))

correlation_table(data = featuresTB1pct %>% ungroup%>% select(-crms_number,-client_id)
                  ,str_target="Is_Enrolled")


cross_enrolled=cross_plot(featuresTB1pct[,-c(1,2)], str_target="Is_Enrolled", path_out = "cross_Enrolled_Enr_data_TB1pct", auto_binning = TRUE)

plotar(data=featuresTB1pct[,-c(1,2)], str_target="Is_Enrolled", plot_type = "boxplot", path_out = "boxplot_Enrolled_Enr_data_TB1pct")

plotar(data=featuresTB1pct[,-c(1,2)], str_target="Is_Enrolled", plot_type = "histdens", path_out = "density_Enrolled_Enr_data_TB1pct")




# Treating Top 5% and Bot 5% as outliers

featuresTB5pct <- dplyr::tbl_df(cbind(as.data.frame(features[,c(1,2,3)])
                               , as.data.frame(tbl_df(prep_outliers(data = features[,-c(1,2,3)], type='stop'
                                                                    , top_percent = 0.05
                                                                    , bottom_percent = 0.05)))))

correlation_table(data = featuresTB5pct %>% ungroup%>% select(-crms_number,-client_id)
                  ,str_target="Is_Enrolled")



cross_enrolled=cross_plot(featuresTB5pct[,-c(1,2)], str_target="Is_Enrolled", path_out = "cross_Enrolled_Enr_data_TB5pct", auto_binning = TRUE)

plotar(data=featuresTB5pct[,-c(1,2)], str_target="Is_Enrolled", plot_type = "boxplot", path_out = "boxplot_Enrolled_Enr_data_TB5pct")

plotar(data=featuresTB5pct[,-c(1,2)], str_target="Is_Enrolled", plot_type = "histdens", path_out = "density_Enrolled_Enr_data_TB5pct")
str(featuresTB5pct)
plotar(data=featuresTB5pct[,-c(1,2)], str_target="Is_Enrolled", str_input = "CommDurationDay", plot_type = "histdens", path_out = "density_Enrolled_Enr_data_TB5pct")
summary(featuresTB5pct$CommDurationDay)

plotar(data=featuresTB5pct[,-c(1,2)], str_target="Is_Enrolled", str_input = "CommDurationDay", plot_type = "histdens", path_out = "density_Enrolled_Enr_data_TB5pct")
summary(featuresTB5pct$CommDurationDay)

## Loading funModeling
suppressMessages(library(funModeling)) 
str(features)

correlation_table(data = features[,-c(1)], 
                  str_target="Is_Enrolled")

fit_glm=glm(Is_Enrolled ~ Comm_Phone_OUT + CommCount + CommEmailRatio 
            + CommPhoneRatio, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Is_Enrolled')

# Stop the clock
proc.time() - ptm



