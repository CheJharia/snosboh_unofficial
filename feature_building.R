library(tidyverse)
library(magrittr)
library(sqldf)
# read crms_comm extract
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
str(crms_comm)
summary(crms_comm)
crms_comm$in_out <- as.factor(crms_comm$in_out)
crms_comm_complete <- crms_comm[complete.cases(crms_comm),]
str(crms_comm_complete)
summary(crms_comm_complete)
length(unique(crms_comm$crms_number)) # 101 134
length(unique(crms_comm_complete$crms_number)) # 82 682
crms_comm <- crms_comm_complete
rm(crms_comm_complete)


comm <-crms_comm %>%
  group_by(crms_number) %>%
  summarise(FirstCommDate = first(date_opened),
            LastCommDate = last(date_opened),
            CommDurationDay = round(difftime(last(date_opened),first(date_opened), unit='day'),2),
            CommCount = n(),
            CommFreqDay= round(CommDurationDay / CommCount,2))
summary(comm)
##################################################
# Feature extract from [crms_student_status_history]
##################################################
# read crms_comm extract
crms_student_status_history <- readr::read_csv(file = "25102016_HCD40_crms_student_status_history.csv"
                             ,progress = T
                             ,col_names = c("client_id"
                                            ,"crms_number"
                                            ,"date_modified"
                                            ,"student_status_mod"
                                            ,"intake_status_mod")
                             , col_types =  c("ciTcc")
                             ,skip = 1)
str(crms_student_status_history)
summary(crms_student_status_history)

crms_student_status_history_features <-crms_student_status_history %>%
  group_by(crms_number) %>%
  summarise(FirstStudentStatusDate = first(date_modified),
            LastStudentStatusDate = last(date_modified),
            StudentStatusDurationHour = round(difftime(last(date_modified),first(date_modified), unit='hour'),2),
            StudentStatusDurationDay = round(difftime(last(date_modified),first(date_modified), unit='day'),2),
            StudentStatusCount = n(),
            StatusEnrolled= ifelse('Enrolled' %in% student_status_mod,1,0))
length(unique(crms_student_status_history_features$crms_number)) # 104716


##################################################
# Feature extract from crms_communication extract
##################################################

# 1)Number of Communication
nb_comm <- sqldf::sqldf("select crms_number, count(*) as crms_comm_total_comm_number
                        from crms_comm
                        group by crms_number")
crms_comm_features <- cbind(nb_comm)
rm(nb_comm)
summary(crms_comm_features)

# 1)Number of in_out
nb_in_out <- sqldf::sqldf("select crms_number, in_out, count(*) as nb
                          from crms_comm
                           group by crms_number, in_out")

NB_in_out <- reshape2::dcast(nb_in_out, crms_number ~ in_out, value.var = "nb" )
summary(nb_in_out)
colnames(NB_in_out)[-1] <- c("comm_IN","comm_OUT")
crms_comm_features <- left_join(crms_comm_features, NB_in_out)
rm(nb_in_out, NB_in_out)
summary(crms_comm_features)

# 3)Number of class
nb_class<- sqldf::sqldf("select crms_number, class, count(*) as nb
                        from crms_comm
                        group by crms_number, class")
NB_class <- reshape2::dcast(nb_class, crms_number ~ class, value.var = "nb" )
NB_class[is.na(NB_class)] <- 0
colnames(NB_class)[-1] <- paste("comm_class",colnames(NB_class)[-1])
crms_comm_features <- left_join(crms_comm_features,NB_class)
rm(nb_class, NB_class)
summary(crms_comm_features)



##################################################
# Feature extract from student extract
##################################################


# read crms_student extract
crms_student <- readr::read_csv(file = "20102016_HCD40_crms_student.csv"
                             ,progress = F)


##################################################
# Feature extract from student tag history extract
##################################################

# read crms_student extract
crms_student_tag_history <- readr::read_csv(file = "20102016_HCD40_crms_student_tag_history.csv"
                                               ,progress = T)
colnames(crms_student_tag_history)
# 2)Number of tag_type
nb_tag_type <- sqldf::sqldf("select crms_number, tag_type, count(*) as nb
                                      from crms_student_tag_history
                                      group by crms_number, tag_type")
NB_tag_type <- reshape2::dcast(nb_tag_type, crms_number ~ tag_type, value.var = "nb" )
NB_tag_type[is.na(NB_tag_type)] <- 0
colnames(NB_tag_type)[-1] <- paste("tag_history_tag_type",colnames(NB_tag_type)[-1])
crms_student_tag_history_features <- cbind(NB_tag_type)
rm(nb_tag_type, NB_tag_type)
summary(crms_student_tag_history_features)


##################################################
# Feature extract from [TableauEnquiryInternationalCommunicationsView]
##################################################
list.files()
# read crms_student extract
view_EnquiryMOIC <- readr::read_csv(file = "20102016_HCD40_TableauEnquiryInternationalCommunicationsView.csv"
                                            ,progress = T)
colnames(view_EnquiryMOIC)

summary(view_EnquiryMOIC)

# 1)Number of Communication
nb_comm <- sqldf::sqldf("select CRMSNumber, count(*) as view_EnquiryMOIC_total_comm_number
                            from view_EnquiryMOIC
                            group by CRMSNumber")
view_EnquiryMOIC_features <- cbind(nb_comm)
rm(nb_comm)
summary(view_EnquiryMOIC_features)


# 2)Number of CommunicationDirection
nb_CommunicationDirection <- sqldf::sqldf("select CRMSNumber, CommunicationDirection, count(*) as nb
                            from view_EnquiryMOIC
                                          group by CRMSNumber, CommunicationDirection")
NB_CommunicationDirection <- reshape2::dcast(nb_CommunicationDirection, CRMSNumber ~ CommunicationDirection, value.var = "nb" )
NB_CommunicationDirection[is.na(NB_CommunicationDirection)] <- 0
colnames(NB_CommunicationDirection)[-1] <- paste("view_EnquiryMOIC",colnames(NB_CommunicationDirection)[-1])
view_EnquiryMOIC_features <- left_join(view_EnquiryMOIC_features, NB_CommunicationDirection)
rm(nb_CommunicationDirection, NB_CommunicationDirection)
summary(view_EnquiryMOIC_features)


# 3)Number of CommunicationType
nb_CommunicationType <- sqldf::sqldf("select CRMSNumber, CommunicationType, count(*) as nb
                                          from view_EnquiryMOIC
                                          group by CRMSNumber, CommunicationType")
NB_CommunicationType <- reshape2::dcast(nb_CommunicationType, CRMSNumber ~ CommunicationType, value.var = "nb" )
NB_CommunicationType[is.na(NB_CommunicationType)] <- 0
colnames(NB_CommunicationType)[-1] <- paste("view_EnquiryMOIC",colnames(NB_CommunicationType)[-1])
view_EnquiryMOIC_features <- left_join(view_EnquiryMOIC_features,NB_CommunicationType)
rm(nb_CommunicationType, NB_CommunicationType)
summary(view_EnquiryMOIC_features)

# 4)Number of CommunicationClass
nb_CommunicationClass <- sqldf::sqldf("select CRMSNumber, CommunicationClass, count(*) as nb
                                     from view_EnquiryMOIC
                                     group by CRMSNumber, CommunicationClass")
NB_CommunicationClass <- reshape2::dcast(nb_CommunicationClass, CRMSNumber ~ CommunicationClass, value.var = "nb" )
NB_CommunicationClass[is.na(NB_CommunicationClass)] <- 0
colnames(NB_CommunicationClass)[-1] <- paste("view_EnquiryMOIC",colnames(NB_CommunicationClass)[-1])
view_EnquiryMOIC_features <- left_join(view_EnquiryMOIC_features,NB_CommunicationClass)
rm(nb_CommunicationClass, NB_CommunicationClass)
summary(view_EnquiryMOIC_features)
colnames(view_EnquiryMOIC_features)[1] <- "crms_number"

##################################################
# Feature extract from [TableauMadeOfferInternationalCommunicationsView]
##################################################
list.files()
# read crms_student extract
view_MOIC <- readr::read_csv(file = "20102016_HCD40_TableauMadeOfferInternationalCommunicationsView.csv"
                                    ,progress = T)
colnames(view_MOIC)

summary(view_MOIC)

# 1)Number of Communication
nb_comm <- sqldf::sqldf("select CRMSNumber, count(*) as view_MOIC_total_comm_number
                        from view_MOIC
                        group by CRMSNumber")
view_MOIC_features <- cbind(nb_comm)
rm(nb_comm)
summary(view_MOIC_features)


# 2)Number of CommunicationDirection
nb_CommunicationDirection <- sqldf::sqldf("select CRMSNumber, CommunicationDirection, count(*) as nb
                                          from view_MOIC
                                          group by CRMSNumber, CommunicationDirection")
NB_CommunicationDirection <- reshape2::dcast(nb_CommunicationDirection, CRMSNumber ~ CommunicationDirection, value.var = "nb" )
NB_CommunicationDirection[is.na(NB_CommunicationDirection)] <- 0
colnames(NB_CommunicationDirection)[-1] <- paste("view_MOIC",colnames(NB_CommunicationDirection)[-1])
view_MOIC_features <- left_join(view_MOIC_features, NB_CommunicationDirection)
rm(nb_CommunicationDirection, NB_CommunicationDirection)
summary(view_MOIC_features)


# 3)Number of CommunicationType
nb_CommunicationType <- sqldf::sqldf("select CRMSNumber, CommunicationType, count(*) as nb
                                     from view_MOIC
                                     group by CRMSNumber, CommunicationType")
NB_CommunicationType <- reshape2::dcast(nb_CommunicationType, CRMSNumber ~ CommunicationType, value.var = "nb" )
NB_CommunicationType[is.na(NB_CommunicationType)] <- 0
colnames(NB_CommunicationType)[-1] <- paste("view_MOIC",colnames(NB_CommunicationType)[-1])
view_MOIC_features <- left_join(view_MOIC_features,NB_CommunicationType)
rm(nb_CommunicationType, NB_CommunicationType)
summary(view_MOIC)

# 4)Number of CommunicationClass
nb_CommunicationClass <- sqldf::sqldf("select CRMSNumber, CommunicationClass, count(*) as nb
                                      from view_MOIC
                                      group by CRMSNumber, CommunicationClass")
NB_CommunicationClass <- reshape2::dcast(nb_CommunicationClass, CRMSNumber ~ CommunicationClass, value.var = "nb" )
NB_CommunicationClass[is.na(NB_CommunicationClass)] <- 0
colnames(NB_CommunicationClass)[-1] <- paste("view_MOIC",colnames(NB_CommunicationClass)[-1])
view_MOIC_features <- left_join(view_MOIC_features,NB_CommunicationClass)
rm(nb_CommunicationClass, NB_CommunicationClass)
summary(view_MOIC_features)
colnames(view_MOIC_features)[1] <- "crms_number"

##################################################
# Feature extract from [TableauMadeOfferInternationalCommunicationsView]
##################################################
list.files()
# read crms_student extract
view_TPAS <- readr::read_csv(file = "20102016_HCD40_TableauProspectsActivitySummary.csv"
                             ,progress = T)
colnames(view_TPAS)

summary(view_TPAS)

################
# Combine features

training <- left_join(left_join(left_join(crms_comm_features, crms_student_tag_history_features)
                      ,view_EnquiryMOIC_features)
                      ,view_MOIC_features)
training$nb_na <- apply(training, 1, function(x) sum(is.na(x)))
write.csv(x = training
          ,file = "training.csv"
          ,row.names = F)
#training <- training[complete.cases(training),]
#training[is.na(training)] <- 0
#table(crms_student$student_status_mod)
#training <- left_join(training, crms_student %>% select(crms_number, student_status_mod))
#training$student_status_mod <- as.factor(training$student_status_mod)
#ummary(training)

training <- right_join(training, view_TPAS %>% select(CRMSNumber,StudentStatus)
                       ,by = c("crms_number" = "CRMSNumber"))

training$StudentStatus <- as.factor(training$StudentStatus)
table(training$StudentStatus)
training <- training %>% filter(StudentStatus == "Enrolled")
?right_join
table(training$nb_na)
