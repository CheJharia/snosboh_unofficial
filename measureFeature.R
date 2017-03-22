library(dplyr)



acu_comm <- dplyr::tbl_df(read.csv(file = "Success@ACU Comm Extract.csv"
                                ,header = T
                                ,check.names = F))
colnames(acu_comm)

acu_student_extract <- dplyr::tbl_df(read.csv(file = "Success@ACU Student Extract.csv"
                                              ,header = T
                                              ,check.names = F))
colnames(acu_student_extract)


################################################################################
################################################################################
# COMMUNICATION EXTRACT
################################################################################
################################################################################


################################################################################
# CommInOut
################################################################################

dCommInOut <- sqldf::sqldf("select CRMSNumber, CommInOut, count(*) as tot
                          from acu_comm
                          group by CRMSNumber, CommInOut")
rdCommInOut <- reshape2::dcast(dCommInOut, CRMSNumber ~ CommInOut, value.var = "tot" )
rdCommInOut[is.na(rdCommInOut)] <- 0

# rdCommInOut
# contains the count of In and Out comm for each student

################################################################################
# CommType # can be grouped by CommClass. No need to use this variable
################################################################################
dCommType <- sqldf::sqldf("select CRMSNumber, CommType, count(*) as tot
                          from acu_comm
                          group by CRMSNumber, CommType")
rdCommType <- reshape2::dcast(dCommType, CRMSNumber ~ CommType, value.var = "tot" )
rdCommType[is.na(rdCommType)] <- 0


################################################################################
# CommClass
################################################################################
dCommClass <- sqldf::sqldf("select CRMSNumber, CommClass, count(*) as tot
                          from acu_comm
                          group by CRMSNumber, CommClass")
rdCommClass <- reshape2::dcast(dCommClass, CRMSNumber ~ CommClass, value.var = "tot" )
rdCommClass[is.na(rdCommClass)] <- 0

################################################################################
# nCommCallEffectiveness nCommCallEffectiveness
################################################################################
dnCommCallEffectiveness <- sqldf::sqldf("select CRMSNumber, nCommCallEffectiveness, count(*) as tot
                          from acu_comm
                          group by CRMSNumber, nCommCallEffectiveness")
rdnCommCallEffectiveness <- reshape2::dcast(dnCommCallEffectiveness, CRMSNumber ~ nCommCallEffectiveness, value.var = "tot" )
rdnCommCallEffectiveness[is.na(rdnCommCallEffectiveness)] <- 0

################################################################################
################################################################################
# STUDENT TAG HISTORY EXTRACT
################################################################################
################################################################################

acu_student_tag_hist <- dplyr::tbl_df(read.csv(file = "Success@ACU Student Tag History Extract.csv"
                                   ,header = T
                                   ,check.names = F))
colnames(acu_student_tag_hist)

################################################################################
# TagType
################################################################################
dTagType <- sqldf::sqldf("select CRMSNumber, TagType, count(*) as tot
                                       from acu_student_tag_hist
                                       group by CRMSNumber, TagType")
rdTagType <- reshape2::dcast(dTagType, CRMSNumber ~ TagType, value.var = "tot" )
rdTagType[is.na(rdTagType)] <- 0


################################################################################
# nTagValue
################################################################################
dnTagValue <- sqldf::sqldf("select CRMSNumber, nTagValue, count(*) as tot
                                       from acu_student_tag_hist
                                       group by CRMSNumber, nTagValue")
rdnTagValue <- reshape2::dcast(dnTagValue, CRMSNumber ~ nTagValue, value.var = "tot" )
rdnTagValue[is.na(rdnTagValue)] <- 0


################################################################################
################################################################################
# SURVEY EXTRACT
################################################################################
################################################################################

acu_survey <- dplyr::tbl_df(read.csv(file = "Success@ACU Survey Extract.csv"
                                               ,header = T
                                               ,check.names = F))
colnames(acu_survey)

################################################################################
# SurveyName
################################################################################
dSurveyName <- sqldf::sqldf("select CRMSNumber, SurveyName, count(*) as tot
                                       from acu_survey
                                       group by CRMSNumber, SurveyName")
rdSurveyName <- reshape2::dcast(dSurveyName, CRMSNumber ~ SurveyName, value.var = "tot" )
rdSurveyName[is.na(rdSurveyName)] <- 0

################################################################################
# nSurveyName
################################################################################
dnSurveyName <- sqldf::sqldf("select CRMSNumber, nSurveyName, count(*) as tot
                                       from acu_survey
                                       group by CRMSNumber, nSurveyName")
rdnSurveyName <- reshape2::dcast(dnSurveyName, CRMSNumber ~ nSurveyName, value.var = "tot" )
rdnSurveyName[is.na(rdnSurveyName)] <- 0

################################################################################
# BUILDING TRAINING SET
setdiff(rdnTagValue$CRMSNumber, rdnCommCallEffectiveness$CRMSNumber)
nCRMSNumber <- as.data.frame(unique(union(union(union(union(union(union(union(union(rdnCommCallEffectiveness$CRMSNumber
             , rdnTagValue$CRMSNumber)
             , rdnSurveyName$CRMSNumber)
             , rdnTagValue$CRMSNumber)
             , rdCommClass$CRMSNumber)
             , rdCommInOut$CRMSNumber)
             , rdCommType$CRMSNumber)
             , rdTagType$CRMSNumber)
             , acu_student_extract$CRMSNumber)))
colnames(nCRMSNumber) <- "CRMSNumber"
dtraining <- NULL
dtraining <- left_join(nCRMSNumber, rdCommClass)
dtraining <- left_join(dtraining, rdCommInOut)
# dtraining <- left_join(dtraining, rdCommType)
dtraining <- left_join(dtraining, rdnCommCallEffectiveness[,-c(2)])
dtraining <- left_join(dtraining, rdnSurveyName)
dtraining <- left_join(dtraining, rdnTagValue[,-c(2)])
dtraining <- left_join(dtraining, rdTagType)

student_status <- sqldf::sqldf("select CRMSNumber, StudentStatus from acu_student_extract")
dtraining <- left_join(dtraining, student_status)
dtraining[is.na(dtraining)] <- 0
summary(dtraining)
colnames(dtraining)
names(dtraining) <- gsub("\\.","",names(dtraining))
names(dtraining)<- gsub(" ","",names(dtraining))

write.csv(x = dtraining
          ,file = "SuccessACU_variable_features.csv"
          ,row.names = F)
