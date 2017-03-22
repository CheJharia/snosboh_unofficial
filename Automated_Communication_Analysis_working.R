# Start the clock!
timestamp()
ptm <- proc.time()
gc()
# Establish Connection with the SQL Server
# require RODBC
library(RODBC)
library(stringr)
library(reshape2)
library(tidyverse)

dbhandle <- RODBC::odbcDriverConnect(connection = 'driver={SQL Server};server=hob-srv-16.dmz.hobsons.com.au\\MSSQLSERVER2012;database=synergygdw;trusted_connection=true') 

# load data from [recon].[dbo].[Enrolment_Data]
# data is stored in tibble format (dplyr package)

client_name <- 'HCD42' # RMIT University

query_enrolment <- paste(
"SELECT 
                CRMSNumber AS crms_number
              , CONVERT(VARCHAR(10), [Date_Created], 120) as Enrol_DateCreated
              , CONVERT(VARCHAR(10), [Semester_Start], 120) AS Enrol_DateEnrollment
              , [Comm Class] AS Enrol_CommClass
              , Channel AS Enrol_Channel
              , CASE WHEN(lower(Class) like '%enquiry%') THEN 'Enquiry Management'
                 WHEN(lower(Class) like '%offer%') THEN 'Offer Management'
              	 WHEN(lower(Channel) like '%enquiry%') THEN 'Enquiry Management'
              	 WHEN(lower(Channel) like '%offer%' ) THEN 'Offer Management'
                END AS 'Enrol_ManagementClassGrouped'
              , Semester AS Enrol_Semester
              , Level AS Enrol_Level
              , Course AS Enrol_Course
              , '1' AS Enrol_Enrolled
 FROM [recon].[dbo].[Enrolment_Data]
 WHERE Client_BK = '",client_name,"' ORDER BY crms_number desc, Enrol_DateCreated desc,
Enrol_DateEnrollment desc",sep = ""
)

Drecon.enrolment_data <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_enrolment))
str(Drecon.enrolment_data)

# Some prospects do have more than 1 enrolment, we create a column to identify this
Drecon.enrolment_data <- dplyr::left_join(Drecon.enrolment_data, Drecon.enrolment_data %>% 
                             dplyr::group_by(crms_number) %>% 
                            dplyr::summarise(OneEnrollment = ifelse(n() > 1, FALSE, TRUE)))

##################################################################################
# INTERNATIONAL PROSPECTS COMMUNICATION - ENQUIRY & MADE OFFER
##################################################################################
# load data from [synergygdw].[dbo].[TableauEnquiryMadeOfferInternationalCommunicationsView]
# to get the communication data for prospects under 'Enquiry and Made Offer'

query_IntComm <- paste(
  "SELECT  
   [ClientID] AS client_id
  ,[CRMSNumber] AS crms_number
  ,[CommunicationDate] AS Int_CommunicationDate
  ,[CommunicationDirection] AS Int_CommunicationDirection
  ,[CommunicationType] AS Int_CommunicationType
  ,[CommunicationClass] AS Int_CommunicationClass
  FROM [synergygdw].[dbo].[TableauEnquiryMadeOfferInternationalCommunicationsView]
  WHERE ClientID = '",client_name,"'",sep = "" 
)
Dsynergygdw.IntCommunication_data <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_IntComm))

Dsynergygdw.IntCommunication_data$Int_CommunicationDate <- as.Date(Dsynergygdw.IntCommunication_data$Int_CommunicationDate)

# Create Features
FInt <- Dsynergygdw.IntCommunication_data %>%
  group_by(crms_number) %>%
  summarise(FInt_Count = n())


stats_class <- reshape2::dcast(Dsynergygdw.IntCommunication_data %>% 
                   group_by(crms_number, Int_CommunicationClass ) %>%
                   summarise(n = n()), crms_number ~ Int_CommunicationClass, value.var = "n")
colnames(stats_class)[-1] <- paste("FInt", colnames(stats_class)[-1], sep = '_')
stats_class[is.na(stats_class)] <- 0                
FInt <- left_join(FInt, stats_class)

stats_type <- reshape2::dcast(Dsynergygdw.IntCommunication_data %>% 
                                 group_by(crms_number, Int_CommunicationType ) %>%
                                 summarise(n = n()), crms_number ~ Int_CommunicationType, value.var = "n")
colnames(stats_type)[-1] <- paste("FInt", colnames(stats_type)[-1], sep = '_')
stats_type[is.na(stats_type)] <- 0                
FInt <- left_join(FInt, stats_type)


stats_direction <- reshape2::dcast(Dsynergygdw.IntCommunication_data %>% 
                                group_by(crms_number, Int_CommunicationDirection ) %>%
                                summarise(n = n()), crms_number ~ Int_CommunicationDirection, value.var = "n")
colnames(stats_direction)[-1] <- paste("FInt", colnames(stats_direction)[-1], sep = '_')
stats_direction[is.na(stats_direction)] <- 0                
FInt <- left_join(FInt, stats_direction)
FInt <- FInt %>% mutate(Ratio_OUTIN = FInt_OUT / FInt_Count
                        ,Ratio_INOUT = FInt_IN / FInt_Count
                        ,Ratio_Email = FInt_Email / FInt_Count
                        ,Ratio_Phone = FInt_Phone / FInt_Count
                        ,Ratio_Enquiry = FInt_Enquiry / FInt_Count
                        ,Ratio_Response = FInt_Response / FInt_Count)
# in progress - need to get student type
query_crms_student <- paste("
SELECT crms_number, current_heat_status AS CRMS_currentHeatStatus,
heat_status_transition AS CRMS_heatStatusTransition, student_status_mod AS CRMS_StudentStatus
, prospect_type AS CRMS_ProspectType
, CASE WHEN(lower(prospect_type) like '%enquiry%') THEN 'Enquiry Management'
                 WHEN(lower(prospect_type) like '%offer%') THEN 'Offer Management'
              	 WHEN(lower(prospect_type) like '%list%') THEN 'Enquiry Management'
  END AS CRMS_ManagementClassGrouped
  FROM [synergygdw].[dbo].[crms_student]
  WHERE client_id = '",client_name,"'",sep = "" 
)

Dsynergygdw.crms_student <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_crms_student))
DFinal <- left_join(left_join(FInt, Dsynergygdw.crms_student), Drecon.enrolment_data %>% dplyr::filter(OneEnrollment == TRUE))
# label the unenrolled
DFinal[is.na(DFinal$Enrol_Enrolled),]$Enrol_Enrolled <- 0
# Enrolled students in recon data but not in commmunication data
DNoComm <- Drecon.enrolment_data[!(Drecon.enrolment_data$crms_number %in% DFinal$crms_number),]
DFinal$Enrol_ManagementClassGrouped <- as.character(DFinal$Enrol_ManagementClassGrouped)
DFinal$CRMS_ManagementClassGrouped <- as.character(DFinal$CRMS_ManagementClassGrouped)
str(DFinal)
DFinal <- DFinal %>% mutate(Final_ManagementClassGrouped = ifelse(is.na(DFinal$Enrol_ManagementClassGrouped), CRMS_ManagementClassGrouped, Enrol_ManagementClassGrouped ))

# need to clean white spaces because some functions don't accept it
colnames(DFinal) <- str_replace_all(colnames(DFinal),"\\s+","_")
DFinal <- DFinal %>% dplyr::select(-FInt_Made_Offer)
Dtrain_EnquiryManagement <- DFinal %>%
                 filter(Final_ManagementClassGrouped == 'Enquiry Management') %>% 
                 dplyr::select(starts_with('FInt'),
                        starts_with('Ratio'),
                        Enrol_Enrolled) 
Dtrain_OfferManagement <- DFinal %>% 
                 filter(Final_ManagementClassGrouped == 'Offer Management') %>% 
                 dplyr::select(starts_with('FInt'),
                        starts_with('Ratio'),
                        Enrol_Enrolled) 


# http://rstatistics.net/variable-importance-of-predictors-that-contribute-most-significantly-to-a-response-variable-in-r/

# Random Forest Method
timestamp()
library(party)
cfenquiry <- cforest( Enrol_Enrolled ~ . , data = Dtrain_EnquiryManagement, control = cforest_unbiased(mtry = 10,ntree = 50)) # fit the random forest
cfoffer <- cforest( Enrol_Enrolled ~ . , data = Dtrain_OfferManagement, control = cforest_unbiased(mtry = 10,ntree = 50)) # fit the random forest
varimp(cfenquiry) # get variable importance, based on mean decrease in accuracy
varimp(cfenquiry, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
varimpAUC(cfenquiry)  # more robust towards class imbalance.

varimp(cfoffer) # get variable importance, based on mean decrease in accuracy
varimp(cfoffer, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
varimpAUC(cfoffer)  # more robust towards class imbalance.


# MARS
library(earth)
marsModelEnquiry <- earth(Enrol_Enrolled ~ . , data = Dtrain_EnquiryManagement) # build model
evEnquiry <- evimp(marsModelEnquiry) # estimate variable importance
plot(evEnquiry)
marsModelOffer <- earth(Enrol_Enrolled ~ . , data = Dtrain_OfferManagement) # build model
evOffer <- evimp(marsModelOffer) # estimate variable importance
plot(evOffer)


# Boruta

library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_outputEnquiry <- Boruta(Enrol_Enrolled ~ . , data = Dtrain_EnquiryManagement, doTrace=2) 
boruta_signifEnquiry <- names(boruta_outputEnquiry$finalDecision[boruta_outputEnquiry$finalDecision %in% c("Confirmed", "Tentative")])  
print(boruta_signifEnquiry) 
plot(boruta_outputEnquiry, cex.axis = .7, las = 2, xlab = "", main = "Variable Importance (Enquiry)") 
boruta_outputOffer <- Boruta(Enrol_Enrolled ~ . , data = na.omit(Dtrain_OfferManagement), doTrace=2)  
boruta_signifOffer <- names(boruta_outputOffer$finalDecision[boruta_outputOffer$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signifOffer)  #
plot(boruta_outputOffer, cex.axis = .7, las = 2, xlab="", main = "Variable Importance (Offer)")  # plot variable importance

timestamp()
