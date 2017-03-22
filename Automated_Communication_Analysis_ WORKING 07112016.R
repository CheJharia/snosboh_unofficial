
# Establish Connection with the SQL Server
# require RODBC
library(RODBC)

dbhandle <- RODBC::odbcDriverConnect(connection = 'driver={SQL Server};server=hob-srv-16.dmz.hobsons.com.au\\MSSQLSERVER2012;database=synergygdw;trusted_connection=true') 

# load data from [recon].[dbo].[Enrolment_Data]
# data is stored in tibble format (dplyr package)
library(tidyverse)
client_name <- 'HCD42' # RMIT University

query_enrolment <- paste(
"SELECT 
                Client_BK as client_id, CRMSNumber AS crms_number
              , CONVERT(VARCHAR(10), [Date_Created], 120) as Enrol_DateCreated
              , CONVERT(VARCHAR(10), [Semester_Start], 120) AS Enrol_DateEnrollment
              , [Comm Class] AS Enrol_CommClass
              , Channel AS Enrol_Channel
              , CASE WHEN(lower(Class) like '%enquiry%') THEN 'Enquiry Management'
                 WHEN(lower(Class) like '%offer%') THEN 'Offer Management'
              	 WHEN(lower(Channel) like '%enquiry%') THEN 'Enquiry Management'
              	 WHEN(lower(Channel) like '%offer%' ) THEN 'Offer Management'
                END AS 'Enrol_ClassGrouped'
              , Level AS Enrol_Level
              , '1' AS Enrol_Enrolled
 FROM [recon].[dbo].[Enrolment_Data]
 WHERE Client_BK = '",client_name,"'",sep = ""
)

Drecon.enrolment_data <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_enrolment))
str(Drecon.enrolment_data)

##################################################################################
# INTERNATIONAL PROSPECTS COMMUNICATION - ENQUIRY & MADE OFFER
##################################################################################
# load data from [synergygdw].[dbo].[TableauEnquiryInternationalCommunicationsView]
# to get the communication data for prospects under 'Enquiry'
# load data from [synergygdw].[dbo].[TableauMadeOfferInternationalCommunicationsView]
# to get the communication data for prospects under 'Made Offer'
query_MadeOfferIntComm <- paste(
  "SELECT  
   [ClientID] AS client_id
  ,[CRMSNumber] AS crms_number
  ,[CommunicationDate] AS MOInt_CommunicationDate
  ,[CommunicationDirection] AS MOInt_CommunicationDirection
  ,[CommunicationType] AS MOInt_CommunicationType
  ,[CommunicationClass] AS MOInt_CommunicationClass
  ,'1' AS 'MOInt_MadeOffer'
  FROM [synergygdw].[dbo].[TableauMadeOfferInternationalCommunicationsView]
  WHERE ClientID = '",client_name,"'",sep = "" 
)
Dsynergygdw.madeOfferIntCommunication_data <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_MadeOfferIntComm))

Dsynergygdw.madeOfferIntCommunication_data$MOInt_CommunicationDate <- as.Date(Dsynergygdw.madeOfferIntCommunication_data$MOInt_CommunicationDate)


query_EnquiryIntComm <- paste(
  "SELECT  
   [ClientID] AS client_id
  ,[CRMSNumber] AS crms_number
  ,[CommunicationDate] AS EQInt_CommunicationDate
  ,[CommunicationDirection] AS EQInt_CommunicationDirection
  ,[CommunicationType] AS EQInt_CommunicationType
  ,[CommunicationClass] AS EQInt_CommunicationClass
  ,'1' AS 'EQInt_Enquiry'
  FROM [synergygdw].[dbo].[TableauEnquiryInternationalCommunicationsView]
  WHERE ClientID = '",client_name,"'",sep = "" 
)

Dsynergygdw.enquiryIntCommunication_data <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_EnquiryIntComm))
Dsynergygdw.enquiryIntCommunication_data$EQInt_CommunicationDate <- as.Date(Dsynergygdw.enquiryIntCommunication_data$EQInt_CommunicationDate)
##################################################################################
# DOMESTIC PROSPECTS COMMUNICATION - ENQUIRY & MADE OFFER
##################################################################################
# load data from [synergygdw].[dbo].[TableauEnquiryDomesticCommunicationsView]
# to get the communication data for prospects under 'Enquiry'
# load data from [synergygdw].[dbo].[TableauMadeOfferDomesticCommunicationsView]
# to get the communication data for prospects under 'Made Offer'
query_MadeOfferDomComm <- paste(
  "SELECT  
   [ClientID] AS client_id
  ,[CRMSNumber] AS crms_number
  ,[CommunicationDate] AS MODom_CommunicationDate
  ,[CommunicationDirection] AS MODom_CommunicationDirection
  ,[CommunicationType] AS MODom_CommunicationType
  ,[CommunicationClass] AS MODom_CommunicationClass
  ,'1' AS 'MODom_MadeOffer'
  FROM [synergygdw].[dbo].[TableauMadeOfferDomesticCommunicationsView]
  WHERE ClientID = '",client_name,"'",sep = "" 
)
Dsynergygdw.madeOfferDomCommunication_data <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_MadeOfferDomComm))
Dsynergygdw.madeOfferDomCommunication_data$MODom_CommunicationDate <- as.Date(Dsynergygdw.madeOfferDomCommunication_data$MODom_CommunicationDate)

query_EnquiryDomComm <- paste(
  "SELECT  
   [ClientID] AS client_id
  ,[CRMSNumber] AS crms_number
  ,[CommunicationDate] AS EQDom_CommunicationDate
  ,[CommunicationDirection] AS EQDom_CommunicationDirection
  ,[CommunicationType] AS EQDom_CommunicationType
  ,[CommunicationClass] AS EQDom_CommunicationClass
  ,'1' AS 'EQDom_Enquiry'
  FROM [synergygdw].[dbo].[TableauEnquiryDomesticCommunicationsView]
  WHERE ClientID = '",client_name,"'",sep = "" 
)

Dsynergygdw.enquiryDomCommunication_data <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_EnquiryDomComm))
Dsynergygdw.enquiryDomCommunication_data$EQDom_CommunicationDate <- as.Date(Dsynergygdw.enquiryDomCommunication_data$EQDom_CommunicationDate)

# Create Features
FIntEnq <- Dsynergygdw.enquiryIntCommunication_data %>%
  group_by(crms_number) %>%
  summarise(FIntEnq_DurationDay = ifelse(round(difftime(last(EQInt_CommunicationDate),first(EQInt_CommunicationDate), unit = 'day'), 2) < 1, 1,
                                               round(difftime(last(EQInt_CommunicationDate), first(EQInt_CommunicationDate), unit = 'day', 2)))
            ,FIntEnq_Count = n()
            ,FIntEnq_FrequencyDay = round(FIntEnq_Count/FIntEnq_DurationDay, 2)
            )

FDomEnq <- Dsynergygdw.enquiryDomCommunication_data %>%
  group_by(crms_number) %>%
  summarise(FDomEnq_DurationDay = ifelse(round(difftime(last(EQDom_CommunicationDate),first(EQDom_CommunicationDate), unit = 'day'), 2) < 1, 1,
                                         round(difftime(last(EQDom_CommunicationDate), first(EQDom_CommunicationDate), unit = 'day', 2)))
            ,FDomEnq_Count = n()
            ,FDomEnq_FrequencyDay = round(FDomEnq_Count/FDomEnq_DurationDay, 2)
  )

FIntMO <- Dsynergygdw.madeOfferIntCommunication_data %>%
  group_by(crms_number) %>%
  summarise(FIntMO_DurationDay = ifelse(round(difftime(last(MOInt_CommunicationDate),first(MOInt_CommunicationDate), unit = 'day'), 2) < 1, 1,
                                         round(difftime(last(MOInt_CommunicationDate), first(MOInt_CommunicationDate), unit = 'day', 2)))
            ,FIntMO_Count = n()
            ,FIntMO_FrequencyDay = round(FIntMO_Count/FIntMO_DurationDay, 2)
  )

FDomMO <- Dsynergygdw.madeOfferDomCommunication_data %>%
  group_by(crms_number) %>%
  summarise(FDomEnq_DurationDay = ifelse(round(difftime(last(MODom_CommunicationDate),first(MODom_CommunicationDate), unit = 'day'), 2) < 1, 1,
                                         round(difftime(last(MODom_CommunicationDate), first(MODom_CommunicationDate), unit = 'day', 2)))
            ,FDomEnq_Count = n()
            ,FDomEnq_FrequencyDay = round(FDomEnq_Count/FDomEnq_DurationDay, 2)
  )

library(stringr)
library(reshape2)
stats_class <- reshape2::dcast(Dsynergygdw.enquiryIntCommunication_data %>% 
                   group_by(crms_number, EQInt_CommunicationClass ) %>%
                   summarise(n = n()), crms_number ~ EQInt_CommunicationClass, value.var = "n")
colnames(stats_class)[-1] <- paste("FIntEnq", colnames(stats_class)[-1], sep = '_')
stats_class[is.na(stats_class)] <- 0                
FIntEnq <- cbind(FIntEnq, stats_class)

stats_class <- reshape2::dcast(Dsynergygdw.madeOfferIntCommunication_data %>% 
                                 group_by(crms_number, MOInt_CommunicationClass ) %>%
                                 summarise(n = n()), crms_number ~ MOInt_CommunicationClass, value.var = "n")
colnames(stats_class)[-1] <- paste("FIntMO", colnames(stats_class)[-1], sep = '_')
stats_class[is.na(stats_class)] <- 0                
FIntMO <- cbind(FIntMO, stats_class)

stats_class <- reshape2::dcast(Dsynergygdw.enquiryDomCommunication_data %>% 
                                 group_by(crms_number, EQDom_CommunicationClass ) %>%
                                 summarise(n = n()), crms_number ~ EQDom_CommunicationClass, value.var = "n")
colnames(stats_class)[-1] <- paste("FDomEnq", colnames(stats_class)[-1], sep = '_')
stats_class[is.na(stats_class)] <- 0                
FDomEnq <- cbind(FIntEnq, stats_class)

stats_class <- reshape2::dcast(Dsynergygdw.madeOfferDomCommunication_data %>% 
                                 group_by(crms_number, MODom_CommunicationClass ) %>%
                                 summarise(n = n()), crms_number ~ MODom_CommunicationClass, value.var = "n")
colnames(stats_class)[-1] <- paste("FDomMO", colnames(stats_class)[-1], sep = '_')
stats_class[is.na(stats_class)] <- 0                
FDomMO <- cbind(FIntEnq, stats_class)



