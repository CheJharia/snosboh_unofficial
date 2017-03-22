library(RODBC)
library(tidyverse)

library(dplyr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(magrittr)
timestamp()
dbhandle <- RODBC::odbcDriverConnect(connection = 'driver={SQL Server};server=hob-srv-16.dmz.hobsons.com.au\\MSSQLSERVER2012;database=synergygdw;trusted_connection=true')

Qclient <- "SELECT * FROM [synergygdw].[dbo].[client]"
Dsynergygdw.client <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Qclient))
Lclient <- Dsynergygdw.client$client_id


#dir.create(paste(getwd(), '/', client,'/', year, sep = ''))
Qstud <- paste("SELECT [client_id]
               ,[crms_number]
               ,[country_of_origin]
               ,[nationality]
               ,[country_of_residence]
               ,[gender]
               FROM [synergygdw].[dbo].[crms_student]" , sep = '')
#fname <- paste(paste('COMM', client, year, sep = '_'),'.csv', sep = '')
DStud <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Qstud))


