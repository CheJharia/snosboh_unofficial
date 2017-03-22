library(RODBC)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(ggthemes)
library(magrittr)
library(ggExtra)

dbhandle <- RODBC::odbcDriverConnect(connection = 'driver={SQL Server};server=hob-srv-16.dmz.hobsons.com.au\\MSSQLSERVER2012;database=synergygdw;trusted_connection=true')

# GET ALL CLIENTS
Qclient <- "SELECT * FROM [synergygdw].[dbo].[client]"
DClient <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Qclient))
Lclient <- DClient$client_id


# GET ALL STUDENTS
Qstud <- paste("SELECT [client_id]
               ,[crms_number]
               ,[country_of_origin]
               ,[nationality]
               ,[country_of_residence]
               ,[gender]
               FROM [synergygdw].[dbo].[crms_student]" , sep = '')
DStud <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Qstud))

# GET ALL USER
Quser <- paste("SELECT *
               FROM [synergygdw].[dbo].[crms_user]" , sep = '')

DUser <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Quser))

# GET ALL COMM
Qcomm <- paste("SELECT *
                 FROM [synergygdw].[dbo].[crms_communication]" , sep = '')
#fname <- paste(paste('COMM', client, year, sep = '_'),'.csv', sep = '')
DComm <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Qcomm))

# BUILD BIG TABLE


