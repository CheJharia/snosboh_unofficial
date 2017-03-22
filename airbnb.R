library(ggplot2)
library(ggmap)
library(sp)
library(grid)
library(geosphere)
library(plyr)

# source the theme_map for ggplot2
source("https://dl.dropboxusercontent.com/u/2364714/theme_map.R")


library(RODBC)
library(tidyverse)

dbhandle <- RODBC::odbcDriverConnect(connection = 'driver={SQL Server};server=hob-srv-16.dmz.hobsons.com.au\\MSSQLSERVER2012;database=synergygdw;trusted_connection=true')
Qclient <- "SELECT client_id, client_name FROM [synergygdw].[dbo].[client]"
Dsynergygdw.client <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Qclient))
Dsynergygdw.client$client_name <- as.character(Dsynergygdw.client$client_name)
# filter to 'good naming' university first
Dsynergygdw.client <- Dsynergygdw.client %>% filter(client_name %in% c('James Cook University'
                                                 ,'Auckland University of Technology'
                                                 ,'University of Balllarat'
                                                 ,'Insearch'
                                                 ,'Kangan Institute'
                                                 ,'Macquarie University'
                                                 ,'The University of Queensland'
                                                 ,'Western Sydney University'
                                                 ,'The University of Sydney'
                                                 ,'The University of Waikato'
                                                 ,'University of Technology Sydney'
                                                 ,'Curtin University'
                                                 ,'Murdoch University'
                                                 ,'Insearch'
                                                 ,'La Trobe University'
                                                 ,'University of Tasmania'
                                                 ,'William Angliss Institute'
                                                 ,'University of Otago'
                                                 ,'University of Newcastle'
                                                 ,'Queensland University of Technology'))

Dsynergygdw.client$geocode_client <- suppressMessages(geocode(Dsynergygdw.client$client_name))

# build enquiry 'origin'
QEnquiryOrigin <- paste("SELECT distinct Comm.client_id, Comm.crms_number, Stud.country_of_origin
FROM [synergygdw].[dbo].[crms_communication] Comm
INNER JOIN [synergygdw].[dbo].[crms_student] Stud
ON Comm.client_id = Stud.client_id AND Comm.crms_number = Stud.crms_number
WHERE Comm.client_id in (", "'HAU_AUCKUNOT','HAU_CURTTECH','HAU_INSEARCH','HAU_JCOOKUNI','HAU_MACQUNIV','HAU_UNIQULND','HAU_UNISYDNE','HAU_UNOWIKAT','HAU_UTSYDCRM','HAU_UWSMOFER','HCD40'",") AND country_of_origin IS NOT NULL", sep = '')

Dsynergygdw.EnquiryOrigin <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, QEnquiryOrigin))


# in the original post I had a data.frame with 500k rows of top origin destination pairs
trips <- data.frame(origin = c("San Francisco", "Sydney"),
                    destination = c("Paris", "Tokyo"),
                    stringsAsFactors = FALSE)

geocode("Australian National University")
# get lat and lon of cities
trips$geocode_origin <- suppressMessages(geocode(trips$origin))
trips$geocode_destination <- suppressMessages(geocode(trips$destination))


# get intermediate points between the two locations
arch <- gcIntermediate(trips$geocode_origin,
                       trips$geocode_destination,
                       n=100,
                       breakAtDateLine=TRUE,
                       addStartEnd=TRUE, sp=TRUE)

# http://docs.ggplot2.org/0.9.3.1/fortify.map.html
arch_fortified <- ldply(arch@lines, fortify)

# a few lines of ggplot2 code
ggplot() +
  geom_line(aes(long,lat,group=group), data=arch_fortified, alpha=0.1,size=1, colour="skyblue1") +
  coord_cartesian(ylim =c(-45, 70), xlim=c(-165, 165)) +
  theme_map() +
  geom_point(aes(lon, lat),data=trips$geocode_origin, alpha = 0.8, size = 1, colour = "white") +
  geom_point(aes(lon, lat),data=trips$geocode_destination, alpha = 0.8, size = 1, colour = "white")
