rm(list = ls()) # get rid ofany existing data
ls() # view open data sets

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
for (client in Lclient) {
  # create subfolder for client
  #client <- 'HAU_2UONLINE'
  print(paste('Commencing heatmap for client', client, timestamp()))
  dir.create(paste(getwd(), '/', client, sep = ''))


  #dir.create(paste(getwd(), '/', client,'/', year, sep = ''))
  Qcomm <- paste("SELECT *
                 FROM [synergygdw].[dbo].[crms_communication]
                 WHERE client_id = '", client,"'" , sep = '')
  #fname <- paste(paste('COMM', client, year, sep = '_'),'.csv', sep = '')
  DComm <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, Qcomm))
  if (nrow(DComm) > 0) {
    DComm %<>% arrange(crms_number) %>%
      mutate(ndate_opened = as.Date(date_opened, format = '%m/%d/%Y %H:%M'))
    # count comm per day
    Dcomm.raw <- DComm %>% group_by(ndate_opened) %>%
      summarise(daily_comm = n())
    Dcomm.raw %<>% mutate(Day = factor(weekdays(Dcomm.raw$ndate_opened,T),levels = rev(c("Mon", "Tue", "Wed", "Thu","Fri", "Sat", "Sun")))
                          ,Week = as.numeric(format(Dcomm.raw$ndate_opened,"%W"))
                          ,Month = as.numeric(format(Dcomm.raw$ndate_opened,"%m"))
                          ,Year = as.numeric(format(Dcomm.raw$ndate_opened,"%Y")))
    # generate overall heatmap for client
    print(paste('    Commencing heatmap overall for ', client, timestamp()))
    print(ggplot(Dcomm.raw, aes(x = Week, y = Day, fill = daily_comm)) +
            facet_wrap( ~ Year, ncol = 4) +
            scale_fill_viridis(name = "Number of Communication",option = "D", limits = c(0, max(Dcomm.raw$daily_comm))) +
            geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
            scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                               labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
            theme_tufte() + coord_equal() + ggtitle(paste("Heatmap of Weekly Number of Communication : ", client,sep = "")))
    # save plot
    #dev.copy(png, paste(getwd(), '/', client, '_heatmap_overall', '.png' ,sep = ''))
    dev.copy(png, paste(getwd(), '/', client, '/',client,'_heatmap_overall', '.png' ,sep = ''), width = 1280, height = 1024, res = 100)
    dev.off()
    print(paste('Completed heatmap for ', client, timestamp()))
    years <- unique(Dcomm.raw$Year)

    # generate yearly heatmap for client
    for (y in years) {
      Dy <- Dcomm.raw %>% filter(Year == y)
      print(paste('    Commencing heatmap for year', y, timestamp()))
      print(ggplot(Dy, aes(x = Week, y = Day, fill = daily_comm)) +
              scale_fill_viridis(name = "Number of Communication",option = "D", limits = c(0, max(Dy$daily_comm))) +
              geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
              scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                                 labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
              theme_tufte() + coord_equal() + ggtitle(paste("Heatmap of Weekly Number of Communication : ", client," ", y,sep = "")))
      dev.copy(png, paste(getwd(), '/', client, '/',client, '_heatmap_', y , '.png' ,sep = ''), width = 1280, height = 1024, res = 100)
      dev.off()
    }
    print(paste('    Completed heatmap for ', y, timestamp()))
  }

}
