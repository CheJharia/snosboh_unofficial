library(ggplot2)
library(dplyr) # data wrangling
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra)
library(tidyr)




######## Plotting starts here#####################
str(df)
str(DComm.raw.phone.hour)

ymin <- 2014
ymax <- 2016
DComm.raw.phone.hour.f <- DComm.raw.phone.hour %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(Hour >=8 & Hour <= 20, Day %in% c("Mon", "Tue", "Wed", "Thu","Fri"))
p <- ggplot(DComm.raw.phone.hour.f,aes(Day, Hour,fill = month_hourly_phone_comm))+
  geom_tile(color = "white",size=0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option ="C")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(DComm.raw.phone.hour.f$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title= paste("Hourly Phone Communication"), x = "Day", y = "Hour Commencing")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title=element_text(size = 14)) +
  theme(axis.text.y=element_text(size=6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme(strip.background = element_rect(colour="white")) +
  theme(plot.title=element_text(hjust=0)) +
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6))
p + removeGrid()
# you will want to expand your plot screen before this bit!
p #awesomeness
p + coord_equal()
#################################
