
DCommStud.raw.phone.hour <- DCommStud %>%
  filter(class == 'Phone') %>%
  group_by(Year, Month,Day, Hour) %>%
  summarise(month_hourly_phone_comm = n())
str(DCommStud.raw.phone.hour)


ymin <- 2010
ymax <- 2016
DCommStud.raw.phone.hour.f <- DCommStud.raw.phone.hour %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(Hour >= 8 & Hour <= 20, Day %in% c("Mon", "Tue", "Wed", "Thu","Fri"))
p <- ggplot(DCommStud.raw.phone.hour.f,aes(Day, Hour,fill = month_hourly_phone_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "C")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(DCommStud.raw.phone.hour.f$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Phone Communication"), x = "Day", y = "Hour Commencing (24H)"
              ,subtitle = "Inbound and Outbound from 2010 to Nov 2016")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6))
p + removeGrid()
# you will want to expand your plot screen before this bit!
p #awesomeness
p + coord_equal()

DCommStud.raw.phone.hour_IN <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(Year, Month,Day, Hour) %>%
  summarise(month_hourly_phone_comm = n())
str(DCommStud.raw.phone.hour_IN)

ymin <- 2013
ymax <- 2016
DCommStud.raw.phone.hour_IN.f <- DCommStud.raw.phone.hour_IN %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(Hour >= 8 & Hour <= 20, Day %in% c("Mon", "Tue", "Wed", "Thu","Fri"))
p <- ggplot(DCommStud.raw.phone.hour_IN.f,aes(Day, Hour,fill = month_hourly_phone_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "C")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(DCommStud.raw.phone.hour_IN.f$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Phone Communication"), x = "Day", y = "Hour Commencing (24H)"
              ,subtitle = "Inbound from 2013 to Nov 2016")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6))
p + removeGrid()
# you will want to expand your plot screen before this bit!
p #awesomeness
p + coord_equal()



DCommStud.raw.phone.hour_OUT <- DCommStud %>%
  filter(class == 'Phone', in_out == 'OUT') %>%
  group_by(Year, Month,Day, Hour) %>%
  summarise(month_hourly_phone_comm = n())
str(DCommStud.raw.phone.hour_OUT)

ymin <- 2013
ymax <- 2016
DCommStud.raw.phone.hour_OUT.f <- DCommStud.raw.phone.hour_OUT %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(Hour >= 8 & Hour <= 20, Day %in% c("Mon", "Tue", "Wed", "Thu","Fri"))
p <- ggplot(DCommStud.raw.phone.hour_OUT.f,aes(Day, Hour,fill = month_hourly_phone_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "C")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(DCommStud.raw.phone.hour_OUT.f$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Phone Communication"), x = "Day", y = "Hour Commencing (24H)"
              ,subtitle = "Outbound from 2013 to Nov 2016")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6))
p + removeGrid()
# you will want to expand your plot screen before this bit!
p #awesomeness
p + coord_equal()


##############################################################################
# UNMC

client <- 'HAU_UNONTNGM'
DCommStud.raw.phone.hour_client <- DCommStud %>%
  filter(class == 'Phone', client_id == client) %>%
  group_by(Year, Month,Day, Hour) %>%
  summarise(month_hourly_phone_comm = n())
str(DCommStud.raw.phone.hour_client)


ymin <- 2013
ymax <- 2016
DCommStud.raw.phone.hour_client.f <- DCommStud.raw.phone.hour_client %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(Day %in% c("Mon", "Tue", "Wed", "Thu","Fri"))
p <- ggplot(DCommStud.raw.phone.hour_client.f,aes(Day, Hour,fill = month_hourly_phone_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(DCommStud.raw.phone.hour_client.f$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Phone Communication"), x = "Day", y = "Hour Commencing (24H)"
              ,subtitle = "Inbound and Outbound from 2013 to Nov 2016")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6))
p <- p + removeGrid()
p <- p + coord_equal()
p

DCommStud.raw.phone.hour_IN_client <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN', client_id == client) %>%
  group_by(Year, Month,Day, Hour) %>%
  summarise(month_hourly_phone_comm = n())
str(DCommStud.raw.phone.hour_IN_client)

ymin <- 2013
ymax <- 2016
DCommStud.raw.phone.hour_IN_client.f <- DCommStud.raw.phone.hour_IN_client %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(Day %in% c("Mon", "Tue", "Wed", "Thu","Fri"))
p <- ggplot(DCommStud.raw.phone.hour_IN_client.f,aes(Day, Hour,fill = month_hourly_phone_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(DCommStud.raw.phone.hour_IN_client.f$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Phone Communication ", client, sep = ""), x = "Day", y = "Hour Commencing (24H)"
              ,subtitle = "Inbound from 2013 to Nov 2016")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6))
p <- p + removeGrid()
p <- p + coord_equal()
p



DCommStud.raw.phone.hour_OUT_client <- DCommStud %>%
  filter(class == 'Phone', in_out == 'OUT', client_id == client) %>%
  group_by(Year, Month,Day, Hour) %>%
  summarise(month_hourly_phone_comm = n())
str(DCommStud.raw.phone.hour_OUT_client)

ymin <- 2013
ymax <- 2016
DCommStud.raw.phone.hour_OUT_client.f <- DCommStud.raw.phone.hour_OUT_client %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(Day %in% c("Mon", "Tue", "Wed", "Thu","Fri"))
p <- ggplot(DCommStud.raw.phone.hour_OUT_client.f,aes(Day, Hour,fill = month_hourly_phone_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(DCommStud.raw.phone.hour_OUT_client.f$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Phone Communication ", client, sep = ""), x = "Day", y = "Hour Commencing (24H)"
              ,subtitle = "Outbound from 2013 to Nov 2016")
p <- p + theme(legend.position = "bottom") +
  theme(plot.title = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
theme(strip.background = element_rect(colour = "white")) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 8)) +
  theme(legend.text = element_text(size = 6))
p <- p + removeGrid()
p <- p + coord_equal()
p

DCommStud %>% filter(client_id == client, in_out == 'IN') %>%
  group_by(country_of_residence) %>%
  summarise(n = n()) %>% arrange(desc(n))

DCommStud %>% filter(in_out == 'IN') %>%
  group_by(client_id) %>%
  summarise(n = n()) %>% arrange(desc(n))

DCommStud %>% filter(in_out == 'IN') %>%
  group_by(country_of_residence) %>%
  summarise(n = n()) %>% arrange(desc(n))

inbound_china_client_year <-DCommStud %>% filter(in_out == 'IN', country_of_residence == 'China') %>%
  group_by(client_id, Year) %>%
  summarise(n = n()) %>% arrange(desc(n))
