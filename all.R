# all
ymin <- 2010
ymax <- 2016


## all comm
p <- ggplot(DCommStudUser %>% filter(Year >= ymin & Year <= ymax) %>%
            group_by(Year, Month, Day, Hour) %>%
              summarise(year_month_hour_all_comm = n()),
            aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)")
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


## all phone user
p <- ggplot(DCommStudUser %>% filter(Year >= ymin & Year <= ymax, class == 'Phone', username == 'aida.annuar') %>%
              group_by(username, Year, Month, Day, Hour) %>%
              summarise(year_month_hour_all_comm = n()),
            aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)")
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


## all phone inbound
p <- ggplot(DCommStudUser %>% filter(Year >= ymin & Year <= ymax, class == 'Phone', in_out == 'IN') %>%
              group_by(Year, Month, Day, Hour) %>%
              summarise(year_month_hour_all_comm = n()),
            aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Inbound Phone Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)")
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


## all phone inbound - china
p <- ggplot(DCommStudUser %>% filter(Year >= 2013 & Year <= 2016, class == 'Phone', in_out == 'IN', country_of_residence == 'China') %>%
              group_by(Year, Month, Day, Hour) %>%
              summarise(year_month_hour_all_comm = n()),
            aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of China Inbound Phone Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)")
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


## all phone outbound - nottingham
p <- ggplot(DCommStudUser %>% filter(class == 'Phone', in_out == 'OUT', client_id == 'HAU_UNONTNGM') %>%
              group_by(Year, Month, Day, Hour) %>%
              summarise(year_month_hour_all_comm = n()),
            aes(Day, Hour,fill = log(year_month_hour_all_comm))) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "log(Number of Outbound Email Communication HAU_UNONTNGM)",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)")
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




# USER

DCommStudUser_user_all_comm <- DCommStudUser %>%
  group_by(username, Year, Month, Day, Hour) %>%
  summarise(year_month_hour_all_comm = n())

ymin <- 2014
ymax <- 2016
df <- DCommStudUser_user_all_comm %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(username == 'aida.annuar')
p <- ggplot(df,aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)"
,subtitle = "aida.annuar
All Communication")
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


DCommStudUser_user_phone_comm <- DCommStudUser %>%
  filter(class == 'Phone') %>%
  group_by(username, Year, Month, Day, Hour) %>%
  summarise(year_month_hour_all_comm = n())

ymin <- 2014
ymax <- 2016
df <- DCommStudUser_user_phone_comm %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(username == 'aida.annuar')
p <- ggplot(df,aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Phone Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)"
              ,subtitle = "aida.annuar
              Phone Communication")
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

DCommStudUser_user_email_comm <- DCommStudUser %>%
  filter(class == 'Email') %>%
  group_by(username, Year, Month, Day, Hour) %>%
  summarise(year_month_hour_all_comm = n())

ymin <- 2014
ymax <- 2016
df <- DCommStudUser_user_email_comm %>% filter( Year >= ymin & Year <= ymax) %>%
  filter(username == 'aida.annuar')
p <- ggplot(df,aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Email Communication",option = "D")
p <- p + facet_grid(Year ~ Month)
p <- p + scale_y_continuous(trans = "reverse", breaks = unique(df$Hour))
#p <-p + scale_x_discrete(breaks =c(1,10,20,31))
p <- p + theme_minimal(base_size = 8)
p <- p + labs(title = paste("Hourly Communication"), x = "Day", y = "Hour Commencing (24H)"
,subtitle = "aida.annuar
Email Communication")
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
