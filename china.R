# china phone inbound comm
p <- ggplot(DCommStudUser %>% filter(Year >= 2013 & Year <= 2016, in_out == 'IN', class == 'Phone', country_of_residence == 'China') %>%
              group_by(Year, Month, Day, Hour) %>%
              summarise(year_month_hour_all_comm = n()),
            aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Country of Residence == China Phone Inbound Communication",option = "D")
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


# china phone outbound comm
p <- ggplot(DCommStudUser %>% filter(Year >= 2013 & Year <= 2016, in_out == 'OUT', class == 'Phone', country_of_residence == 'China') %>%
              group_by(Year, Month, Day, Hour) %>%
              summarise(year_month_hour_all_comm = n()),
            aes(Day, Hour,fill = year_month_hour_all_comm)) +
  geom_tile(color = "white",size = 0.1) +
  scale_fill_viridis(name = "Number of Country of Residence == China Phone Outbound Communication",option = "D")
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
