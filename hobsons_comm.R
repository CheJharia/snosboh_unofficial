count(DCommStudUser, client_id, Year) %>%
  mutate(percent = percent(n/sum(n)), count = comma(n)) %>%
  arrange(desc(n)) -> comm_by_client_by_year


count(DCommStudUser, country_of_residence) %>%
  mutate(percent = percent(n/sum(n)), count = comma(n)) %>%
  mutate(country = sprintf("%s (%s)",country_of_residence, countrycode(country_of_residence, "country.name", "iso2c"))) %>% arrange(desc(n)) -> comm_by_country_of_residence

count(DCommStudUser, country_of_origin) %>%
  mutate(percent = percent(n/sum(n)), count = comma(n)) %>%
  mutate(country = sprintf("%s (%s)",country_of_origin, countrycode(country_of_origin, "country.name", "iso2c"))) %>% arrange(desc(n)) -> comm_by_country_of_origin

count(DCommStudUser, nationality) %>%
  mutate(percent = percent(n/sum(n)), count = comma(n)) %>%
  mutate(country = sprintf("%s (%s)",nationality, countrycode(nationality, "country.name", "iso2c"))) %>% arrange(desc(n)) -> comm_by_nationality


# China
DCommStudUser %>% filter(class == 'Phone', in_out == 'IN') -> DCommStudUserPhoneIN

count(DCommStudUserPhoneIN, country_of_residence) %>%
  mutate(percent = percent(n/sum(n)), count = comma(n)) %>%
  arrange(desc(n)) -> DCommStudUserPhoneIN_by_country_of_residence


DCommStudUser %>%
  count(country_of_residence, Day, Hour) %>%
  ungroup() %>%
  mutate(country_of_residence = factor(country_of_residence,
                          levels = comm_by_country_of_residence$country_of_residence)) %>%
  filter(country_of_residence %in% comm_by_country_of_residence$country_of_residence[c(1,3:11)]) -> DTopPhoneIN_by_country_of_residence


gg <- ggplot(DTopPhoneIN_by_country_of_residence, aes(x = Hour, y = Day, fill = log(n)))
gg <- gg + geom_tile(color = "white", size = 0.1)
gg <- gg + scale_fill_viridis(name = "log(# Inbound Phone Class)", option = "D")
gg <- gg + coord_equal()
gg <- gg + scale_x_continuous(breaks = unique(DTopPhoneIN_byclient$Hour))
gg <- gg + facet_wrap(~ country_of_residence, ncol = 2)
gg <- gg + labs(x = NULL, y = NULL)
gg <- gg + ggtitle(label =  paste("Inbound Phone per weekday & time of day by country (Top 10 countries with The Most Inbound Calls)"), subtitle = '@author github.com/CheJharia
@date 6th December 2016
@dataset GoLive to 29 Nov 2016 [synergygdw].[dbo].[crms_communication]')
gg <- gg + theme_tufte()
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_text(size = 8))
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(plot.title = element_text(hjust = 0))
gg <- gg + theme(strip.text = element_text(hjust = 0))
gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
gg <- gg + theme(panel.spacing.y = unit(0.5, "cm"))
gg <- gg + theme(legend.title = element_text(size = 8))
gg <- gg + theme(legend.title.align=1)
gg <- gg + theme(legend.text = element_text(size = 8))
gg <- gg + theme(legend.position = "bottom")
gg <- gg + theme(legend.key.size = unit(0.2, "cm"))
gg <- gg + theme(legend.key.width = unit(1, "cm"))
gg


gg <- ggplot(DTopPhoneIN_by_country_of_residence, aes(x = Hour, y = Day, fill = n))
gg <- gg + geom_tile(color = "white", size = 0.1)
gg <- gg + scale_fill_viridis(name = "# Inbound Phone Class", option = "D")
gg <- gg + coord_equal()
gg <- gg + scale_x_continuous(breaks = unique(DTopPhoneIN_byclient$Hour))
gg <- gg + facet_wrap(~ country_of_residence, ncol = 2)
gg <- gg + labs(x = NULL, y = NULL)
gg <- gg + ggtitle(label =  paste("Inbound Phone per weekday & time of day by country (Top 10 countries with The Most Inbound Calls)"), subtitle = '@author github.com/CheJharia
@date 6th December 2016
@dataset GoLive to 29 Nov 2016 [synergygdw].[dbo].[crms_communication]')
gg <- gg + theme_tufte()
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_text(size = 8))
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(plot.title = element_text(hjust = 0))
gg <- gg + theme(strip.text = element_text(hjust = 0))
gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
gg <- gg + theme(panel.spacing.y = unit(0.5, "cm"))
gg <- gg + theme(legend.title = element_text(size = 8))
gg <- gg + theme(legend.title.align=1)
gg <- gg + theme(legend.text = element_text(size = 8))
gg <- gg + theme(legend.position = "bottom")
gg <- gg + theme(legend.key.size = unit(0.2, "cm"))
gg <- gg + theme(legend.key.width = unit(1, "cm"))
gg



lapply(comm_by_country_of_residence$country_of_residence[c(1,3:11)], function(cc) {
  gg <- ggplot(filter(DTopPhoneIN_by_country_of_residence, country_of_residence == cc),
               aes(x = Hour, y = Day, fill = n, frame = country_of_residence))
  gg <- gg + geom_tile(color = "white", size = 0.1)
  gg <- gg + scale_x_discrete(expand = c(0,0))
  gg <- gg + scale_y_discrete(expand = c(0,0))
  gg <- gg + scale_fill_viridis(name = "")
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL,
                  title=sprintf("%s", cc))
  gg <- gg + theme_tufte()
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_text(size=10))
  gg <- gg + theme(panel.border =element_blank())
  gg <- gg + theme(plot.title =element_text(hjust=0, size=10))
  gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
  gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
  gg <- gg + theme(legend.title=element_text(size=10))
  gg <- gg + theme(legend.title.align=1)
  gg <- gg + theme(legend.text=element_text(size=8))
  gg <- gg + theme(legend.position="bottom")
  gg <- gg + theme(legend.key.size=unit(0.2, "cm"))
  gg <- gg + theme(legend.key.width=unit(1, "cm"))

  gg
}) -> cclist

cclist[["ncol"]] <- 2

do.call(grid.arrange, cclist)


lapply(comm_by_country_of_residence$country_of_residence[c(1,3:11)], function(cc) {
  gg <- ggplot(filter(DTopPhoneIN_by_country_of_residence, country_of_residence == cc),
               aes(x = Hour, y = Day, fill = n, frame = country_of_residence))
  gg <- gg + geom_tile(color = "white", size = 0.1)
  gg <- gg + scale_fill_viridis(name = "", option = "D")
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL,
                  title = sprintf("%s", cc))
  gg <- gg + scale_x_continuous(breaks = unique(DTopPhoneIN_byclient$Hour))
  gg <- gg + theme_tufte()
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_text(size = 8))
  gg <- gg + theme(panel.border = element_blank())
  gg <- gg + theme(plot.title = element_text(hjust=0, size=10))
  gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
  gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
  gg <- gg + theme(legend.title=element_text(size=10))
  gg <- gg + theme(legend.title.align=1)
  gg <- gg + theme(legend.text=element_text(size=8))
  gg <- gg + theme(legend.position="bottom")
  gg <- gg + theme(legend.key.size=unit(0.2, "cm"))
  gg <- gg + theme(legend.key.width=unit(1, "cm"))

  gg
}) -> cclist

cclist[["ncol"]] <- 2

do.call(grid.arrange, cclist)


lapply(comm_by_country_of_residence$country_of_residence[c(1,3:11)], function(cc) {
  gg <- ggplot(filter(DTopPhoneIN_by_country_of_residence, country_of_residence == cc),
               aes(x = Hour, y = Day, fill = log(n), frame = country_of_residence))
  gg <- gg + geom_tile(color = "white", size = 0.1)
  gg <- gg + scale_fill_viridis(name = "", option = "D")
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL,
                  title = sprintf("%s", cc))
  gg <- gg + scale_x_continuous(breaks = unique(DTopPhoneIN_byclient$Hour))
  gg <- gg + theme_tufte()
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_text(size = 8))
  gg <- gg + theme(panel.border = element_blank())
  gg <- gg + theme(plot.title = element_text(hjust=0, size=10))
  gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
  gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
  gg <- gg + theme(legend.title=element_text(size=10))
  gg <- gg + theme(legend.title.align=1)
  gg <- gg + theme(legend.text=element_text(size=8))
  gg <- gg + theme(legend.position="bottom")
  gg <- gg + theme(legend.key.size=unit(0.2, "cm"))
  gg <- gg + theme(legend.key.width=unit(1, "cm"))

  gg
}) -> cclist

cclist[["ncol"]] <- 2

do.call(grid.arrange, cclist)

