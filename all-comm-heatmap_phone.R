source('multiplot.R')


DComm.raw.phone <- DComm %>%
  filter(class == 'Phone') %>%
  group_by(ndate_opened) %>%
  summarise(daily_phone_comm = n())

DComm.raw.phone %<>% mutate(Day = factor(weekdays(DComm.raw.phone$ndate_opened,T),levels = rev(c("Mon", "Tue", "Wed", "Thu","Fri", "Sat", "Sun")))
                      ,Week = as.numeric(format(DComm.raw.phone$ndate_opened,"%W"))
                      ,Month = as.numeric(format(DComm.raw.phone$ndate_opened,"%m"))
                      ,Year = as.numeric(format(DComm.raw.phone$ndate_opened,"%Y")))

ggplot(DComm.raw.phone, aes(factor(Year), daily_phone_comm)) + geom_boxplot()
DComm.raw.phone %<>% mutate(daily_phone_comm_log = log(daily_phone_comm))
ggplot(DComm.raw.phone, aes(factor(Year), daily_phone_comm_log)) + geom_boxplot()

ymin <- 1999
ymax <- 2001
DComm.raw.phone.f <- DComm.raw.phone %>% filter( Year >= ymin & Year <= ymax)
p1 <- ggplot(DComm.raw.phone.f, aes(x = Week, y = Day, fill = daily_phone_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Phone Communication)",  option = "D", limits = c(0, max(DComm.raw.phone.f$daily_phone_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal()

ymin <- 2002
ymax <- 2004
DComm.raw.phone.f <- DComm.raw.phone %>% filter( Year >= ymin & Year <= ymax)
p2 <- ggplot(DComm.raw.phone.f, aes(x = Week, y = Day, fill = daily_phone_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Phone Communication)",  option = "D", limits = c(0, max(DComm.raw.phone.f$daily_phone_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal()

ymin <- 2005
ymax <- 2007
DComm.raw.phone.f <- DComm.raw.phone %>% filter( Year >= ymin & Year <= ymax)
p3 <- ggplot(DComm.raw.phone.f, aes(x = Week, y = Day, fill = daily_phone_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Phone Communication)",  option = "D", limits = c(0, max(DComm.raw.phone.f$daily_phone_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal()

ymin <- 2008
ymax <- 2010
DComm.raw.phone.f <- DComm.raw.phone %>% filter( Year >= ymin & Year <= ymax)
p4 <- ggplot(DComm.raw.phone.f, aes(x = Week, y = Day, fill = daily_phone_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Phone Communication)",  option = "D", limits = c(0, max(DComm.raw.phone.f$daily_phone_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal()

ymin <- 2011
ymax <- 2013
DComm.raw.phone.f <- DComm.raw.phone %>% filter( Year >= ymin & Year <= ymax)
p5 <- ggplot(DComm.raw.phone.f, aes(x = Week, y = Day, fill = daily_phone_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Phone Communication)",  option = "D", limits = c(0, max(DComm.raw.phone.f$daily_phone_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal()

ymin <- 2014
ymax <- 2016
DComm.raw.phone.f <- DComm.raw.phone %>% filter( Year >= ymin & Year <= ymax)
p6 <- ggplot(DComm.raw.phone.f, aes(x = Week, y = Day, fill = daily_phone_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Phone Communication)",  option = "D", limits = c(0, max(DComm.raw.phone.f$daily_phone_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal()
p1 + ggtitle("Hobsons APAC Weekly Phone Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
1,811,382 phone communication
3rd Aug 2000 - 29th Nov 2016
github.com/CheJharia')

p2 + ggtitle("Hobsons APAC Weekly Phone Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
1,811,382 phone communication
3rd Aug 2000 - 29th Nov 2016
github.com/CheJharia')

p3 + ggtitle("Hobsons APAC Weekly Phone Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
1,811,382 phone communication
3rd Aug 2000 - 29th Nov 2016
github.com/CheJharia')
p4 + ggtitle("Hobsons APAC Weekly Phone Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
1,811,382 phone communication
3rd Aug 2000 - 29th Nov 2016
github.com/CheJharia')
p5 + ggtitle("Hobsons APAC Weekly Phone Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
1,811,382 phone communication
3rd Aug 2000 - 29th Nov 2016
github.com/CheJharia')
p6 + ggtitle("Hobsons APAC Weekly Phone Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
1,811,382 phone communication
3rd Aug 2000 - 29th Nov 2016
github.com/CheJharia')
p1
p2
p3
p4
p5
p6
m1 <- multiplot(p1, p2, cols = 1)
m2 <- multiplot(p3, p4, cols = 1)
m3 <- multiplot(p5, p6, cols = 1)
multiplot(p1, p2, p3, p4, p5, p6, cols = 3)
