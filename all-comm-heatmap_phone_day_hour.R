source('multiplot.R')

DComm %<>% mutate(ndate_opened = as.Date(date_opened, format = '%m/%d/%Y %H:%M')
                 ,Hour = as.numeric(format(date_opened, "%H"))
                 ,Day = factor(weekdays(ndate_opened,T),levels = c("Mon", "Tue", "Wed", "Thu","Fri", "Sat", "Sun"))
                 ,Week = as.numeric(format(ndate_opened,"%W"))
                 ,Month = factor(month.abb[as.numeric(format(ndate_opened,"%m"))], levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
                 ,Year = as.numeric(format(ndate_opened,"%Y")))

DComm.raw.phone.hour <- DComm %>%
  filter(class == 'Phone') %>%
  group_by(Year, Month,Day, Hour) %>%
  summarise(month_hourly_phone_comm = n())
str(DComm)

ggplot(DComm.raw.phone.hour, aes(factor(Month), month_hourly_phone_comm)) + geom_boxplot()
DComm.raw.phone.hour %<>% mutate(month_hourly_phone_comm_log = log(month_hourly_phone_comm))
ggplot(DComm.raw.phone.hour, aes(factor(Month), month_hourly_phone_comm_log)) + geom_boxplot()

ymin <- 2015
ymax <- 2015
DComm.raw.phone.hour.f <- DComm.raw.phone.hour %>% filter( Year >= ymin & Year <= ymax)
ggplot(DComm.raw.phone.hour.f, aes(x = Hour, y = Day, fill = month_hourly_phone_comm)) +
  facet_wrap( ~ Year + Month) +
  scale_fill_viridis(name = "Number of Phone Communication",  option = "D", limits = c(0, max(DComm.raw.phone.hour.f$month_hourly_phone_comm))) +
  geom_tile(color = "white", size = 0.4) +
  #scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 7),
  #                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
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
