source('multiplot.R')
ymin <- 1999
ymax <- 2001
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p1 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal() + ggtitle("Hobsons APAC Weekly Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
37,102,359 communication
10th Aug 1999 - 29th Nov 2016
github.com/CheJharia')

ymin <- 2002
ymax <- 2004
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p2 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal() + ggtitle("                                               ", subtitle = '


                     ')

ymin <- 2005
ymax <- 2007
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p3 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal() + ggtitle("Hobsons APAC Weekly Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
                                          37,102,359 communication
                                          10th Aug 1999 - 29th Nov 2016
                                          github.com/CheJharia')

ymin <- 2008
ymax <- 2010
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p4 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal() + ggtitle("                                               ", subtitle = '


                                          ')

ymin <- 2011
ymax <- 2013
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p5 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal() + ggtitle("Hobsons APAC Weekly Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
                                          37,102,359 communication
                                          10th Aug 1999 - 29th Nov 2016
                                          github.com/CheJharia')

ymin <- 2014
ymax <- 2016
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p6 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
  facet_wrap( ~ Year, shrink = T) +
  scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
  geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_tufte() + coord_equal() + ggtitle("                                               ", subtitle = '


                                          ')

multiplot(p1, p2, cols = 2)
multiplot(p3, p4, cols = 2)
multiplot(p5, p6, cols = 2)
multiplot(p1, p2, p3, p4, p5, p6, cols = 3)
