Dcomm.raw %>% mutate(log_)
colnames(Dcomm.raw)
ggplot(Dcomm.raw, aes(factor(Year), daily_comm)) + geom_boxplot()
ggplot(Dcomm.raw, aes(factor(Year), log(daily_comm))) + geom_boxplot()
ggplot(Dcomm.raw, aes(factor(Year), log(daily_comm, 2))) + geom_boxplot()
Dcomm.raw %<>% mutate(daily_comm_log = log(daily_comm))
Dcomm.raw %<>% mutate(daily_comm_log2 = log(daily_comm, 2))
Dcomm.raw %>% group_by(Year) %>% summarise(m = mean(daily_comm_log))

ggplot(Dcomm.raw, aes(x = Week, y = Day, fill = daily_comm_log)) +
          facet_wrap( ~ Year, ncol = 4) +
          scale_fill_viridis(name = "Number of Communication",option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
          geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
          scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                             labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + theme_tufte() + coord_equal() + ggtitle("Heatmap of Weekly Number of Phone Communication : ")

ggplot(Dcomm.raw, aes(x = Week, y = Day, fill = daily_comm_log)) +
        facet_wrap( ~ Year, ncol = 2) +
        scale_fill_viridis(name = "Number of Communication",option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
        geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + theme_tufte() +  ggtitle("Heatmap of Weekly Number of Phone Communication : ")

``)
ymin <- 1999
ymax <- 2002
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

ymin <- 2003
ymax <- 2006
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p2 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
        facet_wrap( ~ Year, shrink = T) +
        scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
        geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
        theme_tufte() + coord_equal() + ggtitle("Hobsons APAC Weekly Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
37,102,359 communication
10th Aug 1999 - 29th Nov 2016
github.com/CheJharia')

ymin <- 2007
ymax <- 2010
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

ymin <- 2011
ymax <- 2014
Dcomm.raw.f <- Dcomm.raw %>% filter( Year >= ymin & Year <= ymax)
p4 <- ggplot(Dcomm.raw.f, aes(x = Week, y = Day, fill = daily_comm_log)) +
        facet_wrap( ~ Year, shrink = T) +
        scale_fill_viridis(name = "Log(Number of Communication)",  option = "D", limits = c(0, max(Dcomm.raw$daily_comm_log))) +
        geom_tile(color = "white", size = 0.4) + facet_wrap("Year", ncol = 1) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(1, 52, length = 12),
                           labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
        theme_tufte() + coord_equal() + ggtitle("Hobsons APAC Weekly Communication : GoLive YTD ", subtitle = 'Hobsons CRM Communication Extract
37,102,359 communication
10th Aug 1999 - 29th Nov 2016
github.com/CheJharia')

ymin <- 2015
ymax <- 2016
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

multiplot(p1, p2, p3, p4, p5, cols =3)
