library(dplyr)
library(ggplot2)
library(reshape2)
#http://analyzecore.com/2015/06/23/sales-funnel-visualization-with-r/
# creating a data samples
# content
df.content <- data.frame(content = c('main', 'ad landing',
                                     'product 1', 'product 2', 'product 3', 'product 4',
                                     'shopping cart',
                                     'thank you page'),
                         step = c('awareness', 'awareness',
                                  'interest', 'interest', 'interest', 'interest',
                                  'desire',
                                  'action'),
                         number = c(150000, 80000,
                                    80000, 40000, 35000, 25000,
                                    130000,
                                    120000))
# customers
df.customers <- data.frame(content = c('new', 'engaged', 'loyal'),
                           step = c('new', 'engaged', 'loyal'),
                           number = c(25000, 40000, 55000))
# combining two data sets
df.all <- rbind(df.content, df.customers)

# calculating dummies, max and min values of X for plotting
df.all <- df.all %>%
  group_by(step) %>%
  mutate(totnum = sum(number)) %>%
  ungroup() %>%
  mutate(dum = (max(totnum) - totnum)/2,
         maxx = totnum + dum,
         minx = dum)

# data frame for plotting funnel lines
df.lines <- df.all %>%
  select(step, maxx, minx) %>%
  group_by(step) %>%
  unique()

# data frame with dummies
df.dum <- df.all %>%
  select(step, dum) %>%
  unique() %>%
  mutate(content = 'dummy',
         number = dum) %>%
  select(content, step, number)

# data frame with rates
conv <- df.all$totnum[df.all$step == 'action']

df.rates <- df.all %>%
  select(step, totnum) %>%
  group_by(step) %>%
  unique() %>%
  ungroup() %>%
  mutate(prevnum = lag(totnum),
         rate = ifelse(step == 'new' | step == 'engaged' | step == 'loyal',
                       round(totnum / conv, 3),
                       round(totnum / prevnum, 3))) %>%
  select(step, rate)
df.rates <- na.omit(df.rates)

# creting final data frame
df.all <- df.all %>%
  select(content, step, number)

df.all <- rbind(df.all, df.dum)

df.all <- df.all %>%
  group_by(step) %>%
  arrange(desc(content)) %>%
  ungroup()

# calculating position of labels
df.all <- df.all %>%
  group_by(step) %>%
  mutate(pos = cumsum(number) - 0.5*number)

# defining order of steps
df.all$step <- factor(df.all$step, levels = c('loyal', 'engaged', 'new', 'action', 'desire', 'interest', 'awareness'))
list <- c(unique(as.character(df.all$content)))
df.all$content <- factor(df.all$content, levels = c('dummy', c(list)))

# creating custom palette with 'white' color for dummies
cols <- c("#ffffff", "#fec44f", "#fc9272", "#a1d99b", "#fee0d2", "#2ca25f",
          "#8856a7", "#43a2ca", "#fdbb84", "#e34a33",
          "#a6bddb", "#dd1c77", "#ffeda0", "#756bb1")

# plotting chart
ggplot() +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values=cols) +
  geom_bar(data=df.all, aes(x=step, y=number, fill=content), stat="identity", width=1) +
  geom_text(data=df.all[df.all$content!='dummy', ],
            aes(x=step, y=pos, label=paste0(content, '-', number/1000, 'K')),
            size=4, color='white', fontface="bold") +
  geom_ribbon(data=df.lines, aes(x=step, ymax=max(maxx), ymin=maxx, group=1), fill='white') +
  geom_line(data=df.lines, aes(x=step, y=maxx, group=1), color='darkred', size=4) +
  geom_ribbon(data=df.lines, aes(x=step, ymax=minx, ymin=min(minx), group=1), fill='white') +
  geom_line(data=df.lines, aes(x=step, y=minx, group=1), color='darkred', size=4) +
  geom_text(data=df.rates, aes(x=step, y=(df.lines$minx[-1]), label=paste0(rate*100, '%')), hjust=1.2,
            color='darkblue', fontface="bold") +
  theme(legend.position='none', axis.ticks=element_blank(), axis.text.x=element_blank(),
        axis.title.x=element_blank())
