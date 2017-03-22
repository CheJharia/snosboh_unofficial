
# score data which also include blank cells
fn <- "2 output ProfileData-2016-03-17 15_36_30-all-scored-total-scores.csv"
# score data with only complete cases i.e no blank cells
# fn <- "2 output ProfileData-2016-03-17 15_36_30-complete-scored-total-scores.csv"

library(dplyr)
scores <- tbl_df(read.csv(file = fn, header = T, check.names = F))
str(scores)
summary(scores)

# create another column 'division'
scores %<>%
  mutate(division = as.factor(substr(scores$`Global Response ID`, start = 1, stop = 4 ))) %>%
  select(everything())
scores

library(ggplot2)
library(ggthemes)
data <- scores
data <- filter(scores, division == 'APAC')
data <- filter(scores, division == 'EMEA')
ggplot(data , aes(x = `CL Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = division, fill = division),
               geom = "ribbon", position = "identity") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Cost of Living Score distribution") + facet_grid(. ~ division, margins = T) + theme_tufte(base_family="Helvetica")

ggplot(data , aes(x = `EM Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = division, fill = division),
               geom = "ribbon", position = "identity") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Employment Score distribution")+ facet_grid(. ~ division) 

ggplot(data , aes(x = `LE Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = division, fill = division),
               geom = "ribbon", position = "identity") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Lifestyle Score distribution")+ facet_grid(. ~ division) 

ggplot(data , aes(x = `TQ Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = division, fill = division),
               geom = "ribbon", position = "identity") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("Teaching Quality Score distribution")+ facet_grid(. ~ division) 

ggplot(data , aes(x = `UB Score`)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density.., colour = division, fill = division),
               geom = "ribbon", position = "identity") +
  coord_flip() +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.line.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.position="bottom",
        legend.title=element_blank()) + ggtitle("University Brand Score distribution")+ facet_grid(. ~ division) 
