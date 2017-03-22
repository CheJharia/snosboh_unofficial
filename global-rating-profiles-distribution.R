library(cluster)
library(factoextra)
library(dplyr)

#######################################################
# Rating scores
#######################################################

data.global_labelled <- tbl_df(read.csv(file = 'global_data_5_profiles.csv',header = T, check.names = F))
colnames(data.global_labelled)[9] <- "profile"
colnames(data.global_labelled)
data.global_labelled$profile <- as.factor(data.global_labelled$profile)
str(data.global_labelled)
# Rename all levels
levels(data.global_labelled$profile) <- c("Profile-A","Profile-B",
                                          "Profile-C", "Profile-D",
                                          "Profile-E")
droplevels(data.global_labelled)

data.global_labelled <- data.global_labelled %>%
  mutate(`CL percentage rank` = percent_rank(`CL Score`),
         `EM percentage rank` = percent_rank(`EM Score`),
         `LE percentage rank` = percent_rank(`LE Score`),
         `TQ percentage rank` = percent_rank(`TQ Score`),
         `UB percentage rank` = percent_rank(`UB Score`),
         `CL Rating` = ntile(`CL percentage rank`,3),
         `EM Rating` = ntile(`EM percentage rank`,3),
         `LE Rating` = ntile(`LE percentage rank`,3),
         `TQ Rating` = ntile(`TQ percentage rank`,3),
         `UB Rating` = ntile(`UB percentage rank`,3))


###########################################################################
# Cost of Living
###########################################################################

ggplot(data.global_labelled
) +
  aes(x = `CL Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Cost of Living distribution by profile Global')

spider_data_CL <- data.global_labelled %>%
  select(`CL Rating`, profile) %>%
  group_by(profile) %>%
  summarise(mean = mean(`CL Rating`)) %>%
  mutate(theme = 'CL')
spider_data_CL %<>%
  mutate(CL = mean) %>%
  select(profile, CL)

ggplot(data.global_labelled %>% filter(division == 'APAC')
) +
  aes(x = `CL Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Cost of Living distribution by profile APAC')



ggplot(data.global_labelled %>% filter(division == 'EMEA')
) +
  aes(x = `CL Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Cost of Living distribution by profile EMEA')

###########################################################################
# Employment
###########################################################################
ggplot(data.global_labelled
) +
  aes(x = `EM Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Employment distribution by profile Global')

spider_data_EM <- data.global_labelled %>%
  select(`EM Rating`, profile) %>%
  group_by(profile) %>%
  summarise(mean = mean(`EM Rating`)) %>%
  mutate(theme = 'EM')

spider_data_EM %<>%
  mutate(EM = mean) %>%
  select(EM)

ggplot(data.global_labelled %>% filter(division == 'APAC')
) +
  aes(x = `EM Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Employment distribution by profile APAC')

ggplot(data.global_labelled %>% filter(division == 'EMEA')
) +
  aes(x = `EM Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Employment distribution by profile EMEA')

###########################################################################
# Lifestyle
###########################################################################
ggplot(data.global_labelled
) +
  aes(x = `LE Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Lifestyle distribution by profile Global')

spider_data_LE <- data.global_labelled %>%
  select(`LE Rating`, profile) %>%
  group_by(profile) %>%
  summarise(mean = mean(`LE Rating`)) %>%
  mutate(theme = 'LE')

spider_data_LE %<>%
  mutate(LE = mean) %>%
  select(LE)

ggplot(data.global_labelled %>% filter(division == 'APAC')
) +
  aes(x = `LE Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Lifestyle distribution by profile APAC')

ggplot(data.global_labelled %>% filter(division == 'EMEA')
) +
  aes(x = `LE Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Lifestyle distribution by profile EMEA')


###########################################################################
# Teaching Quality
###########################################################################
ggplot(data.global_labelled
) +
  aes(x = `TQ Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Teaching Quality distribution by profile Global')

spider_data_TQ <- data.global_labelled %>%
  select(`TQ Rating`, profile) %>%
  group_by(profile) %>%
  summarise(mean = mean(`TQ Rating`)) %>%
  mutate(theme = 'TQ')

spider_data_TQ %<>%
  mutate(TQ = mean) %>%
  select(TQ)

ggplot(data.global_labelled %>% filter(division == 'APAC')
) +
  aes(x = `TQ Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Teaching Quality distribution by profile APAC')

ggplot(data.global_labelled %>% filter(division == 'EMEA')
) +
  aes(x = `TQ Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Teaching Quality distribution by profile EMEA')



###########################################################################
# University Brand
###########################################################################
ggplot(data.global_labelled
) +
  aes(x = `UB Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('University Brand distribution by profile Global')

spider_data_UB <- data.global_labelled %>%
  select(`UB Rating`, profile) %>%
  group_by(profile) %>%
  summarise(mean = mean(`UB Rating`)) %>%
  mutate(theme = 'UB')

spider_data_UB %<>%
  mutate(UB = mean) %>%
  select(UB)

ggplot(data.global_labelled %>% filter(division == 'APAC')
) +
  aes(x = `UB Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('University Brand distribution by profile APAC')

ggplot(data.global_labelled %>% filter(division == 'EMEA')
) +
  aes(x = `UB Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('University Brand distribution by profile EMEA')


spider_data <- cbind(spider_data_CL,
                     spider_data_EM,
                     spider_data_LE,
                     spider_data_TQ,
                     spider_data_UB)



#write.csv(spider_data, file = 'spider_data_global.csv', row.names = F)


##############################################################################
# Basic stats

data.global_labelled %>%
  group_by(profile) %>%
  summarise(count = n())
round(prop.table(table(data.global_labelled$profile))*100,2)

##############################################################################
# Spider Chart

library(ggradar)

library(scales)

spider_data$profile <- as.character(spider_data$profile)
colnames(spider_data)[2:6] <- c("Cost of Living", "Employment", "Lifestyle", 
                                "Teaching Quality", "University Brand")
# All in 1
ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)) + ggtitle("Radar Chart - Profile comparisons")

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[1,]) + ggtitle("Radar Chart - Profile A")

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[2,]) + ggtitle("Radar Chart - Profile B")

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[3,]) + ggtitle("Radar Chart - Profile C")

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[4,]) + ggtitle("Radar Chart - Profile D")

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[5,]) + ggtitle("Radar Chart - Profile E")
