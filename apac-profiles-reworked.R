library(dplyr)
library(ggplot2)
library(plotly)
data.apac_labelled <- tbl_df(read.csv(file = 'apac_data_5_profiles.csv',header = T, check.names = F))
str(data.apac_labelled)
summary(data.apac_labelled)
data.apac_labelled$profile <- as.factor(data.apac_labelled$profile)
# Rename all levels
levels(data.apac_labelled$profile) <- c("Profile-A","Profile-B",
                                        "Profile-C", "Profile-D",
                                        "Profile-E")

data.apac_labelled <- data.apac_labelled %>%
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


spider_data_CL <- data.apac_labelled %>%
  select(`CL Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`CL Rating`)) %>%
  mutate(theme = 'CL')
spider_data_CL %<>%
  mutate(CL = median) %>%
  select(profile, CL)

spider_data_EM <- data.apac_labelled %>%
  select(`EM Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`EM Rating`)) %>%
  mutate(theme = 'EM')

spider_data_EM %<>%
  mutate(EM = median) %>%
  select(EM)

spider_data_LE <- data.apac_labelled %>%
  select(`LE Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`LE Rating`)) %>%
  mutate(theme = 'LE')

spider_data_LE %<>%
  mutate(LE = median) %>%
  select(LE)

spider_data_TQ <- data.apac_labelled %>%
  select(`TQ Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`TQ Rating`)) %>%
  mutate(theme = 'TQ')

spider_data_TQ %<>%
  mutate(TQ = median) %>%
  select(TQ)

spider_data_UB <- data.apac_labelled %>%
  select(`UB Rating`, profile) %>%
  group_by(profile) %>%
  summarise(median = median(`UB Rating`)) %>%
  mutate(theme = 'UB')

spider_data_UB %<>%
  mutate(UB = median) %>%
  select(UB)


spider_data <- cbind(spider_data_CL,
                     spider_data_EM,
                     spider_data_TQ,
                     spider_data_LE,
                     spider_data_UB)

library(ggradar)

library(scales)

spider_data$profile <- as.character(spider_data$profile)
colnames(spider_data)[2:6] <- c("Cost of Living", "Employment","Teaching Quality", "Lifestyle" 
                                , "University Brand")
spider_data$profile <- c("Future Focused","Balance is Imperative","Life Planner","Prestige Matters","The Whole Package")
ggradar(cbind(spider_data$profile,spider_data[,2:6]/3), axis.label.size = 3) + ggtitle("Radar Chart - Profile comparisons") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[1,], axis.label.size = 3) + ggtitle("Radar Chart - Future Focused") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[2,], axis.label.size = 3) + ggtitle("Radar Chart - Balance is Imperative") + scale_colour_manual(values = "#FFB400") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[3,], axis.label.size = 3) + ggtitle("Radar Chart - Life Planner") + scale_colour_manual(values = "#007A87") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[4,], axis.label.size = 3) + ggtitle("Radar Chart - Prestige Matters")+ scale_colour_manual(values = "#8CE071") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3)[5,], axis.label.size = 3) + ggtitle("Radar Chart - The Whole Package")+ scale_colour_manual(values = "#7B0051") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 


write.csv(spider_data, file = 'spider_data_apac_median.csv', row.names = F)
