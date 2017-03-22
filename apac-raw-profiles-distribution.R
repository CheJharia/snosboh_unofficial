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
droplevels(data.apac_labelled)
str(data.apac_labelled)
ggplot(data.apac_labelled
       ) +
  aes(x = `CL Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Cost of Living distribution for each profile')

ggplot(data.apac_labelled
) +
  aes(x = `CL Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  ggtitle('Cost of Living distribution for each profile')

ggplot(data.apac_labelled
) +
  aes(x = `EM Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Employment distribution by profile')

ggplot(data.apac_labelled
) +
  aes(x = `EM Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  ggtitle('Employment distribution by profile')

ggplot(data.apac_labelled
) +
  aes(x = `LE Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Lifestyle distribution by profile')

ggplot(data.apac_labelled
) +
  aes(x = `LE Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  ggtitle('Lifestyle distribution by profile')

ggplot(data.apac_labelled
) +
  aes(x = `TQ Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Teaching Quality distribution by profile')

ggplot(data.apac_labelled
) +
  aes(x = `TQ Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  ggtitle('Teaching Quality distribution by profile')

ggplot(data.apac_labelled
) +
  aes(x = `UB Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('University Brand distribution by profile')

ggplot(data.apac_labelled
) +
  aes(x = `UB Score`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1) + theme_classic() +
  ggtitle('University Brand distribution for each profile')


p <- ggplot(data %>%
              filter(prospect_age > 10, prospect_age < 50, gender == "F" | gender == "M", region == "Africa")) +
  aes(x = prospect_age, colour = gender, fill = gender) +
  facet_grid(~ CreateYear) +
  scale_x_continuous(breaks = round(seq(min(data$prospect_age), max(data$prospect_age), by = 10),1)) +
  geom_density(alpha=.25, size = 0.1) + theme_classic()+
  ggtitle(paste("Africa" , ' Total population = ', toString(data %>% filter(prospect_age > 10, prospect_age < 50, gender == "F" | gender == "M", region == "Africa") %>% summarise(n=n()))))
ggplotly(p)
