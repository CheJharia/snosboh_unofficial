
res.hc <- eclust(cluster_data, "hclust", k = 5) 
library(cluster)
library(factoextra)
library(dplyr)
?eclust

#######################################################
# Rating scores
#######################################################

data.apac_labelled <- tbl_df(read.csv(file = 'apac_data_5_profiles.csv',header = T, check.names = F))

data.apac_labelled$profile <- as.factor(data.apac_labelled$profile)
# Rename all levels
levels(data.apac_labelled$profile) <- c("Profile-A","Profile-B",
                                        "Profile-C", "Profile-D",
                                        "Profile-E")
droplevels(data.apac_labelled)

data.apac_labelled_augmented <- data.apac_labelled %>%
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


ggplot(data.apac_labelled_augmented
) +
  aes(x = `CL Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Cost of Living distribution by profile')


ggplot(data.apac_labelled_augmented
) +
  aes(x = `EM Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Employment distribution by profile')

ggplot(data.apac_labelled_augmented
) +
  aes(x = `LE Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Lifestyle distribution by profile')


ggplot(data.apac_labelled_augmented
) +
  aes(x = `TQ Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Teaching Quality distribution by profile')

ggplot(data.apac_labelled_augmented
) +
  aes(x = `UB Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('University Brand distribution by profile')




ggplot(data.apac_labelled_augmented
) +
  aes(x = `CL Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1,) + theme_classic() +
  ggtitle('Cost of Living distribution by profile')


ggplotly(ggplot(data.apac_labelled_augmented,
) +
  aes(x = `EM Rating`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1,) + theme_classic() +
  ggtitle('Employment distribution by profile'))

ggplotly(ggplot(data.apac_labelled_augmented,
) +
  aes(x = `LE Rating`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1,) + theme_classic() +
  ggtitle('Lifestyle distribution by profile'))

ggplotly(ggplot(data.apac_labelled_augmented,
) +
  aes(x = `TQ Rating`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1,) + theme_classic() +
  ggtitle('Teaching Quality distribution by profile'))

ggplotly(ggplot(data.apac_labelled_augmented,
) +
  aes(x = `UB Rating`, colour = profile, fill = profile) +
  geom_density(alpha=.25, size =.1,) + theme_classic() +
  ggtitle('University Brand distribution by profile'))



write.csv(data.apac_labelled_augmented, file = 'data_apac_labelled_augmented.csv', row.names = F)
