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


ggplot(data.global_labelled
) +
  aes(x = `CL Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Cost of Living distribution by profile')


ggplot(data.global_labelled
) +
  aes(x = `EM Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Employment distribution by profile')

ggplot(data.global_labelled
) +
  aes(x = `LE Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Lifestyle distribution by profile')


ggplot(data.global_labelled
) +
  aes(x = `TQ Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('Teaching Quality distribution by profile')

ggplot(data.global_labelled
) +
  aes(x = `UB Rating`, colour = profile, fill = profile) +
  geom_bar(alpha=.25, size =.1) + theme_classic() +
  facet_grid(~ profile) +
  ggtitle('University Brand distribution by profile')



#write.csv(data.global_labelled, file = 'data_global_profiled_augmented.csv', row.names = F)
