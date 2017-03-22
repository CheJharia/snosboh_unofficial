library(dplyr)
library(magrittr)

# read global dataset
df <- read.csv("0 raw ProfileData-2016-03-17 15_36_30_all_cells.csv", header = TRUE, check.names = F, na.strings = c("","NA"))

# peek
head(df)
str(df)
colnames(df)
colnames(df)[8:10] <- c("University Name", "University Global Division", "University Prospect Type")
# change IDs to factor type
df %<>%
  mutate(RecordID = as.factor(RecordID),
         `Response ID` = as.factor(`Response ID`))
# only take complete cases
df.complete <- df[complete.cases(df),]

# see freq table by Survey Type: APAC and EMEA
df %>%
  group_by(`University Global Division`) %>%
  summarise(n = n()) %>%
  mutate(freq_percentage = round(n / sum(n) * 100, 2))

# see freq table by Survey Type: APAC and EMEA
# after cleaning

df.complete %>%
  group_by(`University Global Division`) %>%
  summarise(n = n()) %>%
  mutate(freq_percentage = round(n / sum(n) * 100, 2))

summary(df.complete)

# output as csv and do the scoring (for now) in Excel
write.csv(df.complete, file = "0 raw ProfileData-2016-03-17 15_36_30_complete_cells.csv", row.names = F)

