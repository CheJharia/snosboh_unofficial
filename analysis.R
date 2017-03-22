
# alternatives
# https://bost.ocks.org/mike/miserables/ty
library(dplyr)
df <- tbl_df(read.csv(file = 'clients_to_elsewhere.csv', header = T))
colnames(df) <- c("Client", "Lose to")
str(df)
summary(df)

# combine Unspecified and NA
df[df$`Lose to` == 'Unspecified',]$`Lose to`
df[df$`Lose to` == '#N/A',]$`Lose to` <- 'Unspecified'
# remove Unspecified/Other data18.07
df <- df[df$`Lose to` != 'Unspecified',]
df <- df[df$`Lose to` != 'Other',]

df <- droplevels(df)

# rename !Must be sequential and only once!
levels(df$Client)
levels(df$Client)[3:4] <- c("Australian National University")
levels(df$Client)
levels(df$Client)[5] <- c("Central Queensland University")
levels(df$Client)
levels(df$Client)[8:9] <- c("Griffith University")
levels(df$Client)
levels(df$Client)[11:12] <- c("Kangan Institute")
levels(df$Client)
levels(df$Client)[17:20] <- c("Swinburne University")
levels(df$Client)
levels(df$Client)[c(19,25)] <- c("University of Newcastle")
levels(df$Client)
levels(df$Client)[31] <- c("University of New South Wales")
levels(df$Client)

df_count <- df %>%
  group_by(Client, `Lose to`) %>%
  summarise(tot = n())
df_count <- droplevels(df_count)
str(df_count)

## clean using excel
#write.csv(df_count, file = 'prelimenary_cleaning_grouping.csv', row.names = F)

threshold <- 30
df_count_th <- df_count %>% filter(tot >= threshold)

levels(df$Client) %in% levels(df$`Lose to`)
common_unis <- levels(df$`Lose to`)[levels(df$`Lose to`) %in% levels(df$Client)]
df_common <- df_count[df_count$`Lose to` %in% common_unis,]
df_common <- droplevels(df_common)
str(df_common)
write.csv(df_count_th, file = 'client_losing_to.csv', row.names = F)

###
### Other ideas: filter by count then common unis

##
library(RColorBrewer)
library(colorspace)
RColorBrewer::display.brewer.all(type = 'qual')
RColorBrewer::brewer.pal.info
rainbow_hcl(36)
brewer.pal(n = 12,"Set3")
brewer.pal(n = 8,"Set2")
brewer.pal(n = 9,"Set1")
brewer.pal(n = 3,"Dark2")
