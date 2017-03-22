library(funModeling) 

my_data_status <- df_status(training)

# Removing variables with 60% of zero values
vars_to_remove=subset(my_data_status, my_data_status$p_zeros > 1)  
vars_to_remove["variable"]  

## Keeping all except vars_to_remove 
training_2 <- training[, !(names(training) %in% vars_to_remove[,"variable"])]

#Ordering data by percentage of zeros
my_data_status[order(-my_data_status$p_zeros),] 

#training_2 <- right_join(training_2,view_TPAS %>% select(CRMSNumber,StudentStatus)
#                       ,by = c("crms_number" = "CRMSNumber"))
training_2$StudentStatus <- as.factor(training_2$StudentStatus)
table(training_2$StudentStatus)
training_2 <- training_2 %>% filter(StudentStatus == "Enrolled")
training_2$nb_na <- apply(training_2, 1, function(x) sum(is.na(x)))
table(training_2$nb_na)
training_2 %>% filter(nb_na <= 5)
cross_enrolled=cross_plot(training_2[,-c(1)], str_target="StudentStatus", path_out = "cross_enrolled"
                          ,auto_binning = F)  
summary(training_2)
