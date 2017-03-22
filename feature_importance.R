

# http://rstatistics.net/variable-importance-of-predictors-that-contribute-most-significantly-to-a-response-variable-in-r/

# Random Forest Method

library(party)
cfenquiry <- cforest( Enrol_Enrolled ~ . , data = Dtrain_EnquiryManagement, control = cforest_unbiased(mtry = 10,ntree = 50)) # fit the random forest
cfoffer <- cforest( Enrol_Enrolled ~ . , data = Dtrain_OfferManagement, control = cforest_unbiased(mtry = 10,ntree = 50)) # fit the random forest
varimp(cfenquiry) # get variable importance, based on mean decrease in accuracy
varimp(cfenquiry, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
varimpAUC(cfenquiry)  # more robust towards class imbalance.

varimp(cfoffer) # get variable importance, based on mean decrease in accuracy
varimp(cfoffer, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
varimpAUC(cfoffer)  # more robust towards class imbalance.

# Relative Importance
library(relaimpo)
lmModenquiry <- lm(Enrol_Enrolled ~ . , data = Dtrain_EnquiryManagement)  # fit lm() model
relImportanceEnquiry <- calc.relimp(lmModenquiry, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportanceEnquiry$lmg, decreasing=TRUE)  # relative importance

lmModoffer <- lm(Enrol_Enrolled ~ . , data = Dtrain_OfferManagement)  # fit lm() model
relImportanceOffer <- calc.relimp(lmModoffer, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportanceOffer$lmg, decreasing=TRUE)  # relative importance


# MARS
library(earth)
marsModelEnquiry <- earth(Enrol_Enrolled ~ . , data = Dtrain_EnquiryManagement) # build model
evEnquiry <- evimp (marsModelEnquiry) # estimate variable importance
plot(evEnquiry)
marsModelOffer<- earth(Enrol_Enrolled ~ . , data = Dtrain_OfferManagement) # build model
evOffer <- evimp (marsModelOffer) # estimate variable importance
plot(evOffer)


# Boruta

library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_outputEnquiry <- Boruta(Enrol_Enrolled ~ . , data = na.omit(Dtrain_EnquiryManagement), doTrace=2)  # perform Boruta search
# Confirmed 10 attributes: Humidity, Inversion_base_height, Inversion_temperature, Month, Pressure_gradient and 5 more.
# Rejected 3 attributes: Day_of_month, Day_of_week, Wind_speed.
boruta_signifEnquiry <- names(boruta_outputEnquiry$finalDecision[boruta_outputEnquiry$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signifEnquiry)  # significant variables
#=> [1] "Month"                 "ozone_reading"         "pressure_height"      
#=> [4] "Humidity"              "Temperature_Sandburg"  "Temperature_ElMonte"  
#=> [7] "Inversion_base_height" "Pressure_gradient"     "Inversion_temperature"
#=> [10] "Visibility"
plot(boruta_outputEnquiry, cex.axis=.7, las=2, xlab="", main="Variable Importance (Enquiry)")  # plot variable importance


boruta_outputOffer <- Boruta(Enrol_Enrolled ~ . , data = na.omit(Dtrain_OfferManagement), doTrace=2)  # perform Boruta search
# Confirmed 10 attributes: Humidity, Inversion_base_height, Inversion_temperature, Month, Pressure_gradient and 5 more.
# Rejected 3 attributes: Day_of_month, Day_of_week, Wind_speed.
boruta_signifOffer <- names(boruta_outputOffer$finalDecision[boruta_outputOffer$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signifOffer)  # significant variables
#=> [1] "Month"                 "ozone_reading"         "pressure_height"      
#=> [4] "Humidity"              "Temperature_Sandburg"  "Temperature_ElMonte"  
#=> [7] "Inversion_base_height" "Pressure_gradient"     "Inversion_temperature"
#=> [10] "Visibility"
plot(boruta_outputOffer, cex.axis=.7, las=2, xlab="", main="Variable Importance (Offer)")  # plot variable importance
