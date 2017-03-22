
## Loading funModeling
suppressMessages(library(funModeling)) 
str(features)

correlation_table(data = features[,-c(1)], 
                  str_target="Enrolled")

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq + CommCount 
            + CommunicationClassEmail + CommDurationDay, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq + CommCount 
            , data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')

fit_glm=glm(Enrolled ~ CommunicationClassPhone + CommPhoneFreq + CommCount 
            + CommunicationClassEmail, data=features[,-c(1)], family = binomial)
features$score=predict(fit_glm, newdata=features[,-c(1)], type='response')
gain_lift(data=as.data.frame(features[,-c(1)]),str_score='score',str_target='Enrolled')
