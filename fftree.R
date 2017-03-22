library("FFTrees")
library(dplyr)
acu <- dplyr::tbl_df(read.csv(file = "boruta_for_decision_tree.csv"
                ,header = T
                ,check.names = F))
str(acu)
acu_filtered <- acu %>% filter(StudentStatus %in% c("Enrolled", "Withdrawn"))
str(acu_filtered)
acu_filtered <- droplevels(acu_filtered)
acu.fft <- FFTrees(formula = StudentStatus ~.,
                   data = acu)
showcues(acu.fft,
         main = "ACU SE&R cue accuracy")

plot(acu.fft,
     main = "ACU University SE&R Decision Trees: Tree 6", 
     decision.names = c("Withdrawn", "Enrolled"))
