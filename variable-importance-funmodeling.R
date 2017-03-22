
library(funModeling)
 

correlation_table(data = features[,-c(1)], 
                  str_target="Enrolled")
# generate all plots and cross with has_heart_disease

cross_enrolled=cross_plot(features[,-c(1)], str_target="Enrolled", path_out = "cross_Enrolled", auto_binning = TRUE)

plotar(data=features[,-c(1)], str_target="Enrolled", plot_type = "boxplot", path_out = "boxplot_Enrolled")

plotar(data=features[,-c(1)], str_target="Enrolled", plot_type = "histdens", path_out = "density_Enrolled")
