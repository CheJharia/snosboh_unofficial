devtools::install_github("airbnb/Rbnb")
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies=TRUE)

library(ggradar)

suppressPackageStartupMessages(library(dplyr))
library(scales)

mtcars %>%
  add_rownames( var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% select(1:10) -> mtcars_radar
str(mtcars_radar)
str(spider_data)
spider_data$profile <- as.character(spider_data$profile)
ggradar(mtcars_radar[,1:4])
ggradar(spider_data[,1:3])
mtcars_radar$mpg <- mtcars_radar$mpg + 100

ggradar(cbind(spider_data$profile,spider_data[,2:6]/3))
colnames(spider_data)[2:6] <- c("Cost of Living", "Employment", "Lifestyle", 
                                "Teaching Quality", "University Brand")


