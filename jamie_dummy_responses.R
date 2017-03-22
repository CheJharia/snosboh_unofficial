library(tidyverse)

jdummyfiles <- list.files(pattern = "Dummy*")
alljdummydata <- do.call(rbind, lapply(jdummyfiles, function(x) readr::read_csv(file = x, progress = T, trim_ws = T)))
# replace all na
alljdummydata %>% mutate_each(funs(replace(., is.na(.), 0)))

alljdummydata
