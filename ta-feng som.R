library(readr)
library(tidyverse)
readr::read_delim(file = 'D01', delim = ';', skip = 1, col_names = F)
temp <- c("D01","D02","D11","D12")

alldata <- do.call(rbind, lapply(tripallexc2013, function(x) readr::read_csv(file = x, progress = T, trim_ws = T)))
alldata <- do.call(rbind, lapply(temp, function(x) readr::read_delim(file = x, delim = ';', skip = 1, col_names = F)))
colnames(alldata) <- c("date","customer_id","age_group","address","bclass","product_id","quantity","asset","price")
colnames(alldata)
dim(alldata)

system.time(test1 <- alldata[, sum(price), by = customer_id])
alldata %>% group_by(customer_id) %>% mutate(test1 = sum(price))
