# loading libraries
library(googleVis)
library(dplyr)
library(reshape2)
list.files()
#  data from nott malaysia comm
student_status_history <- tbl_df(read.csv(file = "student-status-history.csv"))

student_status_history_order <- student_status_history %>%
  group_by(CRMSNumber) %>%
  mutate(n.ord = paste('status', c(1:n()), sep=''))

student_status_history_order <- dcast(student_status_history_order, CRMSNumber ~ n.ord, value.var='StudentStatus', fun.aggregate = NULL)

student_status_history_analysis <- student_status_history_order %>%
  select(status1, status2, status3, status4, status5
         ,status6, status7, status8, status9, status10
         ,status11, status12, status13, status14, status15)



orders.plot <- data.frame()

for (i in 2:ncol(student_status_history_analysis)) {
  
  ord.cache <- student_status_history_analysis %>%
    group_by(student_status_history_analysis[ , i-1], student_status_history_analysis[ , i]) %>%
    summarise(n=n())
  
  colnames(ord.cache)[1:2] <- c('from', 'to')
  
  # adding tags to carts
  ord.cache$from <- paste(ord.cache$from, '(', i-1, ')', sep='')
  ord.cache$to <- paste(ord.cache$to, '(', i, ')', sep='')
  
  orders.plot <- rbind(orders.plot, ord.cache)
  
}


plot(gvisSankey(orders.plot, from='from', to='to', weight='n',
                options=list(height=900, width=1800, sankey="{link:{color:{fill:'lightblue'}}}")))

plot(gvisSankey(orders.plot[!(grepl("NA",orders.plot$from) | grepl("NA",orders.plot$to)),], from='from', to='to', weight='n',
                options=list(height=900, width=1800, sankey="{link:{color:{fill:'lightblue'}}}")))


# remove NA rows
