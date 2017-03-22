library(dplyr)
library(stringr)
library(magrittr)
bradfrod_agent_raw = tbl_df(read.csv(file = 'Bradford_Agent_List_Raw_Unstructured.csv'
                                     ,header = TRUE
                                     ,stringsAsFactors = F))


bradfrod_agent_raw$content

bradfrod_agent_raw$content = gsub("Â"," ", bradfrod_agent_raw$content)
bradfrod_agent_raw$content = gsub("<br>"," ", bradfrod_agent_raw$content)
bradfrod_agent_raw$content = gsub("<p>"," ", bradfrod_agent_raw$content)
bradfrod_agent_raw$content = gsub("</p>"," ", bradfrod_agent_raw$content)

bradfrod_agent_raw %<>%
  mutate(agent_company_name = gsub("<strong>","", sub("*.<strong>", "", sub("</strong>.*", "", bradfrod_agent_raw$content)))
         ,contact_name = ifelse(contact_name_exist & tel_exist,str_trim(gsub("<strong>","", gsub("</strong>", "", gsub(":","", sub("<a href=.*", "",sub(".*Contact", "", sub("Tel.*", "", bradfrod_agent_raw$content))))))),"")
         ,email = ifelse(email_exist, sub(".*\">", "", sub("</a>.*", "", bradfrod_agent_raw$content)),"")
         ,telephone = ifelse(tel_exist & email_exist, str_trim(gsub("</span>","",gsub("<span>","",gsub("ephone","",gsub(":","", sub(".*Tel", "", sub("Email.*", "", bradfrod_agent_raw$content))))))),"")
         ,address = ifelse(address_exist, str_trim(sub(".*Address:", "", bradfrod_agent_raw$content)),""))
######################################

sub("*.<strong>", "", sub("</strong>.*", "", bradfrod_agent_raw$content[3]))
sub(".*\">", "", sub("</a>.*", "", bradfrod_agent_raw$content))
str_trim(gsub(":","", sub(".*Tel", "", sub("Email.*", "", bradfrod_agent_raw$content))))
str_trim(gsub(":","", sub("<a href=.*", "",sub(".*Contact", "", sub("Tel.*", "", bradfrod_agent_raw$content[3])))))
str_trim(sub(".*Address:", "", bradfrod_agent_raw$content[3]))

bradfrod_agent_raw$content[3]

write.csv(x = bradfrod_agent_raw
          ,file = 'bradford_agent_cleaner.csv'
          , row.names = F)
