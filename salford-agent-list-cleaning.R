library(dplyr)
library(stringr)
library(magrittr)
salfrod_agent_raw = tbl_df(read.csv(file = 'agent_list_salford.csv'
                                    ,header = TRUE
                                    ,stringsAsFactors = F))


salfrod_agent_raw$agent_full_info

salfrod_agent_raw$agent_full_info = gsub("<strong>","", salfrod_agent_raw$agent_full_info)
salfrod_agent_raw$agent_full_info = gsub("</strong>","", salfrod_agent_raw$agent_full_info)
salfrod_agent_raw$agent_full_info = gsub("<p>","", salfrod_agent_raw$agent_full_info)
salfrod_agent_raw$agent_full_info = gsub("</p>","", salfrod_agent_raw$agent_full_info)
salfrod_agent_raw %<>%
  mutate(telephone = grepl(pattern = 'Telephone', salfrod_agent_raw$agent_full_info),
         email = grepl(pattern = 'Email', salfrod_agent_raw$agent_full_info),
         website = grepl(pattern = 'Website', salfrod_agent_raw$agent_full_info),
         address = ifelse(telephone, str_trim(sub("Telephone.*", "", salfrod_agent_raw$agent_full_info)),
                ifelse(email, str_trim(sub("Email.*", "", salfrod_agent_raw$agent_full_info)),
                       ifelse(website,str_trim(sub("Website.*", "", salfrod_agent_raw$agent_full_info)),salfrod_agent_raw$agent_full_info))),
         telephone_num = ifelse(telephone, gsub(" ","",str_trim(gsub(":","",sub(",.*", "",str_trim(sub(".*Telephone", "", salfrod_agent_raw$agent_full_info)))))), ''),
         email_link = ifelse(email, sub("<.*", "", sub(".*\">", "", sub(",.*", "", sub(".*Email", "", salfrod_agent_raw$agent_full_info)))), ''))

write.csv(x = salfrod_agent_raw
          ,file = 'salford_agent_cleaner.csv'
          , row.names = F)
#################
sub("Telephone.*", "", salfrod_agent_raw$agent_full_info[1])
