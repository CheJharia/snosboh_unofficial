# Phone - Inbound
# group by client, nationality

DCommStud_client_nationality_hour <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(client_id, nationality, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())

# group by client, country of origin

DCommStud_client_country_of_origin_hour <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(client_id, country_of_origin, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())

# group by client, country of residence

DCommStud_client_country_of_residence_hour <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(client_id, country_of_residence, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())
# group by client, gender
DCommStud_client_gender_hour <- DCommStud %>%
  filter(class == 'Phone',in_out == 'IN') %>%
  group_by(client_id, gender, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())
# group by nationality

DCommStud_nationality_hour <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(nationality, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())
# group by country of origin

DCommStud_country_of_origin_hour <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(country_of_origin, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())

# group by country of residence
DCommStud_country_of_residence_hour <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(country_of_residence, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())
# group by gender
DCommStud_gender_hour <- DCommStud %>%
  filter(class == 'Phone', in_out == 'IN') %>%
  group_by(gender, Year, Month, Day, Hour) %>%
  summarise(month_hourly_phone_in_comm = n())
gc()

# nationality
#6800258
# country of origin
#36703146
# country of residence
#4546269
