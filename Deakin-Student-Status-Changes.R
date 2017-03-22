my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      legend.position = "bottom",
      legend.justification = "top",
      legend.box = "horizontal",
      legend.box.background = element_rect(colour = "grey50"),
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

dbhandle <- RODBC::odbcDriverConnect(connection = 'driver={SQL Server};server=hob-srv-16.dmz.hobsons.com.au\\MSSQLSERVER2012;database=synergygdw;trusted_connection=true')
client_name <- 'HAU_DEAKINUN'
# in progress - need to get student type
query_crms_student_status <- paste("
                            SELECT [client_id]
                            ,[crms_number]
	                          ,CONVERT(VARCHAR(10), [date_modified], 120) AS student_status_date_modified
                            ,[student_status_mod]
                            ,[intake_status_mod]
                            FROM [synergygdw].[dbo].[crms_student_status_history]
                            WHERE client_id = '",client_name,"'",sep = ""
)

Dsynergygdw.crms_student_status <- dplyr::tbl_df(RODBC::sqlQuery(dbhandle, query_crms_student_status))
Dsynergygdw.crms_student_status$student_status_date_modified <- as.Date(Dsynergygdw.crms_student_status$student_status_date_modified)
str(Dsynergygdw.crms_student_status)

# Deakin: student status mod cleaning
Dsynergygdw.crms_student_status$student_status_mod <- factor(Dsynergygdw.crms_student_status$student_status_mod
                                                             , levels = c("Enquiry", "Applicant", "Applicant Declined", "Offer Conditional", "Offer Full", "Offer Declined", "Accepted", "Enrolled" ))

# Deakin: intake status mod cleaning
levels(Dsynergygdw.crms_student_status$intake_status_mod)
Dsynergygdw.crms_student_status$intake_status_mod <- factor(Dsynergygdw.crms_student_status$intake_status_mod
                                                                    , levels = c("Semester 2 - 2003"
                                                                                 ,"Semester 1 - 2004"
                                                                                 ,"Semester 2 - 2004"
                                                                                 ,"Semester 2 - 2005"
                                                                                 ,"Semester 3 - 2005"
                                                                                 ,"Semester 1 -2006"
                                                                                 ,"Semester 2 - 2006"
                                                                                 ,"Semester 3 - 2006"
                                                                                 ,"Semester 1 - 2007"
                                                                                 ,"Semester 2 - 2007"
                                                                                 ,"Summer Semester - 2007"
                                                                                 ,"Semester 1 - 2008"
                                                                                 ,"Semester 2 - 2008"
                                                                                 ,"Trimester 1 - 2008"
                                                                                 ,"Trimester 2 - 2008"
                                                                                 ,"Trimester 1 - 2009"
                                                                                 ,"Trimester 2 - 2009"
                                                                                 ,"Trimester 3 - 2009"
                                                                                 ,"Trimester 1 - 2010"
                                                                                 ,"Trimester 2 - 2010"
                                                                                 ,"Trimester 3 - 2010"
                                                                                 ,"Trimester 1 - 2011"
                                                                                 ,"Trimester 2 - 2011"
                                                                                 ,"Trimester 3 - 2011"
                                                                                 ,"Trimester 1 - 2012"
                                                                                 ,"Trimester 2 - 2012"
                                                                                 ,"Trimester 3 - 2012"
                                                                                 ,"Trimester 1 - 2013"
                                                                                 ,"Trimester 2 - 2013"
                                                                                 ,"Trimester 3 - 2013"
                                                                                 ,"Trimester 1 - 2014"
                                                                                 ,"Trimester 2 - 2014"
                                                                                 ,"Trimester 3 - 2014"
                                                                                 ,"2015 - Trimester 1"
                                                                                 ,"2015 - Trimester 2"
                                                                                 ,"2015 - Trimester 3"
                                                                                 ,"HDR - 2015"
                                                                                 ,"2016 - Trimester 1"
                                                                                 ,"2016 - Trimester 2"
                                                                                 ,"2016 - Trimester 3"
                                                                                 ,"2017 - Trimester 1"
                                                                                 ,"2017 - Trimester 2"
                                                                                 ,"2017 - Trimester 3"
                                                                                 ,"2018 - Trimester 1"
                                                                                 ,"2018 - Trimester 2"
                                                                                 ,"2018 - Trimester 3"
                                                                                 ,"2019 - Trimester 1"))
levels(Dsynergygdw.crms_student_status$intake_status_mod) <- c("Semester 2 - 2003"
                                                               ,"Semester 1 - 2004"
                                                               ,"Semester 2 - 2004"
                                                               ,"Semester 2 - 2005"
                                                               ,"Semester 3 - 2005"
                                                               ,"Semester 1 -2006"
                                                               ,"Semester 2 - 2006"
                                                               ,"Semester 3 - 2006"
                                                               ,"Semester 1 - 2007"
                                                               ,"Semester 2 - 2007"
                                                               ,"Summer Semester - 2007"
                                                               ,"Semester 1 - 2008"
                                                               ,"Semester 2 - 2008"
                                                               ,"Trimester 1 - 2008"
                                                               ,"Trimester 2 - 2008"
                                                               ,"Trimester 1 - 2009"
                                                               ,"Trimester 2 - 2009"
                                                               ,"Trimester 3 - 2009"
                                                               ,"Trimester 1 - 2010"
                                                               ,"Trimester 2 - 2010"
                                                               ,"Trimester 3 - 2010"
                                                               ,"Trimester 1 - 2011"
                                                               ,"Trimester 2 - 2011"
                                                               ,"Trimester 3 - 2011"
                                                               ,"Trimester 1 - 2012"
                                                               ,"Trimester 2 - 2012"
                                                               ,"Trimester 3 - 2012"
                                                               ,"Trimester 1 - 2013"
                                                               ,"Trimester 2 - 2013"
                                                               ,"Trimester 3 - 2013"
                                                               ,"Trimester 1 - 2014"
                                                               ,"Trimester 2 - 2014"
                                                               ,"Trimester 3 - 2014"
                                                               ,"Trimester 1 - 2015"
                                                               ,"Trimester 2 - 2015"
                                                               ,"Trimester 3 - 2015"
                                                               ,"HDR - 2015"
                                                               ,"Trimester 1 - 2016"
                                                               ,"Trimester 2 - 2016"
                                                               ,"Trimester 3 - 2016"
                                                               ,"Trimester 1 - 2017"
                                                               ,"Trimester 2 - 2017"
                                                               ,"Trimester 3 - 2017"
                                                               ,"Trimester 1 - 2018"
                                                               ,"Trimester 2 - 2018"
                                                               ,"Trimester 3 - 2018"
                                                               ,"Trimester 1 - 2019")
# mark students who have enrolled
Dsynergygdw.crms_student_status <- left_join(Dsynergygdw.crms_student_status, Dsynergygdw.crms_student_status %>% filter(student_status_mod == "Enrolled") %>%
  mutate(enrolled = 'Enrolled') %>% select(crms_number, enrolled))
Dsynergygdw.crms_student_status[is.na(Dsynergygdw.crms_student_status$enrolled),]$enrolled = "Not Enrolled"
Dsynergygdw.crms_student_status$enrolled <- as.factor(Dsynergygdw.crms_student_status$enrolled)
table(Dsynergygdw.crms_student_status$enrolled)
Dsynergygdw.crms_student_status
str(Dsynergygdw.crms_student_status)

# mark students who have Enquiry Status
Dsynergygdw.crms_student_status <- left_join(Dsynergygdw.crms_student_status, Dsynergygdw.crms_student_status %>%
  filter(student_status_mod == "Enquiry") %>%
  mutate(enquirer = 1) %>% select(crms_number, enquirer))
Dsynergygdw.crms_student_status <- Dsynergygdw.crms_student_status %>%
  arrange(crms_number, student_status_date_modified, intake_status_mod)

ggplot(data = Dsynergygdw.crms_student_status %>% filter(enquirer == 1), aes(x = student_status_date_modified, y = student_status_mod, color = enrolled)) +
  geom_path(aes(group = crms_number)) +
  facet_wrap( ~ intake_status_mod, ncol = 7) +
  my_theme() +
  scale_color_brewer(palette = "Set2", na.value = "grey90") +
  #scale_fill_brewer(palette="Set1") +
  labs(
    color = "Student Status",
    x = "Date",
    y = "Student Status",
    title = "Deakin University Student Status Changes Over Time",
    subtitle = "Dataset from CRM",
    caption = "\nTime from first student status to final student status"
  )


for (s in levels(Dsynergygdw.crms_student_status$intake_status_mod)) {
  print(ggplot(data = Dsynergygdw.crms_student_status %>%
           arrange(crms_number, student_status_date_modified, intake_status_mod) %>%
           filter(intake_status_mod == s), aes(x = student_status_date_modified, y = student_status_mod, color = student_status_mod)) +
    geom_path(aes(group = crms_number)) +
    facet_wrap( ~ intake_status_mod + enrolled, ncol = 7) +
    my_theme() +
    scale_color_brewer(palette = "Set2", na.value = "grey60") +
    #scale_fill_brewer(palette="Set1") +
    labs(
      color = "Student Status",
      x = "Date",
      y = "Student Status",
      title = "Deakin University Student Status Changes Over Time",
      subtitle = "Dataset from CRM",
      caption = "\nTime from first student status to final student status"
    ))
  dev.copy(png,paste('images/intake-',s,'-2.png',sep = ''), width = 1280, height = 1024, res = 200)
  dev.off()
}
ggplot(data = Dsynergygdw.crms_student_status %>%
         arrange(crms_number, student_status_date_modified, intake_status_mod) %>%
         filter(intake_status_mod == sem), aes(x = student_status_date_modified, y = student_status_mod, color = enrolled)) +
  geom_path(aes(group = crms_number)) +
  facet_wrap( ~ intake_status_mod, ncol = 7) +
  my_theme() +
  scale_color_brewer(palette = "Set2", na.value = "grey60") +
  #scale_fill_brewer(palette="Set1") +
  labs(
    color = "Student Status",
    x = "Date",
    y = "Student Status",
    title = "Deakin University Student Status Changes Over Time",
    subtitle = "Dataset from CRM",
    caption = "\nTime from first student status to final student status"
  )



# Plot all, Path to Success
ggplot(data = Dsynergygdw.crms_student_status %>%
         arrange(crms_number, student_status_date_modified, intake_status_mod) %>%
         filter(enrolled == 1), aes(x = student_status_date_modified, y = student_status_mod, color = enrolled)) +
  geom_path(aes(group = crms_number)) +
  #facet_wrap( ~ intake_status_mod, ncol = 7) +
  my_theme() +
  scale_color_brewer(palette = "Set2",na.value = "grey60") +
  #scale_fill_brewer(palette="Set1") +
  labs(
    color = "Enrolled",
    x = "Date",
    y = "Student Status",
    title = "Deakin University Student: Path to Enrolment Over Time",
    subtitle = "Dataset from CRM: `Enrolled` Status based on [synergygdw].[dbo].[crms_student_status_history]",
    caption = "\nTime from first student status to final student status"
  )

ggplot(data = Dsynergygdw.crms_student_status %>%
         arrange(crms_number, student_status_date_modified, intake_status_mod) %>%
         filter(enrolled == 1), aes(x = student_status_date_modified, y = student_status_mod, color = enrolled)) +
  #geom_point(aes(shape = student_status_mod), size = 1.5, alpha = 0.6) +
  geom_path(aes(group = crms_number)) +
  facet_wrap( ~ intake_status_mod, ncol = 7) +
  my_theme() +
  scale_color_brewer(palette="Set2",na.value = "grey60") +
  #scale_fill_brewer(palette="Set1") +
  labs(
    color = "Enrolled",
    #shape = "Student Status",
    x = "Date",
    y = "Student Status",
    title = "Deakin University Student: Path to Enrolment Over Time",
    subtitle = "Dataset from CRM: `Enrolled` Status based on [synergygdw].[dbo].[crms_student_status_history]",
    caption = "\nTime from first student status to final student status"
  )


# Plot all, Failed Path
ggplot(data = Dsynergygdw.crms_student_status %>%
         arrange(crms_number, student_status_date_modified, intake_status_mod) %>%
         filter(is.na(enrolled)), aes(x = student_status_date_modified, y = student_status_mod, color = enrolled)) +
  geom_path(aes(group = crms_number)) +
  #facet_wrap( ~ intake_status_mod, ncol = 7) +
  my_theme() +
  scale_color_brewer(palette="Set2",na.value = "grey60") +
  #scale_fill_brewer(palette="Set1") +
  labs(
    color = "Enrolled",
    x = "Date",
    y = "Student Status",
    title = "Deakin University Student: Path to Enrolment Over Time",
    subtitle = "Dataset from CRM: `Enrolled` Status based on [synergygdw].[dbo].[crms_student_status_history]",
    caption = "\nTime from first student status to final student status"
  )


# Rate of Conversion
convertion <- right_join(Dsynergygdw.crms_student_status %>%
  filter(enrolled == 1) %>%
  group_by(intake_status_mod) %>%
  summarise(tot_enrolled = n())
,Dsynergygdw.crms_student_status %>%
  group_by(intake_status_mod) %>%
  summarise(tot_population = n()))
convertion[is.na(convertion$tot_enrolled),]$tot_enrolled <- 0
convertion <- convertion %>% mutate(conversion_rate = round(tot_enrolled / tot_population * 100,2))


tt <- Dsynergygdw.crms_student_status %>%
  filter(intake_status_mod == "Trimester 3 - 2017")
