library(funModeling)  
library(dplyr)
library(magrittr)
list.files()
# read raw survey data
apac_survey <- tbl_df(read.csv(file = "20160615010807-SurveyExport.csv"))

# filter out respondents who are not planning to study overseas
apac_survey_overseas <- apac_survey %>% filter (Are.you.planning.to.study.overseas..or.in.your.home.country. == "Overseas (in a country I am not a  permanent resident or citizen of)")

# verify variables
apac_survey_status=df_status(apac_survey)
apac_survey_overseas_status=df_status(apac_survey_overseas)  

# subset UTAS
utas_apac_survey_overseas <- apac_survey_overseas %>% filter(Link.Name == 'UTAS')

# subset Griffith
griffith_apac_survey_overseas <- apac_survey_overseas %>% filter(Link.Name == 'Griffith')

# verify UTAS questions
maputas_apac_survey_status=df_status(utas_apac_survey_overseas)  

# verify griffith questions
griffith_apac_survey_status=df_status(griffith_apac_survey_overseas)  

# verify UTAS new scoring questions
utas_apac_survey_status_new_questions <- utas_apac_survey_status[c(80, 83, 106, 115, 116, 118, 119, 120, 121, 132, 136, 140, 141, 148, 149, 150, 153, 156),]

# verify Griffith new scoring questions
griffith_apac_survey_status_new_questions <- griffith_apac_survey_status[c(80, 83, 106, 115, 116, 118, 119, 120, 121, 132, 136, 140, 141, 148, 149, 150, 153, 156),]


utas_apac_survey_overseas_new_questions <- utas_apac_survey_overseas[, c(1, 80, 83, 106, 115, 116, 118, 119, 120, 121, 132, 136, 140, 141, 148, 149, 150, 153, 156)]

griffith_apac_survey_overseas_new_questions <- griffith_apac_survey_overseas[, c(1, 80, 83, 106, 115, 116, 118, 119, 120, 121, 132, 136, 140, 141, 148, 149, 150, 153, 156)]

utas_apac_survey_overseas_new_questions[is.na(utas_apac_survey_overseas_new_questions[,c(2)]),c(2)] <- 0
utas_apac_survey_overseas_new_questions[is.na(utas_apac_survey_overseas_new_questions[,c(3)]),c(3)] <- 0

utas_apac_survey_overseas_new_questions_complete_cases <- na.omit(utas_apac_survey_overseas_new_questions) 

griffith_apac_survey_overseas_new_questions[is.na(griffith_apac_survey_overseas_new_questions[,c(2)]),c(2)] <- 0
griffith_apac_survey_overseas_new_questions[is.na(griffith_apac_survey_overseas_new_questions[,c(3)]),c(3)] <- 0

griffith_apac_survey_overseas_new_questions_complete_cases <- na.omit(griffith_apac_survey_overseas_new_questions) 

################################################################################
utas_apac_survey_overseas_new_questions_complete_cases %<>% mutate(global_response_id = paste("APAC", `Response.ID`))

griffith_apac_survey_overseas_new_questions_complete_cases %<>% mutate(global_response_id = paste("APAC", `Response.ID`))


