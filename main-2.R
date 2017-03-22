
# clear all objects
rm(list = ls())
# clear console
cat("\014")


library(tidyverse)
################################################################################
# READ: read survey data into R environment
Diss_2016 <- readr::read_csv(file = "20161116025341-SurveyExport.csv"
                             , progress = T
                             , trim_ws = T
)

################################################################################
# POPULATION: determine the population to profile/segment
# Split responses to 2 populations based on their answer
# for `624: Are you planning to study overseas, or in your home country?`

colnames(Diss_2016)

Diss_2016_INT <- Diss_2016 %>% dplyr::filter(`Are you planning to study overseas, or in your home country?` == "Overseas (in a country I am not a  permanent resident or citizen of)")
Diss_2016_DOM <- Diss_2016 %>% dplyr::filter(`Are you planning to study overseas, or in your home country?` == "In my home country (a country I am a  permanent resident or citizen of)")
dim(Diss_2016)[1] # 14,115 respondents
dim(Diss_2016_INT)[1] # 12,348 respondents
dim(Diss_2016_DOM)[1] # 1,765 respodents
# check for missing respondents
setdiff(setdiff(Diss_2016$`Response ID`, Diss_2016_INT$`Response ID`), Diss_2016_DOM$`Response ID`)
# VERIF: 2 respondents didnt answer the question `624: Are you planning ...`


# Check for missing values
# analyse missing values of each variable
my_data_status <- dplyr::tbl_df(funModeling::df_status(data = Diss_2016_INT
                                                       , print_results = F)) %>%
  mutate(var_original_pos = row_number()) %>%
  arrange(desc(p_na))
# get the variables position in the dataset for easy
# selection using dplyr

Dvars_id <- my_data_status %>% filter(variable %in% c("Response ID", "City" ,"Country","What is your nationality?", "Link Name"))
# select variables with less than 20pct NA
(Dvars <- my_data_status %>% filter(p_na < 20 & unique <= 10 & q_na < 100))
Dvars_char <- Dvars %>% filter(type == 'character')
Dvars_int <- Dvars %>% filter(type == 'integer')
# get candidate variables
DCandidateVariables <- Dvars_int # because we are using numeric variables for clustering this time

# send to Tim for review/selection
write.csv(x = DCandidateVariables
          ,file = 'fCandidate_Questions.csv'
          ,row.names = F)
DFinalClusterQuestions <- readr::read_csv(file = 'fCandidate_Questions_reviewed.csv'
                                          ,trim_ws = T)

DtCL <- Diss_2016_INT %>%
  select( (DFinalClusterQuestions %>% filter(theme == 1))$var_original_pos )
DtEM <- Diss_2016_INT %>%
  select( (DFinalClusterQuestions %>% filter(theme == 2))$var_original_pos )
DtLE <- Diss_2016_INT %>%
  select( (DFinalClusterQuestions %>% filter(theme == 3))$var_original_pos )
DtTQ <- Diss_2016_INT %>%
  select( (DFinalClusterQuestions %>% filter(theme == 4))$var_original_pos )
DtUB <- Diss_2016_INT %>%
  select( (DFinalClusterQuestions %>% filter(theme == 5))$var_original_pos )
DtID <- Diss_2016_INT %>%
  select( (Dvars_id %>% arrange(var_original_pos))$var_original_pos )
DtThemes <- dplyr::tbl_df(cbind(DtCL, DtEM, DtLE, DtTQ, DtUB))
DtFinal <- dplyr::tbl_df(cbind(DtID, DtThemes))

DtThemes_complete <- DtThemes[complete.cases(DtThemes),]
DtID_complete <- DtID[complete.cases(DtThemes),]
DtFinal_complete <- cbind(DtID[complete.cases(DtThemes),], DtThemes_complete)
DTheme <- DFinalClusterQuestions %>% group_by(theme) %>% summarise(n = n())
vNbCL <- as.numeric(DTheme[1,2])
vNbEM <- as.numeric(DTheme[2,2])
vNbLE <- as.numeric(DTheme[3,2])
vNbTQ <- as.numeric(DTheme[4,2])
vNbUB <- as.numeric(DTheme[5,2])
DtCL_complete <- DtThemes_complete %>% select(1:vNbCL)
DtEM_complete <- DtThemes_complete %>% select((vNbCL + 1):(vNbCL + vNbEM))
DtLE_complete <- DtThemes_complete %>% select((vNbCL + vNbEM + 1):(vNbCL + vNbEM + vNbLE))
DtTQ_complete <- DtThemes_complete %>% select((vNbCL + vNbEM + vNbLE + 1):(vNbCL + vNbEM + vNbLE + vNbTQ))
DtUB_complete <- DtThemes_complete %>% select((vNbCL + vNbEM + vNbLE + vNbTQ + 1):(vNbCL + vNbEM + vNbLE + vNbTQ + vNbUB))

# How many respondents for each client (international)?
inner_join(Diss_2016_INT %>% group_by(`Link Name`) %>%
             summarise(total_respondents = n()) %>% arrange(desc(total_respondents)),
           DtFinal %>% group_by(`Link Name`) %>%
             summarise(total_respondents_clean = n()) %>% arrange(desc(total_respondents_clean)), by = c("Link Name" = "Link Name")) %>% mutate(p_diff = total_respondents - total_respondents_clean)

DtFinal_complete_murdoch <- dplyr::tbl_df(DtFinal_complete %>% filter(`Link Name` == 'Murdoch'))
DtCL_complete_murdoch <- DtFinal_complete_murdoch[,-c(1:5)] %>% select(1:vNbCL)
DtEM_complete_murdoch <- DtFinal_complete_murdoch[,-c(1:5)] %>% select((vNbCL + 1):(vNbCL + vNbEM))
DtLE_complete_murdoch <- DtFinal_complete_murdoch[,-c(1:5)] %>% select((vNbCL + vNbEM + 1):(vNbCL + vNbEM + vNbLE))
DtTQ_complete_murdoch <- DtFinal_complete_murdoch[,-c(1:5)] %>% select((vNbCL + vNbEM + vNbLE + 1):(vNbCL + vNbEM + vNbLE + vNbTQ))
DtUB_complete_murdoch <- DtFinal_complete_murdoch[,-c(1:5)] %>% select((vNbCL + vNbEM + vNbLE + vNbTQ + 1):(vNbCL + vNbEM + vNbLE + vNbTQ + vNbUB))

# Scoring
library(FactoMineR)
library(factoextra)
library(rARPACK)

## If retvec == FALSE, we don't calculate eigenvectors
DCL_eigs <- eigs_sym(cov(scale(DtCL_complete_murdoch)), k = ncol(DtCL_complete_murdoch), which = "LM", opts = list(retvec = TRUE))
DEM_eigs <- eigs_sym(cov(scale(DtEM_complete_murdoch)), k = ncol(DtEM_complete_murdoch), which = "LM", opts = list(retvec = TRUE))
DLE_eigs <- eigs_sym(cov(scale(DtLE_complete_murdoch)), k = ncol(DtLE_complete_murdoch), which = "LM", opts = list(retvec = TRUE))
DTQ_eigs <- eigs_sym(cov(scale(DtTQ_complete_murdoch)), k = ncol(DtTQ_complete_murdoch), which = "LM", opts = list(retvec = TRUE))
DUB_eigs <- eigs_sym(cov(scale(DtUB_complete_murdoch)), k = ncol(DtUB_complete_murdoch), which = "LM", opts = list(retvec = TRUE))
DScore <- dplyr::tbl_df(cbind(DtCL_complete_murdoch
                              ,abs(tbl_df(as.matrix(DtCL_complete_murdoch) %*% DCL_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_CL = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_CL)
                              ,DtEM_complete_murdoch
                              ,abs(tbl_df(as.matrix(DtEM_complete_murdoch) %*% DEM_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_EM = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_EM)
                              ,DtLE_complete_murdoch
                              ,abs(tbl_df(as.matrix(DtLE_complete_murdoch) %*% DLE_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_LE = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_LE)
                              ,DtTQ_complete_murdoch
                              ,abs(tbl_df(as.matrix(DtTQ_complete_murdoch) %*% DTQ_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_TQ = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_TQ)
                              ,DtUB_complete_murdoch
                              ,abs(tbl_df(as.matrix(DtUB_complete_murdoch) %*% DUB_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_UB = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_UB)))
write.csv(x = DScore
          ,file = 'scoreFinal_murdoch.csv'
          ,row.names = F)
Dcluster <- DScore %>% select(starts_with("Score_"))
Dcluster_scaled <- dplyr::tbl_df(round(scale(Dcluster)[,],5))
str(Dcluster_scaled)
summary(Dcluster_scaled)


colnames(Dcluster_scaled) <- paste(colnames(Dcluster_scaled),"_normalized", sep = '')

Dcluster_scaled_ranked <- Dcluster_scaled %>%
  dplyr::mutate(CL_percentage_rank = percent_rank(Score_CL_normalized),
                EM_percentage_rank = percent_rank(Score_EM_normalized),
                LE_percentage_rank = percent_rank(Score_LE_normalized),
                TQ_percentage_rank = percent_rank(Score_TQ_normalized),
                UB_percentage_rank = percent_rank(Score_UB_normalized))
Dcluster_percentage_rank <- Dcluster_scaled_ranked %>% select(contains("percentage_rank"))



# clustering
library(cluster)
kmeans_gap_stat <- clusGap(Dcluster_scaled, FUN = kmeans, nstart = 25, K.max = 20, B = 5, d.power = 2, verbose = T)
print(kmeans_gap_stat, method = "globalmax")
#c("firstSEmax", "Tibs2001SEmax", "globalSEmax",
#  "firstmax", "globalmax"),
fviz_gap_stat(kmeans_gap_stat) # 10 or 11 clusters

pam_gap_stat <- clusGap(Dcluster, FUN = pam, K.max = 10, B = 5, d.power = 2, verbose = T)
fviz_gap_stat(pam_gap_stat) # 4 clusters
# Gap statistic for hierarchical clustering
gc()
hcut_gap_stat <- clusGap(Dcluster, FUN = hcut, K.max = 10, B = 5, d.power = 2, verbose = T)
fviz_gap_stat(hcut_gap_stat) # 5 clusters
?clusGap

fanny_gap_stat <- clusGap(Dcluster, FUN = fanny, K.max = 10, B = 5, d.power = 2, verbose = T)
fviz_gap_stat(fanny_gap_stat) # 7 clusters

