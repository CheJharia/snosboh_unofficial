

################################################################################
################################################################################
################################################################################
# Data Preparation
################################################################################
################################################################################

# clear all objects
rm(list = ls())
# clear console
cat("\014")


library(tidyverse)
library(cluster)
library(factoextra)
library(fields)
library(ggradar)
library(scales)
library(nsprcomp)
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

################################################################################
################################################################################
################################################################################
# SCORING
################################################################################
################################################################################


DCL_eigs <- nscumcomp(x = DtCL_complete, nneg = TRUE)
DEM_eigs <- nscumcomp(x = DtEM_complete, nneg = TRUE)
DLE_eigs <- nscumcomp(x = DtLE_complete, nneg = TRUE)
DTQ_eigs <- nscumcomp(x = DtTQ_complete, nneg = TRUE)
DUB_eigs <- nscumcomp(x = DtUB_complete, nneg = TRUE)



DScore <- bind_cols(tbl_df(tbl_df(as.matrix(DtCL_complete) %*% DCL_eigs$rotation) %>% rowSums())
,tbl_df(tbl_df(as.matrix(DtEM_complete) %*% DEM_eigs$rotation) %>% rowSums())
,tbl_df(tbl_df(as.matrix(DtLE_complete) %*% DLE_eigs$rotation) %>% rowSums())
,tbl_df(tbl_df(as.matrix(DtTQ_complete) %*% DTQ_eigs$rotation) %>% rowSums())
,tbl_df(tbl_df(as.matrix(DtUB_complete) %*% DUB_eigs$rotation) %>% rowSums()))
colnames(DScore) <- c("Score_CL", "Score_EM", "Score_LE", "Score_TQ", "Score_UB")
DScore_scaled <- tbl_df(scale(DScore)[,])

write.csv(x = bind_cols(DtID_complete, DScore, DScore_scaled)
          ,file = 'scoreFinal.csv'
          ,row.names = F)

################################################################################
# FINDING OPTIMUM NUMBER OF CLUSTERS
################################################################################
# +++ Using NbClust package to determine the Best Number of Clusters in a Data Set
# It provides 30 indexes for determining the optimal number of clusters in a data set and offers
# the best clustering scheme from different results to the user.
# NbClust package provides 30 indices for determining the number of clusters and proposes to user
# the best clustering scheme from the different results obtained by varying all combinations of number
# of clusters, distance measures, and clustering methods
res <- NbClust(DScore, distance = "euclidean", min.nc = 2, max.nc = 10,
               method = "ward.D2", index = "all")


# KMEANS clustering
# ++++++++++++++++++++



kmeans_gap_stat <- clusGap(x = DScore, FUNcluster = kmeans, K.max = 30, B = 5, d.power = 1, verbose = T
                           , spaceH0 = 'original'
                           )
fviz_gap_stat(kmeans_gap_stat)
print(kmeans_gap_stat)
# define number of cluster
nb_k <- 6
kmeans.res <- eclust(DScore, FUNcluster = "kmeans", k = nb_k, graph = F
                      ,verbose = T
                      ,stand = T # apply scale() to DScore
                      )
kmeans.res$data
table(kmeans.res$cluster)
fviz_cluster(kmeans.res, geom = "point", frame.type = "convex", show.clust.cent = T,
             title = paste("KMeans Clustering K = ", nb_k, sep = '')
)

tbl_df(kmeans.res$centers) %>%
  add_rownames( var = "profile") %>%
  mutate_each(funs(rescale), -profile) -> kmeans_radar
  colnames(kmeans_radar)[-1] <- c("Cost of Living", "Employment"
                                  ,"Lifestyle", "Teaching Quality", "University Brand")
  colorpalette <- RColorBrewer::brewer.pal(nrow(kmeans_radar), "Set1")
  ggradar(kmeans_radar, axis.label.size = 2.5) + ggtitle("Radar Chart - Profile comparisons") + theme(text = element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom") + scale_colour_manual(values = colorpalette)

for (pr in 1:nrow(kmeans_radar)) {
  paste(pr)
  print(ggradar(kmeans_radar[pr,], axis.label.size = 2.5) + scale_colour_manual(values = colorpalette[pr]) + ggtitle  ("Radar Chart - Profile comparisons") + theme(text =       element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom"))
}

rm(list = setdiff(ls(), "DScore"))
gc()
library(NbClust)

