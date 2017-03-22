

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

Dvars_id <- my_data_status %>% filter(variable %in% c("Response ID", "City" ,"Country","What is your nationality?", "Link Name","PrincipleSubjectName:When applying to university which subject will/have you applied to study?","SubjectGroupName:When applying to university which subject will/have you applied to study?"))
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
library(dplyr)
DtCL <- Diss_2016_INT %>%
  dplyr::select( (DFinalClusterQuestions %>% filter(theme == 1))$var_original_pos )
DtEM <- Diss_2016_INT %>%
  dplyr::select( (DFinalClusterQuestions %>% filter(theme == 2))$var_original_pos )
DtLE <- Diss_2016_INT %>%
  dplyr::select( (DFinalClusterQuestions %>% filter(theme == 3))$var_original_pos )
DtTQ <- Diss_2016_INT %>%
  dplyr::select( (DFinalClusterQuestions %>% filter(theme == 4))$var_original_pos )
DtUB <- Diss_2016_INT %>%
  dplyr::select( (DFinalClusterQuestions %>% filter(theme == 5))$var_original_pos )
DtID <- Diss_2016_INT %>%
  dplyr::select( (Dvars_id %>% arrange(var_original_pos))$var_original_pos )
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
DtCL_complete <- DtThemes_complete %>% dplyr::select(1:vNbCL)
DtEM_complete <- DtThemes_complete %>% dplyr::select((vNbCL + 1):(vNbCL + vNbEM))
DtLE_complete <- DtThemes_complete %>% dplyr::select((vNbCL + vNbEM + 1):(vNbCL + vNbEM + vNbLE))
DtTQ_complete <- DtThemes_complete %>% dplyr::select((vNbCL + vNbEM + vNbLE + 1):(vNbCL + vNbEM + vNbLE + vNbTQ))
DtUB_complete <- DtThemes_complete %>% dplyr::select((vNbCL + vNbEM + vNbLE + vNbTQ + 1):(vNbCL + vNbEM + vNbLE + vNbTQ + vNbUB))

# How many respondents for each client (international)?
inner_join(Diss_2016_INT %>% group_by(`Link Name`) %>%
             summarise(total_respondents = n()) %>% arrange(desc(total_respondents)),
           DtFinal_complete %>% group_by(`Link Name`) %>%
             summarise(total_respondents_clean = n()) %>% arrange(desc(total_respondents_clean)), by = c("Link Name" = "Link Name")) %>% mutate(p_diff = total_respondents - total_respondents_clean)

################################################################################
################################################################################
################################################################################
# SCORING
################################################################################
################################################################################
library(FactoMineR)
pcaCP <- PCA(DtCL_complete, scale.unit=TRUE, ncp=5, graph=T)
pcaEM <- PCA(DtEM_complete, scale.unit=TRUE, ncp=9, graph=T)
pcaLE <- PCA(DtLE_complete, scale.unit=TRUE, ncp=11, graph=T)
pcaTQ <- PCA(DtTQ_complete, scale.unit=TRUE, ncp=8, graph=T)
pcaUB <- PCA(DtUB_complete, scale.unit=TRUE, ncp=5, graph=T)
pcaOvr <- PCA(DtThemes_complete, scale.unit = T, ncp = 47, graph = T)

d <- 5

CP_pca_score <- tbl_df(as.matrix(DtCL_complete)  %*%
                         ((as.matrix(pcaCP$var$contrib[,c(1:d)]) %*% as.matrix(pcaCP$eig[,1][1:d])) /
                            (as.matrix(pcaCP$var$contrib[,c(1:d)]) %*% as.matrix(pcaCP$eig[,1][1:d]) %>% colSums())))

EM_pca_score <- tbl_df(as.matrix(DtEM_complete) %*%
                         ((as.matrix(pcaEM$var$contrib[,c(1:d)]) %*% as.matrix(pcaEM$eig[,1][1:d]) /
                            as.matrix(pcaEM$var$contrib[,c(1:d)]) %*% as.matrix(pcaEM$eig[,1][1:d]) %>% colSums())))

LE_pca_score <- tbl_df(as.matrix(DtLE_complete) %*%
                         ((as.matrix(pcaLE$var$contrib[,c(1:d)]) %*% as.matrix(pcaLE$eig[,1][1:d]) /
                            as.matrix(pcaLE$var$contrib[,c(1:d)]) %*% as.matrix(pcaLE$eig[,1][1:d]) %>% colSums())))

TQ_pca_score <- tbl_df(as.matrix(DtTQ_complete) %*%
                         ((as.matrix(pcaTQ$var$contrib[,c(1:d)]) %*% as.matrix(pcaTQ$eig[,1][1:d]) /
                            as.matrix(pcaTQ$var$contrib[,c(1:d)]) %*% as.matrix(pcaTQ$eig[,1][1:d]) %>% colSums())))

UB_pca_score <- tbl_df(as.matrix(DtUB_complete) %*%
                         ((as.matrix(pcaUB$var$contrib[,c(1:d)]) %*% as.matrix(pcaUB$eig[,1][1:d]) /
                            as.matrix(pcaUB$var$contrib[,c(1:d)]) %*% as.matrix(pcaUB$eig[,1][1:d]) %>% colSums())))
DScore_pca <- bind_cols(CP_pca_score, EM_pca_score, LE_pca_score, TQ_pca_score, UB_pca_score)
(DScore_pca.rank <- floor(tbl_df(t(apply(DScore_pca, MARGIN = 1, FUN = function(x) rank(x))))))
colnames(DScore_pca) <- paste(c('CP','EM','LE','TQ','UB'), rep('_pca_score',5), sep = '')
colnames(DScore_pca.rank) <- paste(c('CP','EM','LE','TQ','UB'), rep('_pca_score_rank',5), sep = '')
DScore_pca.scaled <- scale(DScore_pca)[,]
colnames(DScore_pca.scaled) <- paste(c('CP','EM','LE','TQ','UB'), rep('_pca_score',5), sep = '')
RATING <- 20

DScore_pcaScaledRated <- tbl_df(DScore_pca.scaled) %>%
  dplyr::mutate(`CL percentage rank` = percent_rank(`CP_pca_score`),
                `EM percentage rank` = percent_rank(`EM_pca_score`),
                `LE percentage rank` = percent_rank(`LE_pca_score`),
                `TQ percentage rank` = percent_rank(`TQ_pca_score`),
                `UB percentage rank` = percent_rank(`UB_pca_score`),
                `CL Rating` = ntile(`CL percentage rank`,RATING),
                `EM Rating` = ntile(`EM percentage rank`,RATING),
                `LE Rating` = ntile(`LE percentage rank`,RATING),
                `TQ Rating` = ntile(`TQ percentage rank`,RATING),
                `UB Rating` = ntile(`UB percentage rank`,RATING))

ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`CL percentage rank`)) + geom_density()
ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`CL Rating`)) + geom_density()

ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`EM percentage rank`)) + geom_density()
ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`EM Rating`)) + geom_density()

myv <- t(t(c(2,4,6,10,15)))
sum(dist(myv))
DScore_pcaScaledRated %>% select(ends_with('Rating')) -> DScore_pcaScaledRated_r
sum(dist(t(DScore_pca[1,])))
vv <- c()
for(i in 1:nrow(DScore_pcaScaledRated_r)){
  vv <- c(vv, sum(dist(t(DScore_pcaScaledRated_r[i,]))))
}
vv <- tbl_df(vv)
nrow(vv)
DScore_pcaScaledRated_r_q <- bind_cols(DScore_pcaScaledRated_r, tbl_df(vv))

ggplot(tbl_df(DScore_pca.rank), aes(x=UB_pca_score_rank)) + geom_density()

quantile(DScore_pcaScaledRated_r_q$value, c(.25,.5,.75))

View(bind_cols(DScore_pcaScaledRated_r, tbl_df(vv)))
DScore_pca
vv


ggplot(iris, aes(Sepal.Length, Petal.Length)) +
      geom_point(aes(col = Species)) +
      scale_colour_manual(values = CPCOLS)
ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`LE Rating`)) + geom_density()


ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`TQ percentage rank`)) + geom_density()
ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`TQ Rating`)) + geom_density()


ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`UB percentage rank`)) + geom_density()
ggplot(tbl_df(DScore_pcaScaledRated), aes(x=`UB Rating`)) + geom_density()

quantile(DScore_pcaScaledRated_r_q$value, c(.25,.5,.75))

maxB <- 65
minB <- 45
DScore_pcaScaledRated %>% select(ends_with('Rating')) %>% mutate(sumRate = rowSums(.)) %>% mutate(G = (sumRate < maxB & sumRate > minB)) %>% filter(G == T) %>% select(ends_with('Rating')) -> DScore_pcaScaledRated.bis

DScore_pcaScaledRated_r_q %>% filter(value >= minB & value <= maxB) %>% select(ends_with('Rating')) -> DScore_pcaScaledRated.bis

ggplot(tbl_df(DScore_pca), aes(x=CP_pca_score)) + geom_density()
ggplot(tbl_df(DScore_pca), aes(x=EM_pca_score)) + geom_density()
ggplot(tbl_df(DScore_pca), aes(x=LE_pca_score)) + geom_density()
ggplot(tbl_df(DScore_pca), aes(x=TQ_pca_score)) + geom_density()
ggplot(tbl_df(DScore_pca), aes(x=UB_pca_score)) + geom_density()

DScore_pca %>% mutate(CP_pca_score_pnorm = (CP_pca_score - min(CP_pca_score)) / (max(CP_pca_score) - min(CP_pca_score)),
                      EM_pca_score_pnorm = (EM_pca_score - min(EM_pca_score)) / (max(EM_pca_score) - min(EM_pca_score)),
                      TQ_pca_score_pnorm = (TQ_pca_score - min(TQ_pca_score)) / (max(TQ_pca_score) - min(TQ_pca_score)),
                      UB_pca_score_pnorm = (UB_pca_score - min(UB_pca_score)) / (max(UB_pca_score) - min(UB_pca_score)),
                      LE_pca_score_pnorm = (LE_pca_score - min(LE_pca_score)) / (max(LE_pca_score) - min(LE_pca_score))) %>%
              dplyr::select(ends_with('pnorm')) -> DScore_pca_pnorm

ggplot(tbl_df(DScore_pca_pnorm), aes(x=CP_pca_score_pnorm)) + geom_density()
ggplot(tbl_df(DScore_pca_pnorm), aes(x=EM_pca_score_pnorm)) + geom_density()
ggplot(tbl_df(DScore_pca_pnorm), aes(x=TQ_pca_score_pnorm)) + geom_density()
ggplot(tbl_df(DScore_pca_pnorm), aes(x=UB_pca_score_pnorm)) + geom_density()
ggplot(tbl_df(DScore_pca_pnorm), aes(x=LE_pca_score_pnorm)) + geom_density()

ggplot(tbl_df(DScore_pca), aes(x=CP_pca_score)) + geom_density()
ggplot(tbl_df(DScore_pca.scaled), aes(x=CP_pca_score)) + geom_density()
ggplot(tbl_df(DScore_pca_pnorm), aes(x=CP_pca_score_pnorm)) + geom_density()

DCL_eigs <- nscumcomp(x = DtCL_complete, nneg = TRUE)
DEM_eigs <- nscumcomp(x = DtEM_complete, nneg = TRUE)
DLE_eigs <- nscumcomp(x = DtLE_complete, nneg = TRUE)
DTQ_eigs <- nscumcomp(x = DtTQ_complete, nneg = TRUE)
DUB_eigs <- nscumcomp(x = DtUB_complete, nneg = TRUE)

DScore_old <- bind_cols(tbl_df(tbl_df(as.matrix(DtCL_complete) %*% DCL_eigs$rotation) %>% rowSums())
                    ,tbl_df(tbl_df(as.matrix(DtEM_complete) %*% DEM_eigs$rotation) %>% rowSums())
                    ,tbl_df(tbl_df(as.matrix(DtLE_complete) %*% DLE_eigs$rotation) %>% rowSums())
                    ,tbl_df(tbl_df(as.matrix(DtTQ_complete) %*% DTQ_eigs$rotation) %>% rowSums())
                    ,tbl_df(tbl_df(as.matrix(DtUB_complete) %*% DUB_eigs$rotation) %>% rowSums()))
(DScore_old.rank <- floor(tbl_df(t(apply(DScore_old, MARGIN = 1, FUN = function(x) rank(x))))))
colnames(DScore_old) <- c("Score_CL", "Score_EM", "Score_LE", "Score_TQ", "Score_UB")
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
nb_k <- 5
kmeans.res <- eclust(tbl_df(DScore_pca.rank), FUNcluster = "kmeans", k = nb_k, graph = F
                     ,verbose = T
                     ,stand = F # apply scale() to DScore
)

colorpalette <- RColorBrewer::brewer.pal(kmeans.res$nbclust, "Set1")
colorpalette.adjust <- as.vector(sapply(colorpalette, function(x) adjustcolor(x,0.5), simplify = T))

fviz_silhouette(cluster::silhouette(kmeans.res$cluster, dist(as.matrix(DScore_pca.rank)))) + scale_color_manual(values = colorpalette.adjust)

kmeans.res$data
table(kmeans.res$cluster)
fviz_cluster(kmeans.res, geom = "point", frame.type = "convex", show.clust.cent = T,
             title = paste("KMeans Clustering K = ", nb_k, sep = '')
)
nrow(kmeans.res$data)
nrow(DtID_complete)
Drate_cluster <- bind_cols(tbl_df(kmeans.res$data), tbl_df(kmeans.res$cluster))
colnames(Drate_cluster)[6] <- 'cluster'
colnames(Drate_cluster)

nc <- 4
Drate_cluster %>% filter(cluster == nc) -> C1
median(C1$`UB Rating`)
kc <- tbl_df(kmeans.res$centers)
kc$CP_pca_score <- rescale(kc$CP_pca_score)$t.t.scale.x.....sd...mean.
kc$EM_pca_score <- rescale(kc$EM_pca_score)$t.t.scale.x.....sd...mean.
kc$LE_pca_score <- rescale(kc$LE_pca_score)$t.t.scale.x.....sd...mean.
kc$TQ_pca_score <- rescale(kc$TQ_pca_score)$t.t.scale.x.....sd...mean.
kc$UB_pca_score <- rescale(kc$UB_pca_score)$t.t.scale.x.....sd...mean.

kc$`CL Rating` <- kc$`CL Rating` + (max(kc$`CL Rating`) - min(kc$`CL Rating`))
kc$`EM Rating` <- kc$`EM Rating` + (max(kc$`EM Rating`) - min(kc$`EM Rating`))
kc$`LE Rating` <- kc$`LE Rating` + (max(kc$`LE Rating`) - min(kc$`LE Rating`))
kc$`TQ Rating` <- kc$`TQ Rating` + (max(kc$`TQ Rating`) - min(kc$`TQ Rating`))
kc$`UB Rating` <- kc$`UB Rating` + (max(kc$`UB Rating`) - min(kc$`UB Rating`))

kc %>%
  rownames_to_column( var = "profile") -> kmeans_radar

kc %>%
  rownames_to_column( var = "profile" ) %>%
  mutate_each(funs(rescale), -profile)  -> kmeans_radar


colnames(kmeans_radar)[-1] <- c("Cost of Living", "Employment"
                                ,"Lifestyle", "Teaching Quality", "University Brand")
colorpalette <- RColorBrewer::brewer.pal(nrow(kmeans_radar), "Set1")
str(kmeans_radar)
ggradar(kmeans_radar, axis.label.size = 2.5) + ggtitle("Radar Chart - Profile comparisons") + theme(text = element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom") + scale_colour_manual(values = colorpalette)

for (pr in 1:nrow(kmeans_radar)) {
  paste(pr)
  print(ggradar(kmeans_radar[pr,], axis.label.size = 2.5) + scale_colour_manual(values = colorpalette[pr]) + ggtitle  ("Radar Chart - Profile comparisons") + theme(text =       element_text(size = 10), legend.text = element_text(size = 7), legend.position = "bottom"))
}

rm(list = setdiff(ls(), "DScore"))
gc()
library(NbClust)

