# Scoring
library(FactoMineR)
library(factoextra)
library(rARPACK)

glimpse(DtCL)
DtCL
scale(DtCL)
cov(scale(DtCL))
tt <- scale(DtCL)[,]
ttt <- cov(tt)
## If retvec == FALSE, we don't calculate eigenvectors
DCL_eigs <- eigs_sym(cov(scale(DtCL_complete)), k = ncol(DtCL_complete), which = "LM", opts = list(retvec = TRUE))
DEM_eigs <- eigs_sym(cov(scale(DtEM_complete)), k = ncol(DtEM_complete), which = "LM", opts = list(retvec = TRUE))
DLE_eigs <- eigs_sym(cov(scale(DtLE_complete)), k = ncol(DtLE_complete), which = "LM", opts = list(retvec = TRUE))
DTQ_eigs <- eigs_sym(cov(scale(DtTQ_complete)), k = ncol(DtTQ_complete), which = "LM", opts = list(retvec = TRUE))
DUB_eigs <- eigs_sym(cov(scale(DtUB_complete)), k = ncol(DtUB_complete), which = "LM", opts = list(retvec = TRUE))
DScore <- dplyr::tbl_df(cbind(DtCL_complete
                              ,abs(tbl_df(as.matrix(DtCL_complete) %*% DCL_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_CL = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_CL)
                              ,DtEM_complete
                              ,abs(tbl_df(as.matrix(DtEM_complete) %*% DEM_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_EM = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_EM)
                              ,DtLE_complete
                              ,abs(tbl_df(as.matrix(DtLE_complete) %*% DLE_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_LE = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_LE)
                              ,DtTQ_complete
                              ,abs(tbl_df(as.matrix(DtTQ_complete) %*% DTQ_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_TQ = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_TQ)
                              ,DtUB_complete
                              ,abs(tbl_df(as.matrix(DtUB_complete) %*% DUB_eigs$vectors)) %>%
                                select(V1) %>%
                                mutate(Score_UB = abs(V1)/max(abs(V1))*20) %>%
                                select(Score_UB)))
write.csv(x = DScore
          ,file = 'scoreFinal.csv'
          ,row.names = F)
Dcluster <- DScore %>% select(starts_with("Score_"))
Dcluster_scaled <- dplyr::tbl_df(round(scale(Dcluster)[,],5))
str(Dcluster_scaled)
summary(Dcluster_scaled)
