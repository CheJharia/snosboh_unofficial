RATING <- 20

clusterDataScaledRated <- tbl_df(clusterDataScaled) %>%
  dplyr::mutate(`CL percentage rank` = percent_rank(`CLT`),
                `EM percentage rank` = percent_rank(`EMT`),
                `LE percentage rank` = percent_rank(`LET`),
                `TQ percentage rank` = percent_rank(`TQT`),
                `UB percentage rank` = percent_rank(`UBT`),
                `CL Rating` = ntile(`CL percentage rank`,RATING),
                `EM Rating` = ntile(`EM percentage rank`,RATING),
                `LE Rating` = ntile(`LE percentage rank`,RATING),
                `TQ Rating` = ntile(`TQ percentage rank`,RATING),
                `UB Rating` = ntile(`UB percentage rank`,RATING))

profiledRated<- cbind(rawScores[,1],clusterDataScaledRated)
write.csv(x = profiledRated
          ,file = 'apac_custom_apac_rated.csv'
          ,row.names = F)

