library(tidyverse)
library(nsprcomp)


Draw <- readr::read_csv(file = '20161227042619-SurveyExport.csv'
                        ,progress = T)

colnames(Draw)

Draw.intprospect <- Draw %>% dplyr::filter(`Are you studying overseas or planning to study overseas?` == "Yes - I am planning to study overseas" & Status == 'Complete' )


dplyr::tbl_df(funModeling::df_status(data = Draw.intprospect ,print_results = F)) %>%
  mutate(var_original_pos = row_number()) %>%
  arrange(desc(p_na)) -> Dvar.status

Dvar.status %>% filter(variable %in% c('Response ID', 'Longitude','Latitude','Country','City','Link Name'
                                       ,'What nationality are you?'
                                       ,'What gender are you?'
                                       ,'Field:What are you planning to study?'
                                       ,'Subject:What are you planning to study?'
                                       ,'At what level are you currently planning to study?'))

Draw.intprospect %>% dplyr::select(dplyr::contains("What five things are most important")) %>%
  dplyr::select(-dplyr::contains("recommended")) -> Draw.profiling

c(paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(1,2),5), sep = ''),
  paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(3,4),5), sep = ''),
  paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(5,6),5), sep = ''),
  paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c(7,8),5), sep = '')) -> colnames(Draw.profiling)


# summary(sapply(Draw.profiling, function(x) as.factor(x)))
tbl_df(sapply(Draw.profiling, function(x) plyr::mapvalues(x, c(NA,seq(1:5)), c(0,seq(from = 5, to = 1, by = -1))))) -> Draw.profiling.score

Draw.profiling.score %>%
  mutate_each_(funs(factor(.)),colnames(Draw.profiling.score)) -> Draw.profiling.score.factor
summary(Draw.profiling.score.factor)
summary(Draw.profiling.score)

# Burt table
tbl_df(amap::burt(Draw.profiling.score.factor)) -> Draw.profiling.score.factor.burt

# Disjonctif table
ade4::acm.disjonctif(as.matrix(Draw.profiling.score.factor)) -> Draw.profiling.score.factor.disj
colnames(Draw.profiling.score.factor.disj) <- colnames(Draw.profiling.score.factor.burt)

# Basic Sum Score scheme
bind_cols(tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("CP")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("TQ")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("GO")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("LE")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("BR")) %>% rowSums(.))) -> DScore.rawSum
colnames(DScore.rawSum) <- c('CP.Score.rawSum','TQ.Score.rawSum','GO.Score.rawSum','LE.Score.rawSum','BR.Score.rawSum')

# preference separation quality measure
Dpref.sep.quality <- c()
for(i in 1:nrow(DScore.rawSum)){
  Dpref.sep.quality <- c(Dpref.sep.quality, sum(dist(t(DScore.rawSum[i,]))))
}
Dpref.sep.quality <- tbl_df(Dpref.sep.quality)
colnames(Dpref.sep.quality) <- 'Pref.Separation.Quality'
dist(t(DScore.rawSum[i,]))
