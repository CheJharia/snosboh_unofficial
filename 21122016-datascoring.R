
# Basic Sum Score scheme
bind_cols(tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("CP")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("TQ")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("GO")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("LE")) %>% rowSums(.)), tbl_df(Draw.profiling.score %>% dplyr::select(starts_with("BR")) %>% rowSums(.))) -> DScore.rawSum
colnames(DScore.rawSum) <- c('CP.Score.rawSum','TQ.Score.rawSum','GO.Score.rawSum','LE.Score.rawSum','BR.Score.rawSum')

tbl_df(matrix(rescale(as.vector(as.matrix(DScore.rawSum)), c(0,36)), ncol = 5, byrow =F)) %>% rowSums()
tbl_df(t(apply(DScore.rawSum, MARGIN = 1, FUN = function(x) rank(x))))
rescale(DScore.rawSum, c(0,360))
DScore.rawSum.rowScaled <- t(apply(X = DScore.rawSum, MARGIN = 1, FUN = function(x) rescale(x,c(min(x),max(x)))))
DScore.rawSum.rowScaled <- tbl_df(scrime::rowScales(X = DScore.rawSum))
DScore.rawSum
DScore.rawSum.rowScaled <- t(apply(X = DScore.rawSum.rowScaled, MARGIN = 1, FUN = function(x) rescale(x,c(min(x),36))))

DScore.rawSum %>% rowSums()

# Rank scheme
(DScore.rank <- floor(tbl_df(t(apply(DScore.rawSum, MARGIN = 1, FUN = function(x) rank(-x))))))
colnames(DScore.rank) <- c('CP.rank', 'TQ.rank', 'GO.rank', 'LE.rank', 'BR.rank')

# Inverse Rank scheme
(DScore.rank.inverse <- floor(tbl_df(t(apply(DScore.rawSum, MARGIN = 1, FUN = function(x) rank(x))))))
colnames(DScore.rank.inverse) <- c('CP.rank.inverse', 'TQ.rank.inverse', 'GO.rank.inverse', 'LE.rank.inverse', 'BR.rank.inverse')

# Burt table
tbl_df(amap::burt(DScore.rank.inverse)) -> DScore.rank.inverse.burt

# Disjonctif table
ade4::acm.disjonctif(as.matrix(DScore.rank.inverse)) -> DScore.rank.inverse.disj
colnames(DScore.rank.inverse.disj) <- colnames(DScore.rank.inverse.burt)

# define high as rank more than 3
lapply(DScore.rank.inverse, function(x) x > 3)
# Define rank higher than 3 as High for all columns, and convert the list into a dataframe
DScore.rank.inverse.high <- tbl_df(data.frame(matrix(unlist(lapply(DScore.rank.inverse, function(x) x > 3)), nrow=nrow(DScore.rank.inverse), byrow=F)) * 1)
colnames(DScore.rank.inverse.high) <- colnames(DScore.rank.inverse)
tbl_df(amap::burt(DScore.rank.inverse.high)) -> DScore.rank.inverse.high.burt
ade4::acm.disjonctif(as.matrix(DScore.rank.inverse.high)) -> DScore.rank.inverse.high.disj
colnames(DScore.rank.inverse.high.disj) <- paste(rep(c('CP','TQ','GO','LE','BR'), each = 2), rep(c('.Low','.High'),5), sep = '')

write.csv(bind_cols(Draw.profiling.score, DScore.rank.inverse, tbl_df(vol.cl@cluster)), file = 'bb.csv')


