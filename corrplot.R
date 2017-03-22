bh <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(bh) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
pacman::p_load(corrplot)
corrplot(cor(bh))
colnames(DtThemes_complete)

DtCL_complete
DtEM_complete
DtLE_complete
DtTQ_complete
DtUB_complete


dd <- DtLE_complete
NC <- ncol(dd)
colnames(dd) <- paste('V',seq(1:NC), sep = '')
corrplot(cor(dd))

ggplot(DScore_old, aes(x=Score_CL)) + geom_density()
