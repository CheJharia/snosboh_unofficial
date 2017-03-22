library(VIM)

png(filename = "NAsPatternEq.png",
    type = "cairo",
    units = "in",
    width = 12,
    height = 12,
    pointsize = 10,
    res = 1500)
df <- Dtrain_EnquiryManagement
df[df == 0] <- NA
miceplot1 <- aggr(df, col = c("red","blue"), bars = TRUE,
                  numbers = TRUE, combined = TRUE, varheight = FALSE, border = NA,
                  sortVars = TRUE, sortCombs = FALSE, ylabs = c("Missing Data Pattern"),
                  labels = paste("V",1:length(colnames(df)),sep = ''), cex.axis = .7
                  )
dev.off()

png(filename = "NAsPatternAdj.png",
    type = "cairo",
    units ="in",
    width = 12,
    height = 12,
    pointsize = 10,
    res = 300)
paste("V",1:length(colnames(df)),sep = '')
miceplot2 <- aggr(df, col = c("dodgerblue","dimgray"),
                  numbers = TRUE, combined=TRUE, varheight=TRUE, border=NA,
                  sortVars = TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern w/ Height Adjustment"),
                  labels = paste("V",1:length(colnames(df)),sep = ''), cex.axis=.7)
dev.off()
