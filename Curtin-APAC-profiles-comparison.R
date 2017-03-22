library(ggradar)

x = t( data.frame(c("Curtin", 0.227318046,	0.209371884,	0.261216351,	0.129611167,	0.172482552), 
                  c("APAC", 0.218376337,	0.248898678,	0.257709251,	0.1312146,	0.143801133)) )
dimnames(x) = NULL
colnames(x) <- c("Segment", "Future Focused",	"Balance is Imperative",	"Life Planner",	"Prestige Matters",	"The Whole Package")

print(x)
curtin_data <- as.data.frame(x)
curtin_data$`Future Focused` <- as.numeric(levels(curtin_data$`Future Focused`))
curtin_data$`Balance is Imperative` <- as.numeric(levels(curtin_data$`Balance is Imperative`))
curtin_data$`Life Planner` <- as.numeric(levels(curtin_data$`Life Planner`))
curtin_data$`Prestige Matters` <- as.numeric(levels(curtin_data$`Prestige Matters`))
curtin_data$`The Whole Package` <- as.numeric(levels(curtin_data$`The Whole Package`))
ggradar(curtin_data, axis.label.size = 3
        , grid.max = 0.3) + ggtitle("Radar Chart - Curtin vs APAC comparisons") + theme(text = element_text(size = 15), legend.text = element_text(size = 13), legend.position = "bottom") 
?ggradar
