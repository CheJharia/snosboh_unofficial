# read nutrients data
Dnutrients <- readr::read_csv(file = 'nutrients.csv', progress = T)
# set the top nb group
nb <- 5
# get the top nb group
Dnutrients %>% group_by(group) %>% summarise(tot = n()) %>% arrange(desc(tot)) %>% top_n(nb) %>% select(group) -> topNBgroup
Dnutrients %>% filter(group %in% topNBgroup$group) -> Dnb.data
# define colors for the nb group
colorpalette <- RColorBrewer::brewer.pal(nb, "Set1")
colorpalette <- as.vector(sapply(colorpalette, function(x) adjustcolor(x,0.1), simplify = T))
# generate colors for all selected items in the dataset
profile_col <- (colorpalette)[as.numeric(plyr::mapvalues(Dnb.data$group, levels(as.factor(Dnb.data$group)) , 1:length(levels(as.factor(Dnb.data$group)))))]
# plot parcoord
MASS::parcoord(Dnb.data[,-c(1,2,ncol(Dnb.data))], col = profile_col , lwd = 2)

###############################################################################
