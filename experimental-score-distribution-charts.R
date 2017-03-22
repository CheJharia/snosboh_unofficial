
Dscore_p <- bind_cols(DScore, tbl_df(kmeans.res$cluster))
colnames(Dscore_p)[6] <- "cluster"
Dscore_p$cluster <- as.factor(Dscore_p$cluster)
ggplot(Dscore_p, aes(Score_CL / max(Score_CL) * 20, ..count.., colour = cluster, fill = cluster)) +
  geom_density(position = "fill") + scale_colour_manual(values = colorpalette) + scale_fill_manual(values = colorpalette) + labs(x = "Cost of Living"                                                                               , title = "Score Distribution")

ggplot(Dscore_p, aes(Score_EM / max(Score_EM) * 20, ..density.., colour = cluster, fill = cluster)) +
  geom_density(position = "fill") + scale_colour_manual(values = colorpalette) + scale_fill_manual(values = colorpalette) + labs(x = "Employment")

ggplot(Dscore_p, aes(Score_LE / max(Score_LE) * 20, ..density.., colour = cluster, fill = cluster)) +
  geom_density(position = "fill") + scale_colour_manual(values = colorpalette) + scale_fill_manual(values = colorpalette) + labs(x = "Lifestyle")

ggplot(Dscore_p, aes(Score_TQ / max(Score_TQ) * 20, ..density.., colour = cluster, fill = cluster)) +
  geom_density(position = "fill") + scale_colour_manual(values = colorpalette) + scale_fill_manual(values = colorpalette) + labs(x = "Teaching Quality")

ggplot(Dscore_p, aes(Score_UB / max(Score_UB) * 20, ..density.., colour = cluster, fill = cluster)) +
  geom_density(position = "fill") + scale_colour_manual(values = colorpalette) + scale_fill_manual(values = colorpalette) + labs(x = "University Brand")

ggplot(Dscore_p, aes(Score_UB / max(Score_UB) * 20, ..count.., colour = cluster, fill = cluster)) +
  geom_density(position = "fill") + scale_colour_manual(values = colorpalette) + scale_fill_manual(values = colorpalette) + labs(x = "University Brand")
