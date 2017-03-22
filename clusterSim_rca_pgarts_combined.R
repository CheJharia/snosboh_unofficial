combined_data <- tbl_df(read.csv(file = "rca_iss_pgarts_combined.csv"
                               ,header = T
                               ,check.names = F))

clusterData <- combined_data[,-c(1,2)]
str(clusterData)
library(clusterSim)

cluster.Sim(as.data.frame(clusterData), 2, 2, 10, "S",
            outputCsv="cclusterSim_S.csv"
            ,outputHtml = "cclusterSim_S.html"
)

cluster.Sim(as.data.frame(clusterData), 2, 2, 10, "G1",
            outputCsv="cclusterSim_G1.cs"
            ,outputHtml = "cclusterSim_G1.html"
)

#cluster.Sim(as.data.frame(clusterData), 2, 2, 10, "G2",
#            outputCsv="clusterSim_G2.csv"
#            ,outputHtml = "clusterSim_G2.html"
#)

cluster.Sim(as.data.frame(clusterData), 2, 2, 10, "G3",
            outputCsv="cclusterSim_G3.csv"
            ,outputHtml = "cclusterSim_G3.html"
)

cluster.Sim(as.data.frame(clusterData), 2, 2, 10, "KL",
            outputCsv="cclusterSim_KL.csv"
            ,outputHtml = "cclusterSim_KL.html"
)
