library("clusterSim")
data("data_ratio")
data("data_binary")
data("data_interval")
data("data_mixed")
cluster.Sim(x = clusterData
            ,p = 2
            ,minClusterNo = 2
            ,maxClusterNo = 7
            ,icq = "G1"
            ,outputHtml = "clusterSim.html"
            ,outputCsv = "clusterSim.csv")
