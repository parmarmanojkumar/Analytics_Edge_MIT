rm(list = ls())
airlines = read.csv("AirlinesCluster.csv")
summary(airlines)
sort(sapply(airlines, mean))
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc,airlines)
summary(airlinesNorm)
sort(sapply(airlinesNorm, max))
sort(sapply(airlinesNorm, min))
distances = dist(airlinesNorm, method="euclidean")
hierClust = hclust(distances, method="ward.D")
plot(hierClust)
clusterGroups = cutree(hierClust, k = 5)
HierCluster = split(airlines, clusterGroups)
HierCluster1 = HierCluster[[1]]
HierCluster2 = HierCluster[[2]]
HierCluster3 = HierCluster[[3]]
HierCluster4 = HierCluster[[4]]
HierCluster5 = HierCluster[[5]]
(colMeans(HierCluster1))
(colMeans(HierCluster2))
(colMeans(HierCluster3))
(colMeans(HierCluster4))
(colMeans(HierCluster5))

#problem 3
set.seed(88)
k = 5
airlinesKMC = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(airlinesKMC)
airlinesKMC$centers
