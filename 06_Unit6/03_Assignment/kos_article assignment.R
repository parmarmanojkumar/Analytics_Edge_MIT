rm(list = ls())
#problem 1
dailykos = read.csv("dailykos.csv")
str(dailykos)
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")
plot(kosHierClust)
rect.hclust(kosHierClust, k = 7, border = "red")
hierGroups = cutree(kosHierClust, k = 7)
table(hierGroups)
HierCluster1 = subset(dailykos, hierGroups == 1)
HierCluster2 = subset(dailykos, hierGroups == 2)
HierCluster3 = subset(dailykos, hierGroups == 3)
HierCluster4 = subset(dailykos, hierGroups == 4)
HierCluster5 = subset(dailykos, hierGroups == 5)
HierCluster6 = subset(dailykos, hierGroups == 6)
HierCluster7 = subset(dailykos, hierGroups == 7)
HierCluster = split(dailykos, hierGroups)
HierCluster1 = HierCluster[[1]]
HierCluster2 = HierCluster[[2]]
HierCluster3 = HierCluster[[3]]
HierCluster4 = HierCluster[[4]]
HierCluster5 = HierCluster[[5]]
HierCluster6 = HierCluster[[6]]
HierCluster7 = HierCluster[[7]]


#problem2
set.seed(1000)
k = 7
kosKMC = kmeans(dailykos, centers = k)
str(kosKMC)
kosKMC$size

KmeansCluster = split(dailykos, kosKMC$cluster)
KmeansCluster1 = KmeansCluster[[1]]
KmeansCluster2 = KmeansCluster[[2]]
KmeansCluster3 = KmeansCluster[[3]]
KmeansCluster4 = KmeansCluster[[4]]
KmeansCluster5 = KmeansCluster[[5]]
KmeansCluster6 = KmeansCluster[[6]]
KmeansCluster7 = KmeansCluster[[7]]
tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

table(hierGroups, kosKMC$cluster)