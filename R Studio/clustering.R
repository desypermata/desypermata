library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library("readxl")
library(ggplot2)
library(NbClust)
library(dplyr)
library(psych)
library(clusterSim)
library(ClusterR)
library(cluster)
library("writexl")


# VARIAN DELTA
delta <- read_excel("E:\\...\\delta.xlsx")
head(delta)

# 1. memisahkan parameter
df<-na.omit(delta)
x<-df[-1]

sd<-scale(x)
View(sd)

# 2. memilih jumlah cluster
NbClust(sd, distance="euclidean", min.nc=2, max.nc=10, method="ward.D2", index="hubert")
NbClust(sd, distance="euclidean", min.nc=2, max.nc=10, method="ward.D2", index="dindex")

NbClust(sd, distance="euclidean", min.nc=2, max.nc=10, method="complete", index="alllong")



#FCM Delta

proses.fcm <- fanny(sd, 3, metric = "euclidean", stand = FALSE)
proses.fcm

df<-cbind(delta, cluster_delta = proses.fcm$cluster)
View(df)

fviz_cluster(proses.fcm, data = sd)+ggtitle("Fuzzy C Means Clustering")

#metode silhouette
fm.sil<-silhouette(proses.fcm$cluster, dist(sd))
fviz_silhouette(fm.sil)

#metode Davies Bouldin
clu2 <- pam(sd, 3)
print(index.DB(sd, proses.fcm$clustering, centrotypes="centroids"))

d <-dist(sd)
dbi_fcm <-print(index.DB(sd, proses.fcm$cluster, d, centrotypes = 'centroids'))



#kmeans Delta
km <- eclust(sd,FUNcluster="kmeans", k=3,hc_metric = "euclidean")
km


#metode silhouette
km.sil<-silhouette(km$cluster, dist(x))
fviz_silhouette(km.sil)

#metode Davies Bouldin
d <-dist(sd)
dbi_kmean3 <-print(index.DB(sd, km$cluster, d, centrotypes = 'centroids'))


final<-cbind(delta, cluster_delta = km$cluster)
View(final)

fviz_cluster(km, geom = "point", data = sd)+ggtitle("K-Means Clustering")

km$cluster
cm <- table(delta$kecamatan, km$cluster)
cm

plot(sd[c(2:8)])
plot(sd[c(2:8)], 
     col = km$cluster)
plot(sd[c(2:8)], 
     col = km$cluster, 
     main = "K-means with 3 clusters")

char1 <- x %>%
  mutate(Cluster = km$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
View(char1)



#PAM Delta
pm <- eclust(sd,FUNcluster="pam", k=3,hc_metric = "euclidean")

#metode silhouette
pm.sil<-silhouette(pm$cluster, dist(x))
fviz_silhouette(pm.sil)

#metode Davies Bouldin
d <-dist(sd)
dbi_pam <-print(index.DB(sd, pm$cluster, d, centrotypes = 'centroids'))


#VARIAN OMICRON

omicron <- read_excel("E:\\BACKUP C NEW\\Documents\\JSC\\data\\omicron.xlsx")
head(omicron)

#memisahkan parameter
df1<-na.omit(omicron)
x1<-df1[-1]

sd1<-scale(x1)
View(sd1)


#memilih jumlah cluster
NbClust(sd1, distance="euclidean", min.nc=2, max.nc=10, method="ward.D2", index="hubert")
NbClust(sd1, distance="euclidean", min.nc=2, max.nc=10, method="ward.D2", index="dindex")

NbClust(x1, distance="euclidean", min.nc=2, max.nc=10, method="complete", index="alllong")


#FCM Omicron
proses.fcm1 <- fanny(x1, 3, metric = "euclidean", stand = FALSE)
proses.fcm1

df1<-cbind(omicron, cluster_omicron = proses.fcm1$cluster)
View(df1)

fviz_cluster(proses.fcm1, data = x1)

#metode silhouette
fm1.sil<-silhouette(proses.fcm1$cluster, dist(x1))
fviz_silhouette(fm1.sil)

#metode Davies Bouldin
clu2 <- pam(sd, 3)
print(index.DB(sd, proses.fcm$clustering, centrotypes="centroids"))

d1 <-dist(x1)
dbi_fcm1 <-print(index.DB(x1, proses.fcm1$cluster, d1, centrotypes = 'centroids'))


#kmeans Omicron
km1 <- eclust(x1,FUNcluster="kmeans", k=3,hc_metric = "euclidean")


#metode silhouette
km1.sil<-silhouette(km1$cluster, dist(x1))
fviz_silhouette(km1.sil)

#metode Davies Bouldin
d1 <-dist(x1)
dbi_kmean2 <-print(index.DB(x1, km1$cluster, d1, centrotypes = 'centroids'))

char2 <- x %>%
  mutate(Cluster = km1$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
View(char2)

final1<-cbind(omicron, cluster_omicron = km1$cluster)
View(final1)

fviz_cluster(km1, geom = "point", data = x1)+ggtitle("K-Means Clustering")

km1$cluster
cm1 <- table(delta$kecamatan, km1$cluster)
cm1

plot(sd[c(2:8)])
plot(sd[c(2:8)], 
     col = km1$cluster)
plot(sd[c(2:8)], 
     col = km1$cluster, 
     main = "K-means with 3 clusters")



#PAM Omicron
pm1 <- eclust(x1,FUNcluster="pam", k=3,hc_metric = "euclidean")


#metode silhouette
pm1.sil<-silhouette(pm1$cluster, dist(x1))
fviz_silhouette(pm1.sil)

#metode Davies Bouldin
d1 <-dist(x1)
dbi_pam1 <-print(index.DB(x1, pm$cluster, d, centrotypes = 'centroids'))

#menyimpan output
write_xlsx(final, "E:\\...\\Delta Clust.xlsx")
write_xlsx(final1, "E:\\...\\Omicron Clust.xlsx")
write_xlsx(char, "E:\\...\\Delta Char.xlsx")
write_xlsx(char1, "E:\\...\\Omicron Char.xlsx")

