# Dynamic Patterns of FvdP in 38ii

fvdp<-quavers38ii[quavers38ii$performer == "FvdP",8:13]

kmeans(scale(fvdp),2)


fviz_nbclust(scale(fvdp),kmeans,method="silhouette", k.max = 16) 
m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=190)
a <- 0
for (k in 2:12){
  for (i in 1:50){
    a <- kmeans(scale(fvdp), centers = k,iter.max = 5000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(fvdp)))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-p[,1]
which.max(u)

fvdp$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(fvdp, center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=fvdp)


pat1<-fvdp[fvdp$cluster.seqb == 1,]
pat2<-fvdp[fvdp$cluster.seqb == 2,]
pat3<-fvdp[fvdp$cluster.seqb == 3,]
pat4<-fvdp[fvdp$cluster.seqb == 4,]

pat5<-fvdp[fvdp$cluster.seqb == 5,]
pat6<-fvdp[fvdp$cluster.seqb == 6,]
pat7<-fvdp[fvdp$cluster.seqb == 7,]
pat8<-fvdp[fvdp$cluster.seqb == 8,]

pat9<-fvdp[fvdp$cluster.seqb == 9,]
pat10<-fvdp[fvdp$cluster.seqb == 10,]
pat11<-fvdp[fvdp$cluster.seqb == 11,]


