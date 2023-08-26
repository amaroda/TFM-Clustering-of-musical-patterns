#Op. 99i

# Beat percentages

crotchets38ii <- crotchets38ii[-c(221,663,854,1075,1296,1517,1738,1959,2180,2401,2813,3037,3258,3699,3920,4141,4362),]

fviz_nbclust(scale(crotchets38ii[,2:4]),kmeans,method="silhouette") 
fviz_nbclust(scale(crotchets38ii[,2:4]),kmeans,method="wss")

m <- matrix(,nrow = 550, ncol=3)
n <- matrix(,nrow = 550, ncol=4345)
a <- 0
for (k in 2:9){
  for (i in 1:50){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(crotchets38ii[,2:4]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets38ii[,2:4])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-p[,1]
which.max(u)

crotchets38ii$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets38ii[,2:4], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=crotchets38ii)


pattern1b<-crotchets38ii[crotchets38ii$cluster.seq == 1,c(1:4,8)]
pattern2b<-crotchets38ii[crotchets38ii$cluster.seq == 2,c(1:4,8)]
pattern3b<-crotchets38ii[crotchets38ii$cluster.seq == 3,c(1:4,8)]
pattern4b<-crotchets38ii[crotchets38ii$cluster.seq == 4,c(1:4,8)]

pattern5b<-crotchets38ii[crotchets38ii$cluster.seq == 5,c(1:4,8)]
pattern6b<-crotchets38ii[crotchets38ii$cluster.seq == 6,c(1:4,8)]
pattern7b<-crotchets38ii[crotchets38ii$cluster.seq == 7,c(1:4,8)]
pattern8b<-crotchets38ii[crotchets38ii$cluster.seq == 8,c(1:4,8)]


colMeans(pattern1b[,2:4]) # longer 1st
table(pattern1b$performer)/as.vector(table(crotchets38ii$performer))

colMeans(pattern2b[,2:4]) # longer 3rd
table(pattern2b$performer)/as.vector(table(crotchets38ii$performer))

colMeans(pattern3b[,2:4]) # longer 2nd
table(pattern3b$performer)/as.vector(table(crotchets38ii$performer))

colMeans(pattern4b[,2:4]) # average bars
table(pattern4b$performer)/as.vector(table(crotchets38ii$performer))

colMeans(crotchets38ii[,2:4])

colMeans(pattern5b[,2:4])
table(pattern5b$performer)/as.vector(table(crotchets38ii$performer))
colMeans(pattern6b[,2:4])
colMeans(pattern7b[,2:4])
colMeans(pattern8b[,2:4])

matrixb<-matrix(,nrow = 15,ncol=7)

matrixb[,4]<-table(pattern1b$performer)/as.vector(table(crotchets38ii$performer))
matrixb[,3]<-table(pattern2b$performer)/as.vector(table(crotchets38ii$performer))
matrixb[,2]<-table(pattern3b$performer)/as.vector(table(crotchets38ii$performer))
matrixb[,1]<-table(pattern4b$performer)/as.vector(table(crotchets38ii$performer))

matrixb[,5]<-table(pattern5b$performer)/as.vector(table(crotchets38ii$performer))
matrixb[,6]<-table(pattern6b$performer)/as.vector(table(crotchets38ii$performer))
matrixb[,7]<-table(pattern7b$performer)/as.vector(table(crotchets38ii$performer))


rownames(matrixb)<-c("CH","DPB" ,"FB"  ,"GG"  ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")
dist<-dist(matrixb)
hc<-hclust(dist, method="ward.D2")
plot(hc)



# Intensity variations

fviz_nbclust(scale(crotchets38ii[,5:7]),kmeans,method="silhouette", k.max = 16) 
fviz_nbclust(scale(crotchets38ii[,5:7]),kmeans,method="wss")

m <- matrix(,nrow = 750, ncol=3)
n <- matrix(,nrow = 750, ncol=4345)
a <- 0
for (k in 16){
  for (i in 1:50){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(crotchets38ii[,5:7]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets38ii[,5:7])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-0.75*p[,1]
which.max(u)

crotchets38ii$cluster.seqi8 <- as.factor(n[313,])
pca.r<-prcomp(crotchets38ii[,5:7], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi8',data=crotchets38ii)

pattern1i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 1,c(1,5:8)]
pattern2i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 2,c(1,5:8)]
pattern3i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 3,c(1,5:8)]

pattern4i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 4,c(1,5:8)]
pattern5i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 5,c(1,5:8)]
pattern6i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 6,c(1,5:8)]
pattern7i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 7,c(1,5:8)]

pattern8i<-crotchets38ii[crotchets38ii$cluster.seqi8 == 8,c(1,5:8)]

table(pattern1i$performer)/as.vector(table(crotchets38ii$performer))
table(pattern2i$performer)/as.vector(table(crotchets38ii$performer))
table(pattern3i$performer)/as.vector(table(crotchets38ii$performer))


colMeans(pattern1i[,2:4]) 
colMeans(pattern2i[,2:4]) 
colMeans(pattern3i[,2:4]) 
colMeans(pattern4i[,2:4]) 
colMeans(pattern5i[,2:4]) 

colMeans(pattern6i[,2:4]) 
colMeans(pattern7i[,2:4]) 
colMeans(pattern8i[,2:4]) 

colMeans(crotchets38ii[,5:7])

matrixi<-matrix(,nrow = 20,ncol=3)


matrixi[,1]<-table(pattern2i$performer)/as.vector(table(crotchets38ii$performer))
matrixi[,2]<-table(pattern3i$performer)/as.vector(table(crotchets38ii$performer))
matrixi[,3]<-table(pattern1i$performer)/as.vector(table(crotchets38ii$performer))


rownames(matrixi)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
dist<-dist(matrixi)
hc<-hclust(dist, method="ward.D2")
plot(hc)
