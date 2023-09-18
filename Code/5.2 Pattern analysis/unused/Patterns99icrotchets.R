
#Op. 99i

# Beat percentages

fviz_nbclust(scale(crotchets99i[,2:4]),kmeans,method="silhouette") 
fviz_nbclust(scale(crotchets99i[,2:4]),kmeans,method="wss")

m <- matrix(,nrow = 350, ncol=3)
n <- matrix(,nrow = 350, ncol=4125)
a <- 0
for (k in 2:8){
  for (i in 1:50){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(crotchets99i[,2:4]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets99i[,2:4])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-p[,1]
which.max(u)

crotchets99i$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets99i[,2:4], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=crotchets99i)


pattern1b<-crotchets99i[crotchets99i$cluster.seqb == 1,c(1:4,8)]
pattern2b<-crotchets99i[crotchets99i$cluster.seqb == 2,c(1:4,8)]
pattern3b<-crotchets99i[crotchets99i$cluster.seqb == 3,c(1:4,8)]
pattern4b<-crotchets99i[crotchets99i$cluster.seqb == 4,c(1:4,8)]

pattern5b<-crotchets99i[crotchets99i$cluster.seqb == 5,c(1:4,8)]
pattern6b<-crotchets99i[crotchets99i$cluster.seqb == 6,c(1:4,8)]
pattern7b<-crotchets99i[crotchets99i$cluster.seqb == 7,c(1:4,8)]


colMeans(pattern1b[,2:4]) # longer 1st
table(pattern1b$performer)/as.vector(table(crotchets99i$performer))

colMeans(pattern2b[,2:4]) # longer 3rd
table(pattern2b$performer)/as.vector(table(crotchets99i$performer))

colMeans(pattern3b[,2:4]) # longer 2nd
table(pattern3b$performer)/as.vector(table(crotchets99i$performer))

colMeans(pattern4b[,2:4]) # average bars
table(pattern4b$performer)/as.vector(table(crotchets99i$performer))

colMeans(crotchets99i[,2:4])

colMeans(pattern5b[,2:4])
table(pattern5b$performer)/as.vector(table(crotchets99i$performer))
colMeans(pattern6b[,2:4])
colMeans(pattern7b[,2:4])

matrixb<-matrix(,nrow = 15,ncol=7)

matrixb[,4]<-table(pattern1b$performer)/as.vector(table(crotchets99i$performer))
matrixb[,3]<-table(pattern2b$performer)/as.vector(table(crotchets99i$performer))
matrixb[,2]<-table(pattern3b$performer)/as.vector(table(crotchets99i$performer))
matrixb[,1]<-table(pattern4b$performer)/as.vector(table(crotchets99i$performer))

matrixb[,5]<-table(pattern5b$performer)/as.vector(table(crotchets99i$performer))
matrixb[,6]<-table(pattern6b$performer)/as.vector(table(crotchets99i$performer))
matrixb[,7]<-table(pattern7b$performer)/as.vector(table(crotchets99i$performer))


rownames(matrixb)<-c("CH","DPB" ,"FB"  ,"GG"  ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")
dist<-dist(matrixb)
hc<-hclust(dist, method="ward.D2")
plot(hc)



# Intensity variations

fviz_nbclust(scale(crotchets99i[,5:7]),kmeans,method="silhouette") 
fviz_nbclust(scale(crotchets99i[,5:7]),kmeans,method="wss")

m <- matrix(,nrow = 700, ncol=3)
n <- matrix(,nrow = 700, ncol=4125)
a <- 0
for (k in 2:16){
  for (i in 1:50){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(crotchets99i[,5:7]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets99i[,5:7])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-p[,1]
which.max(u)

crotchets99i$cluster.seqi7 <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets99i[,5:7], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi7',data=crotchets99i)

pattern1i<-crotchets99i[crotchets99i$cluster.seqi7 == 1,c(1,5:8)]
pattern2i<-crotchets99i[crotchets99i$cluster.seqi7 == 2,c(1,5:8)]
pattern3i<-crotchets99i[crotchets99i$cluster.seqi7 == 3,c(1,5:8)]

pattern4i<-crotchets99i[crotchets99i$cluster.seqi7 == 4,c(1,5:8)]
pattern5i<-crotchets99i[crotchets99i$cluster.seqi7 == 5,c(1,5:8)]
pattern6i<-crotchets99i[crotchets99i$cluster.seqi7 == 6,c(1,5:8)]
pattern7i<-crotchets99i[crotchets99i$cluster.seqi7 == 7,c(1,5:8)]

table(pattern1i$performer)/as.vector(table(crotchets99i$performer))
table(pattern2i$performer)/as.vector(table(crotchets99i$performer))
table(pattern3i$performer)/as.vector(table(crotchets99i$performer))


colMeans(pattern1i[,2:4]) 
colMeans(pattern2i[,2:4]) 
colMeans(pattern3i[,2:4]) 
colMeans(pattern4i[,2:4]) 
colMeans(pattern5i[,2:4]) 

colMeans(pattern6i[,2:4]) 
colMeans(pattern7i[,2:4]) 

colMeans(crotchets99i[,5:7])

matrixi<-matrix(,nrow = 20,ncol=3)


matrixi[,1]<-table(pattern2i$performer)/as.vector(table(crotchets99i$performer))
matrixi[,2]<-table(pattern3i$performer)/as.vector(table(crotchets99i$performer))
matrixi[,3]<-table(pattern1i$performer)/as.vector(table(crotchets99i$performer))


rownames(matrixi)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
dist<-dist(matrixi)
hc<-hclust(dist, method="ward.D2")
plot(hc)



