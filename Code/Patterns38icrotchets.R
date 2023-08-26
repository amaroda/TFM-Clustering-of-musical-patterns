# Op.38i

# Beat percentages

# Choosing and creating the clusters
fviz_nbclust(scale(crotchets38i[,2:5]),kmeans,method="silhouette", k.max =  16) 
fviz_nbclust(scale(crotchets38i[,2:5]),kmeans,method="wss")

m <- matrix(,nrow = 750, ncol=3)
n <- matrix(,nrow = 750, ncol=6680)
a <- 0
for (k in 2:16){
  for (i in 1:50){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(crotchets38i[,2:5]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets38i[,2:5])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-p[,1]
which.max(u)

# Plotting clusters in a PCA plot
crotchets38i$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets38i[,2:5], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=crotchets38i)

# Separating patterns into dataframes
pattern1b<-crotchets38i[crotchets38i$cluster.seqb == 1,c(1:5,10)]
pattern2b<-crotchets38i[crotchets38i$cluster.seqb == 2,c(1:5,10)]
pattern3b<-crotchets38i[crotchets38i$cluster.seqb == 3,c(1:5,10)]
pattern4b<-crotchets38i[crotchets38i$cluster.seqb == 4,c(1:5,10)]

colMeans(pattern1b[,2:5]) # longer 4th beat
table(pattern1b$performer)/as.vector(table(crotchets38i$performer))

colMeans(pattern2b[,2:5]) # longer 3rd beat
table(pattern2b$performer)/as.vector(table(crotchets38i$performer))

colMeans(pattern3b[,2:5]) # longer 2nd beat
table(pattern3b$performer)/as.vector(table(crotchets38i$performer))

colMeans(pattern4b[,2:5]) # longer 1st beat
table(pattern4b$performer)/as.vector(table(crotchets38i$performer))

colMeans(crotchets38i[,2:5])

matrixb<-matrix(,nrow = 20,ncol=4)

matrixb[,4]<-table(pattern1b$performer)/as.vector(table(crotchets38i$performer))
matrixb[,3]<-table(pattern2b$performer)/as.vector(table(crotchets38i$performer))
matrixb[,2]<-table(pattern3b$performer)/as.vector(table(crotchets38i$performer))
matrixb[,1]<-table(pattern4b$performer)/as.vector(table(crotchets38i$performer))


rownames(matrixb)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
dist<-dist(matrixb)
hc<-hclust(dist, method="ward.D2")
plot(hc)



# Intensity variations

# Choosing the clusters
fviz_nbclust(scale(crotchets38i[,6:9]),kmeans,method="silhouette", k.max = 16) 
fviz_nbclust(scale(crotchets38i[,6:9]),kmeans,method="wss")

m <- matrix(,nrow = 350, ncol=3)
n <- matrix(,nrow = 350, ncol=6680)
a <- 0
for (k in 2:8){
  for (i in 1:50){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(crotchets38i[,6:9]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets38i[,6:9])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-0.75*p[,1]
which.max(u)


# Plotting the clusters in a PCA plot
crotchets38i$cluster.seqi <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets38i[,6:9], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seq2i',data=crotchets38i)

# Separating the patterns into dataframes
pattern1i<-crotchets38i[crotchets38i$cluster.seq2i == 1,c(1,6:9,10)]
pattern2i<-crotchets38i[crotchets38i$cluster.seq2i == 2,c(1,6:9,10)]

table(pattern1i$performer)/as.vector(table(crotchets38i$performer))
table(pattern2i$performer)/as.vector(table(crotchets38i$performer))

# Centers of the clusters
colMeans(pattern1i[,2:5]) 
colMeans(pattern2i[,2:5]) 




