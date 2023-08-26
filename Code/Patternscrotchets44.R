crotchets38i$type<-1
crotchets38i$cluster.seqb<-0
crotchets38iii$type<-2

crotchets38<-rbind(crotchets38i,crotchets38iii)

pca.r<-prcomp(crotchets38[,6:9], center=T,scale. = T)
autoplot(pca.r, colour='type',data=crotchets38)

# Choose and create the clusters
fviz_nbclust(scale(crotchets38[,6:9]),kmeans,method="silhouette", k.max = 16) 
fviz_nbclust(scale(crotchets38[,6:9]),kmeans,method="wss")

m <- matrix(,nrow = 750, ncol=3)
n <- matrix(,nrow = 750, ncol=10620)
a <- 0
for (k in 4){
  for (i in 1:50){
    a <- kmeans(scale(crotchets38[,6:9]), centers = k,iter.max = 5000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets38[,6:9])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-1*p[,1]
which.max(u)

crotchets38$cluster.seqi <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets38[,6:9], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi',data=crotchets38)

pattern1i<-crotchets38[crotchets38$cluster.seqi == 1,c(1,6:9)]
pattern2i<-crotchets38[crotchets38$cluster.seqi == 2,c(1,6:9)]

colMeans(pattern1i[,2:5]) 
colMeans(pattern2i[,2:5]) 


fviz_nbclust(scale(crotchets38[,2:5]),kmeans,method="silhouette", k.max = 16) 

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=10620)
a <- 0
for (k in 4){
  for (i in 1:100){
    a <- kmeans(scale(crotchets38[,2:5]), centers = k,iter.max = 5000)
    m[i+(k-2)*100,1] <- a$tot.withinss
    n[i+(k-2)*100,] <- a$cluster
    m[i+(k-2)*100,2] <- mean(silhouette(as.vector(n[i+(k-2)*100,]),dist(scale(crotchets38[,2:5])))[,3])
    m[i+(k-2)*100,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-1*p[,1]
which.max(u)

crotchets38$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets38[,2:5], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=crotchets38)

pattern1b<-crotchets38[crotchets38$cluster.seqb == 1,c(1:5,10)]
pattern2b<-crotchets38[crotchets38$cluster.seqb == 2,c(1:5,10)]
pattern3b<-crotchets38[crotchets38$cluster.seqb == 3,c(1:5,10)]
pattern4b<-crotchets38[crotchets38$cluster.seqb == 4,c(1:5,10)]


colMeans(pattern1b[,2:5])
colMeans(pattern2b[,2:5]) 
colMeans(pattern3b[,2:5]) 
colMeans(pattern4b[,2:5])

colMeans(crotchets38[,2:5])

# Create plots of the patterns
par(mfrow=c(2,2))

plot(1:4,as.vector(colMeans(pattern1b[,2:5])), ylim = c(0.22,0.32), 
     ylab = " ", xlab = paste0("L4, " ,dim(pattern1b)[1] , " bars" ))
lines(1:4,as.vector(colMeans(pattern1b[,2:5])))
abline(h=1/4, col="cadetblue3")

plot(1:4,as.vector(colMeans(pattern2b[,2:5])), ylim = c(0.22,0.32),
     ylab = " ", xlab = paste0("L6, " ,dim(pattern2b)[1] , " bars" ) )
lines(1:4,as.vector(colMeans(pattern2b[,2:5])))
abline(h=1/4, col="cadetblue3")

plot(1:4,as.vector(colMeans(pattern3b[,2:5])),  ylim = c(0.22,0.32),
     ylab = " ", xlab = paste0("sL4, " ,dim(pattern3b)[1] , " bars" ) )
lines(1:4,as.vector(colMeans(pattern3b[,2:5])))
abline(h=1/4, col="cadetblue3")

plot(1:4,as.vector(colMeans(pattern4b[,2:5])),  ylim = c(0.22,0.32),
     ylab = " ", xlab = paste0("sL4, " ,dim(pattern3b)[1] , " bars" ) )
lines(1:4,as.vector(colMeans(pattern4b[,2:5])))
abline(h=1/4, col="cadetblue3")


matrixb<-matrix(,nrow=20,ncol=4)

matrixb[,1]<-table(pattern1b$performer)/as.vector(table(crotchets38$performer))
matrixb[,2]<-table(pattern2b$performer)/as.vector(table(crotchets38$performer))
matrixb[,3]<-table(pattern3b$performer)/as.vector(table(crotchets38$performer))
matrixb[,4]<-table(pattern4b$performer)/as.vector(table(crotchets38$performer))

rownames(matrixb)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
