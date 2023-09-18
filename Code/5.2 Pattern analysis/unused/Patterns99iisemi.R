# Op. 99ii

library(cluster)
library(ggfortify)

# Beat percentages

# Choosing the number of clusters and creating them
fviz_nbclust(scale(semiquavers99ii[,2:9]),kmeans,method="silhouette", k.max = 20) 
fviz_nbclust(scale(semiquavers99ii[,2:9]),kmeans,method="wss")


m <- matrix(,nrow = 350, ncol=3)
n <- matrix(,nrow = 350, ncol=1050)
a <- 0
for (k in 7){
  for (i in 1:100){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(semiquavers99ii[,2:9]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(semiquavers99ii[,2:9])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-0.05*p[,1]
which.max(u)

# Plotting the clusters in a PCA plot
semiquavers99ii$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(semiquavers99ii[,2:5], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=semiquavers99ii)

# Separating the clusters (patterns) into dataframes
pattern1b<-semiquavers99ii[semiquavers99ii$cluster.seqb == 1,c(1:9,18)]
pattern2b<-semiquavers99ii[semiquavers99ii$cluster.seqb == 2,c(1:9,18)]
pattern3b<-semiquavers99ii[semiquavers99ii$cluster.seqb == 3,c(1:9,18)]
pattern4b<-semiquavers99ii[semiquavers99ii$cluster.seqb == 4,c(1:9,18)]
pattern5b<-semiquavers99ii[semiquavers99ii$cluster.seqb == 5,c(1:9,18)]
pattern6b<-semiquavers99ii[semiquavers99ii$cluster.seqb == 6,c(1:9,18)]
pattern7b<-semiquavers99ii[semiquavers99ii$cluster.seqb == 7,c(1:9,18)]

# centers of the clusters
colMeans(pattern1b[,2:9]) 
colMeans(pattern2b[,2:9])
colMeans(pattern3b[,2:9])
colMeans(pattern4b[,2:9])
colMeans(pattern5b[,2:9])
colMeans(pattern6b[,2:9])
colMeans(pattern7b[,2:9])

colMeans(semiquavers99ii[,2:9])

# Plotting the patterns
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/rhythmic99ii.pdf",
    width = 16, height = 16)

par(mfrow=c(4,2))

plot(1:8,as.vector(colMeans(pattern1b[,2:9])),  xlab = "beat", ylim = c(0.105,0.17),
     ylab = "length", main =paste0("L4L6, (" ,dim(pattern1b)[1] , " measures)" ))
lines(1:8,as.vector(colMeans(pattern1b[,2:9])))
abline(h=1/8, col="cadetblue3")

plot(1:8,as.vector(colMeans(pattern2b[,2:9])), xlab = "beat",  
     ylab = "length", main =paste0("sL8, (" ,dim(pattern2b)[1] , " measures)" ) )
lines(1:8,as.vector(colMeans(pattern2b[,2:9])))
abline(h=1/8, col="cadetblue3")

plot(1:8,as.vector(colMeans(pattern3b[,2:9])), xlab = "beat",  ylim = c(0.105,0.17),
     ylab = "length", main =paste0("L1, (" ,dim(pattern3b)[1] , " measures)" ) )
lines(1:8,as.vector(colMeans(pattern3b[,2:9])))
abline(h=1/8, col="cadetblue3")

plot(1:8,as.vector(colMeans(pattern4b[,2:9])), xlab = "beat",  ylim = c(0.105,0.17),
      ylab = "length", main =paste0("L2,L3, (" ,dim(pattern4b)[1] , " measures)" )  )
lines(1:8,as.vector(colMeans(pattern4b[,2:9])))
abline(h=1/8, col="cadetblue3")



plot(1:8,as.vector(colMeans(pattern5b[,2:9])), xlab = "beat",  ylim = c(0.105,0.17),
      ylab = "length", main =paste0("L8, (" ,dim(pattern5b)[1] , " measures)" )  )
lines(1:8,as.vector(colMeans(pattern5b[,2:9])))
abline(h=1/8, col="cadetblue3")

plot(1:8,as.vector(colMeans(pattern6b[,2:9])), xlab = "beat",  ylim = c(0.105,0.17),
      ylab = "length", main =paste0("L7, (" ,dim(pattern6b)[1] , " measures)" )  )
lines(1:8,as.vector(colMeans(pattern6b[,2:9])))
abline(h=1/8, col="cadetblue3")

plot(1:8,as.vector(colMeans(pattern7b[,2:9])), xlab = "beat", ylim = c(0.105,0.17),
     ylab = "length", main =paste0("L4L5, (" ,dim(pattern7b)[1] , " measures)" )  )
lines(1:8,as.vector(colMeans(pattern7b[,2:9])))
abline(h=1/8, col="cadetblue3")

dev.off()

table(pattern1b$performer)/as.vector(table(semiquavers99ii$performer))


table(pattern2b$performer)/as.vector(table(semiquavers99ii$performer))



# Intensity variations

# Choosing the number of clusters and creating them
fviz_nbclust(scale(semiquavers99ii[,10:17]),kmeans,method="silhouette") 
fviz_nbclust(scale(semiquavers99ii[,10:17]),kmeans,method="wss")

m <- matrix(,nrow = 350, ncol=3)
n <- matrix(,nrow = 350, ncol=1050)
a <- 0
for (k in 4){
  for (i in 1:100){
    n_clust <- as.numeric(k)
    a <- kmeans(scale(semiquavers99ii[,10:17]), centers = n_clust,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(semiquavers99ii[,10:17])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-0.08*p[,1]
which.max(u)

# Plotting the clusters in a PCA plot
semiquavers99ii$cluster.seqi <- as.factor(n[180,])
pca.r<-prcomp(semiquavers99ii[,10:17], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi',data=semiquavers99ii)

# Separating the clusters (patterns) into dataframes
pattern1i<-semiquavers99ii[semiquavers99ii$cluster.seqi == 1,c(1,10:17,18)]
pattern2i<-semiquavers99ii[semiquavers99ii$cluster.seqi == 2,c(1,10:17,18)]
pattern3i<-semiquavers99ii[semiquavers99ii$cluster.seqi == 3,c(1,10:17,18)]
pattern4i<-semiquavers99ii[semiquavers99ii$cluster.seqi == 4,c(1,10:17,18)]

table(pattern1i$performer)/as.vector(table(semiquavers99ii$performer))
table(pattern2i$performer)/as.vector(table(semiquavers99ii$performer))
table(pattern3i$performer)/as.vector(table(semiquavers99ii$performer))

# Centers of the clusters
colMeans(pattern1i[,2:9]) 
colMeans(pattern2i[,2:9]) 
colMeans(pattern3i[,2:9]) 
colMeans(pattern4i[,2:9])

colMeans(semiquavers99ii[,10:17])

# Plotting the patterns
v1<-numeric(9)
v2<-numeric(9)
v3<-numeric(9)
v4<-numeric(9)
v1[1]<-0
v2[1]<-0
v3[1]<-0
v4[1]<-0
v1[2]<- colMeans(pattern1i[,2:9])[1] 
v2[2]<- colMeans(pattern2i[,2:9])[1] 
v3[2]<- colMeans(pattern3i[,2:9])[1] 
v4[2]<- colMeans(pattern4i[,2:9])[1] 

for (i in 3:9){
  v1[i]<-v1[i-1]+colMeans(pattern1i[,2:9])[i-1] 
  v2[i]<-v2[i-1]+colMeans(pattern2i[,2:9])[i-1] 
  v3[i]<-v3[i-1]+colMeans(pattern3i[,2:9])[i-1] 
  v4[i]<-v4[i-1]+colMeans(pattern4i[,2:9])[i-1] 
}


pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynamic99ii.pdf",
    width = 16, height = 8)

par(mfrow=c(2,2))

plot(0:8,v1, ylim = range(c(v1,v2,v4)), xlab ="beat",
     ylab = "change", main =paste0("(+ - + - + - + -), " ,dim(pattern1i)[1] , " measures" )  )
lines(0:8,v1)
abline(h=0, col="cadetblue3")

plot(0:8,v2, ylim = range(c(v1,v2,v4)), xlab ="beat",
     ylab = "change", main =paste0("(- + - + - + - -), " ,dim(pattern2i)[1] , " measures" )  )
lines(0:8,v2)
abline(h=0, col="cadetblue3")

plot(0:8,v3, ylim = range(c(v1,v2,v4)), xlab ="beat",
     ylab = "change", main =paste0("(0 0 + - 0 0 + -), " ,dim(pattern3i)[1] , " measures" )  )
lines(0:8,v3)
abline(h=0, col="cadetblue3")

plot(0:8,v4, ylim = range(c(v1,v2,v4)), xlab ="beat",
     ylab = "change", main =paste0("(+ - - + + - - +), " ,dim(pattern4i)[1] , " measures" )  )
lines(0:8,v4)
abline(h=0, col="cadetblue3")


dev.off()

# Table with the frequencies of the patterns for each performer
all_levels <- c("CH","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                "ML"  ,"PN"  ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS") 
missing_levels <- setdiff(all_levels, levels(pattern3i$performer))
pattern3i$performer <- factor(pattern3i$performer, levels = c(levels(pattern3i$performer), missing_levels))

patterns<-matrix(,nrow=15,ncol=11)

patterns[,1]<-as.vector(table(pattern1b$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,2]<-as.vector(table(pattern2b$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,3]<-as.vector(table(pattern3b$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,4]<-as.vector(table(pattern4b$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,5]<-as.vector(table(pattern5b$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,6]<-as.vector(table(pattern6b$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,7]<-as.vector(table(pattern7b$performer)/as.vector(table(semiquavers99ii$performer)))

patterns[,8]<-as.vector(table(pattern1i$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,9]<-as.vector(table(pattern2i$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,10]<-as.vector(table(pattern3i$performer)/as.vector(table(semiquavers99ii$performer)))
patterns[,11]<-as.vector(table(pattern4i$performer)/as.vector(table(semiquavers99ii$performer)))




colnames(patterns)<-c("L4L6","sL8","L1","L2L3","L8","L7","L4L5","IP1","IP2","IP3","IP4")
rownames(patterns)<-c("CH","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                      "ML"  ,"PN"  ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")

# Duration

mx <- t(as.matrix(patterns[c(1:15),1:7]))
colnames(mx) <-c("CH","DPB" ,"FB", "GG", "GGu" ,"IH" ,"MA","MG",
                 "ML" , "PN", "PR66" , "PV" ,"RS" ,"SB" ,"SS")


colours = c("red","orange","#f0ed6e","green", "cyan","blue","purple","hotpink")
colours2 = c("cyan","blue","purple","hotpink")

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/frecrhy99ii.pdf",
    width = 16, height = 8)

barplot(mx, xlab='Performance',beside = F, 
        col=colours,ylim=c(0,max(mx)*2.5), yaxt='n')
# to add a box around the plot
box()

# add a legend
legend('topleft',fill=colours,legend=c('L4L6: Long 6th and 4th beats','sL8: very long 8th beat','L1: Long 1st beat','L2L3: Long 2nd and 3rd beats'))
legend('topright',fill=colours2,legend=c('L8: Long 8th beat','L7: Long 4th beat','L4L5: Long 4th and 5th beats'))

dev.off()

# Intensity

mx <- t(as.matrix(patterns[c(1:15),8:11]))
colnames(mx) <-c("CH","DPB" ,"FB", "GG", "GGu" ,"IH" ,"MA","MG",
                 "ML" , "PN", "PR66" , "PV" ,"RS" ,"SB" ,"SS")


colours = c("#FF5C77","#30CF82","#FFEC59","skyblue")

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/frecdyn99ii.pdf",
    width = 16, height = 8)

barplot(mx, xlab='Performance',beside = F, 
        col=colours,ylim=c(0,max(mx)*2.65), yaxt='n')
# to add a box around the plot
box()

# add a legend
legend('topleft',fill=colours,legend=c('(+ - + - + - + -)','(- + - + - + - -)','(0 0 + - 0 0 + -)','(+ - - + + - - +)'))


dev.off()
