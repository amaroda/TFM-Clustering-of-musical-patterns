# Op.38ii

# Beat percentages

# Choose and create the clusters
fviz_nbclust(scale(quavers38ii[,2:7]),kmeans,method="silhouette", k.max =  12) 
fviz_nbclust(scale(quavers38ii[,2:7]),kmeans,method="wss")

m <- matrix(,nrow = 350, ncol=3)
n <- matrix(,nrow = 350, ncol=4340)
a <- 0
for (k in 8){
for (i in 1:50){
  a <- kmeans(scale(quavers38ii[,2:7]), centers = k,iter.max = 10000)
  m[i+(k-2)*50,1] <- a$tot.withinss
  n[i+(k-2)*50,] <- a$cluster
  m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(quavers38ii[,2:7])))[,3])
  m[i+(k-2)*50,3] <- k
  
}
  print(k)
}

m
p<-scale(m)
u<-p[,2]-0.5*p[,1]
which.max(u)

# Plot the cluster in a PCA plot
quavers38ii$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(quavers38ii[,2:7], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=quavers38ii)

#Separate clusters into dataframes
pattern1b<-quavers38ii[quavers38ii$cluster.seqb == 1,c(1:7,14)]
pattern2b<-quavers38ii[quavers38ii$cluster.seqb == 2,c(1:7,14)]
pattern3b<-quavers38ii[quavers38ii$cluster.seqb == 3,c(1:7,14)]
pattern4b<-quavers38ii[quavers38ii$cluster.seqb == 4,c(1:7,14)]

pattern5b<-quavers38ii[quavers38ii$cluster.seqb == 5,c(1:7,14)]
pattern6b<-quavers38ii[quavers38ii$cluster.seqb == 6,c(1:7,14)]
pattern7b<-quavers38ii[quavers38ii$cluster.seqb == 7,c(1:7,14)]
pattern8b<-quavers38ii[quavers38ii$cluster.seqb == 8,c(1:7,14)]

# Centers of the clusters
colMeans(pattern1b[,2:7])
colMeans(pattern2b[,2:7]) 
colMeans(pattern3b[,2:7]) 
colMeans(pattern4b[,2:7])
colMeans(pattern5b[,2:7])
colMeans(pattern6b[,2:7]) 
colMeans(pattern7b[,2:7]) 
colMeans(pattern8b[,2:7])


colMeans(quavers38ii[,2:7])


# Create plots of the patterns
par(mfrow=c(4,2))

plot(1:6,as.vector(colMeans(pattern1b[,2:7])), ylim = c(0.145,0.22), 
            ylab = " ", xlab = paste0("L4, " ,dim(pattern1b)[1] , " bars" ))
lines(1:6,as.vector(colMeans(pattern1b[,2:7])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern2b[,2:7])), ylim = c(0.145,0.21),
     ylab = " ", xlab = paste0("L6, " ,dim(pattern2b)[1] , " bars" ) )
lines(1:6,as.vector(colMeans(pattern2b[,2:7])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern3b[,2:7])),  
     ylab = " ", xlab = paste0("sL4, " ,dim(pattern3b)[1] , " bars" ) )
lines(1:6,as.vector(colMeans(pattern3b[,2:7])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern4b[,2:7])), 
     ylim = c(0.145,0.21), ylab = " ", xlab = paste0("L3, " ,dim(pattern4b)[1] , " bars" )  )
lines(1:6,as.vector(colMeans(pattern4b[,2:7])))
abline(h=1/6, col="cadetblue3")



plot(1:6,as.vector(colMeans(pattern5b[,2:7])), 
     ylim = c(0.145,0.21), ylab = " ", xlab = paste0("L1, " ,dim(pattern5b)[1] , " bars" )  )
lines(1:6,as.vector(colMeans(pattern5b[,2:7])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern6b[,2:7])), 
     ylim = c(0.145,0.21), ylab = " ", xlab = paste0("L5, " ,dim(pattern6b)[1] , " bars" )  )
lines(1:6,as.vector(colMeans(pattern6b[,2:7])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern7b[,2:7])), 
     ylim = c(0.13,0.2), ylab = " ", xlab = paste0("S5,S6, " ,dim(pattern7b)[1] , " bars" )  )
lines(1:6,as.vector(colMeans(pattern7b[,2:7])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern8b[,2:7])), 
     ylim = c(0.145,0.21), ylab = " ", xlab = paste0("L2, " ,dim(pattern8b)[1] , " bars" )  )
lines(1:6,as.vector(colMeans(pattern8b[,2:7])))
abline(h=1/6, col="cadetblue3")




# Intensity variations

# Choose and create the clusters
fviz_nbclust(scale(quavers38ii[,6:9]),kmeans,method="silhouette", k.max = 16) 
fviz_nbclust(scale(quavers38ii[,6:9]),kmeans,method="wss")

m <- matrix(,nrow = 350, ncol=3)
n <- matrix(,nrow = 350, ncol=4340)
a <- 0
for (k in 3){
  for (i in 1:50){
    a <- kmeans(scale(quavers38ii[,8:13]), centers = k,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(quavers38ii[,8:13])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-p[,1]
which.max(u)

# Plot the clusters in a PCA plot
quavers38ii$cluster.seqi <- as.factor(n[which.max(u),])
pca.r<-prcomp(quavers38ii[,8:13], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi',data=quavers38ii)

# Separate clusters into dataframes
pattern1i<-quavers38ii[quavers38ii$cluster.seqi == 1,c(1,8:13)]
pattern2i<-quavers38ii[quavers38ii$cluster.seqi == 2,c(1,8:13)]
pattern3i<-quavers38ii[quavers38ii$cluster.seqi == 3,c(1,8:13)]

# Centers of the clusters
colMeans(pattern1i[,2:7]) 
colMeans(pattern2i[,2:7]) 
colMeans(pattern3i[,2:7]) 

colMeans(quavers38ii[,8:13])

# Create plots of the patterns
v1<-numeric(7)
v2<-numeric(7)
v3<-numeric(7)
v1[1]<-0
v2[1]<-0
v3[1]<-0
v1[2]<- colMeans(pattern1i[,2:7])[1] 
v2[2]<- colMeans(pattern2i[,2:7])[1] 
v3[2]<- colMeans(pattern3i[,2:7])[1] 

for (i in 3:7){
  v1[i]<-v1[i-1]+colMeans(pattern1i[,2:7])[i-1] 
  v2[i]<-v2[i-1]+colMeans(pattern2i[,2:7])[i-1] 
  v3[i]<-v3[i-1]+colMeans(pattern3i[,2:7])[i-1] 
}

par(mfrow=c(3,1))

plot(0:6,v1, ylim = range(c(v1,v2,v3)),
      ylab = " ", xlab = paste0("(+ - - - + -), " ,dim(pattern1i)[1] , " bars" ) )
lines(0:6,v1)
plot(0:6,v2, ylim = range(c(v2,v3)),
      ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:6,v2)
plot(0:6,v3, ylim = range(c(v1,v2,v3+0.05)/1.2)
     , ylab = " ", xlab = paste0("small fluctuations, " ,dim(pattern5b)[1] , " bars" ) )
lines(0:6,v3)


# Table of the frequencies of the patterns
all_levels <- c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")
missing_levels <- setdiff(all_levels, levels(pattern3b$performer))
pattern3b$performer <- factor(pattern3b$performer, levels = c(levels(pattern3b$performer), missing_levels))

patterns<-matrix(,nrow=20,ncol=11)

patterns[,1]<-as.vector(table(pattern1b$performer)/as.vector(table(quavers38ii$performer)))
patterns[,2]<-as.vector(table(pattern2b$performer)/as.vector(table(quavers38ii$performer)))
patterns[,3]<-as.vector(table(pattern3b$performer)/as.vector(table(quavers38ii$performer)))
patterns[,4]<-as.vector(table(pattern4b$performer)/as.vector(table(quavers38ii$performer)))
patterns[,5]<-as.vector(table(pattern5b$performer)/as.vector(table(quavers38ii$performer)))
patterns[,6]<-as.vector(table(pattern6b$performer)/as.vector(table(quavers38ii$performer)))
patterns[,7]<-as.vector(table(pattern7b$performer)/as.vector(table(quavers38ii$performer)))
patterns[,8]<-as.vector(table(pattern8b$performer)/as.vector(table(quavers38ii$performer)))

patterns[,9]<-as.vector(table(pattern1i$performer)/as.vector(table(quavers38ii$performer)))
patterns[,10]<-as.vector(table(pattern2i$performer)/as.vector(table(quavers38ii$performer)))
patterns[,11]<-as.vector(table(pattern3i$performer)/as.vector(table(quavers38ii$performer)))



colnames(patterns)<-c("L5","sL4","L6","L4","L3","L1","S5S6","L2","IP1","IP2","IP3")
rownames(patterns)<-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                      "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")



kable(patterns, linesep = "|", booktabs = TRUE) %>% kable_styling()



significancy_I<- function(dataframe){
  
  for (i in 2:7){
    if (sign(quantile(dataframe[,i], probs = c(0.05,0.95))[1]) == sign(quantile(dataframe[,i], probs = c(0.05,0.95))[2])){
      print(paste0("The variable ", colnames(dataframe)[i], "is significant. Direction: ", sign(quantile(dataframe[,i], probs = c(0.05,0.95))[1])))
    }
  }
}

significancy_D<- function(dataframe){
  
  for (i in 2:7){
    if (as.numeric(quantile(dataframe[,i], probs = c(0.05,0.95))[1])>colMeans(quavers38ii[,2:7])[i-1]){
      print(paste0("The variable ", colnames(dataframe)[i], " is positively significant."))
      
    }
    if (as.numeric(quantile(dataframe[,i], probs = c(0.05,0.95))[2])<colMeans(quavers38ii[,2:7])[i-1]){
      print(paste0("The variable ", colnames(dataframe)[i], " is negatively significant."))
      
    }
  }
}





