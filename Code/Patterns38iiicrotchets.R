# Op.38iii

# Beat percentages

# Choose and create the clusters
fviz_nbclust(scale(crotchets38iii[,2:5]),kmeans,method="silhouette", k.max =  12) 
fviz_nbclust(scale(crotchets38iii[,2:5]),kmeans,method="wss")

m <- matrix(,nrow = 1500, ncol=3)
n <- matrix(,nrow = 1500, ncol=3940)
a <- 0
for (k in 2:16){
  for (i in 1:50){
    a <- kmeans(scale(crotchets38iii[,2:5]), centers = k,iter.max = 5000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets38iii[,2:5])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-1.1*p[,1]
which.max(u)

# Plot the cluster in a PCA plot
crotchets38iii$cluster.seqb11 <- as.factor(n[457,])
pca.r<-prcomp(crotchets38iii[,2:5], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb11',data=crotchets38iii)

#Separate clusters into dataframes
pattern1b<-crotchets38iii[crotchets38iii$cluster.seqb == 1,c(1:5,10)]
pattern2b<-crotchets38iii[crotchets38iii$cluster.seqb == 2,c(1:5,10)]
pattern3b<-crotchets38iii[crotchets38iii$cluster.seqb == 3,c(1:5,10)]
pattern4b<-crotchets38iii[crotchets38iii$cluster.seqb == 4,c(1:5,10)]


# Centers of the clusters
colMeans(pattern1b[,2:5])
colMeans(pattern2b[,2:5]) 
colMeans(pattern3b[,2:5]) 
colMeans(pattern4b[,2:5])


colMeans(crotchets38iii[,2:5])


# Create plots of the patterns
par(mfrow=c(4,2))

plot(1:6,as.vector(colMeans(pattern1b[,2:5])), ylim = c(0.145,0.22), 
     ylab = " ", xlab = paste0("L4, " ,dim(pattern1b)[1] , " bars" ))
lines(1:6,as.vector(colMeans(pattern1b[,2:5])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern2b[,2:5])), ylim = c(0.145,0.21),
     ylab = " ", xlab = paste0("L6, " ,dim(pattern2b)[1] , " bars" ) )
lines(1:6,as.vector(colMeans(pattern2b[,2:5])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern3b[,2:5])),  
     ylab = " ", xlab = paste0("sL4, " ,dim(pattern3b)[1] , " bars" ) )
lines(1:6,as.vector(colMeans(pattern3b[,2:5])))
abline(h=1/6, col="cadetblue3")

plot(1:6,as.vector(colMeans(pattern4b[,2:5])), 
     ylim = c(0.145,0.21), ylab = " ", xlab = paste0("L3, " ,dim(pattern4b)[1] , " bars" )  )
lines(1:6,as.vector(colMeans(pattern4b[,2:5])))
abline(h=1/6, col="cadetblue3")



plot(1:6,as.vector(colMeans(pattern5b[,2:5])), 
     ylim = c(0.145,0.21), ylab = " ", xlab = paste0("L1, " ,dim(pattern5b)[1] , " bars" )  )
lines(1:6,as.vector(colMeans(pattern5b[,2:5])))
abline(h=1/6, col="cadetblue3")


# Intensity variations

# Choose and create the clusters
fviz_nbclust(scale(crotchets38iii[,6:9]),kmeans,method="silhouette", k.max = 16) 
fviz_nbclust(scale(crotchets38iii[,6:9]),kmeans,method="wss")

m <- matrix(,nrow = 800, ncol=3)
n <- matrix(,nrow = 800, ncol=3940)
a <- 0
for (k in 2:18){
  for (i in 1:50){
    a <- kmeans(scale(crotchets38iii[,6:9]), centers = k,iter.max = 10000)
    m[i+(k-2)*50,1] <- a$tot.withinss
    n[i+(k-2)*50,] <- a$cluster
    m[i+(k-2)*50,2] <- mean(silhouette(as.vector(n[i+(k-2)*50,]),dist(scale(crotchets38iii[,6:9])))[,3])
    m[i+(k-2)*50,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-0.75*p[,1]
which.max(u)

# Plot the clusters in a PCA plot
crotchets38iii$cluster.seqi9 <- as.factor(n[366,])
pca.r<-prcomp(crotchets38iii[,6:9], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi9',data=crotchets38iii)

# Separate clusters into dataframes
pattern1i<-crotchets38iii[crotchets38iii$cluster.seqi == 1,c(1,6:9)]
pattern2i<-crotchets38iii[crotchets38iii$cluster.seqi == 2,c(1,6:9)]

# Centers of the clusters
colMeans(pattern1i[,2:5]) 
colMeans(pattern2i[,2:5]) 

colMeans(crotchets38iii[,6:9])


# Create plots of the patterns
v1<-numeric(7)
v2<-numeric(7)
v3<-numeric(7)
v1[1]<-0
v2[1]<-0
v3[1]<-0
v1[2]<- colMeans(pattern1i[,2:5])[1] 
v2[2]<- colMeans(pattern2i[,2:5])[1] 
v3[2]<- colMeans(pattern3i[,2:5])[1] 

for (i in 3:7){
  v1[i]<-v1[i-1]+colMeans(pattern1i[,2:5])[i-1] 
  v2[i]<-v2[i-1]+colMeans(pattern2i[,2:5])[i-1] 
  v3[i]<-v3[i-1]+colMeans(pattern3i[,2:5])[i-1] 
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

patterns[,1]<-as.vector(table(pattern1b$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,2]<-as.vector(table(pattern2b$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,3]<-as.vector(table(pattern3b$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,4]<-as.vector(table(pattern4b$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,5]<-as.vector(table(pattern5b$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,6]<-as.vector(table(pattern6b$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,7]<-as.vector(table(pattern7b$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,8]<-as.vector(table(pattern8b$performer)/as.vector(table(crotchets38iii$performer)))

patterns[,9]<-as.vector(table(pattern1i$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,10]<-as.vector(table(pattern2i$performer)/as.vector(table(crotchets38iii$performer)))
patterns[,11]<-as.vector(table(pattern3i$performer)/as.vector(table(crotchets38iii$performer)))



colnames(patterns)<-c("L5","sL4","L6","L4","L3","L1","S5S6","L2","IP1","IP2","IP3")
rownames(patterns)<-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                      "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")



kable(patterns, linesep = "|", booktabs = TRUE) %>% kable_styling()



significancy_I<- function(dataframe){
  
  for (i in 2:5){
    if (sign(quantile(dataframe[,i], probs = c(0.05,0.95))[1]) == sign(quantile(dataframe[,i], probs = c(0.05,0.95))[2])){
      print(paste0("The variable ", colnames(dataframe)[i], "is significant. Direction: ", sign(quantile(dataframe[,i], probs = c(0.05,0.95))[1])))
    }
  }
}

significancy_D<- function(dataframe){
  
  for (i in 2:5){
    if (as.numeric(quantile(dataframe[,i], probs = c(0.05,0.95))[1])>colMeans(crotchets38iii[,2:5])[i-1]){
      print(paste0("The variable ", colnames(dataframe)[i], " is positively significant."))
      
    }
    if (as.numeric(quantile(dataframe[,i], probs = c(0.05,0.95))[2])<colMeans(crotchets38iii[,2:5])[i-1]){
      print(paste0("The variable ", colnames(dataframe)[i], " is negatively significant."))
      
    }
  }
}





