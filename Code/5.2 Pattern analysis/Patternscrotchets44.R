# Patterns of quarter-note beats of op.38i and 38iii 

crotchets38[crotchets38$type==1,]$type<-1
crotchets38[crotchets38$type==1,]ii$type<-2
crotchets38<-rbind(crotchets38[crotchets38$type==1,],crotchets38iii[crotchets38$type==1,])


# Rhythmic patterns

fviz_nbclust(scale(crotchets38[,2:5]),kmeans,method="silhouette", k.max = 16) 

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=10620)
a <- 0
for (k in 2:8){
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
u<-p[,2]-p[,1]
which.max(u)

crotchets38$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets38[,2:5], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=crotchets38)

pattern1b<-crotchets38[crotchets38$cluster.seqb == 3,c(1:5,11)]
pattern2b<-crotchets38[crotchets38$cluster.seqb == 4,c(1:5,11)]
pattern3b<-crotchets38[crotchets38$cluster.seqb == 2,c(1:5,11)]
pattern4b<-crotchets38[crotchets38$cluster.seqb == 1,c(1:5,11)]

# Centers of the clusters
colMeans(pattern1b[,2:5])
colMeans(pattern2b[,2:5]) 
colMeans(pattern3b[,2:5]) 
colMeans(pattern4b[,2:5])

colMeans(crotchets38[crotchets38$type==1,][,6:9])
colMeans(crotchets38[crotchets38$type==2,][,6:9])


# Create plots of the patterns

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/rhythmic38.pdf",
    width = 16, height = 8)

par(mfrow=c(2,2))

plot(1:4,as.vector(colMeans(pattern1b[,2:5])), ylim = c(0.23,0.31), xlab = "beat", xaxt='n',
     ylab = "length", main = paste0("L1, (" ,dim(pattern1b)[1] , " measures)" ))
lines(1:4,as.vector(colMeans(pattern1b[,2:5])))
abline(h=1/4, col="cadetblue3")
axis(1, at = 1:4, labels = 1:4)

plot(1:4,as.vector(colMeans(pattern2b[,2:5])), ylim = c(0.23,0.31), xlab = "beat", xaxt='n',
     ylab = "length", main = paste0("L2, (" ,dim(pattern2b)[1] , " measures)" ) )
lines(1:4,as.vector(colMeans(pattern2b[,2:5])))
abline(h=1/4, col="cadetblue3")
axis(1, at = 1:4, labels = 1:4)

plot(1:4,as.vector(colMeans(pattern3b[,2:5])),  ylim = c(0.23,0.31), xlab = "beat", xaxt='n',
     ylab = "length", main = paste0("L3, (" ,dim(pattern3b)[1] , " measures)" ) )
lines(1:4,as.vector(colMeans(pattern3b[,2:5])))
abline(h=1/4, col="cadetblue3")
axis(1, at = 1:4, labels = 1:4)

plot(1:4,as.vector(colMeans(pattern4b[,2:5])),  ylim = c(0.23,0.31), xlab = "beat", xaxt='n',
     ylab = "length", main = paste0("L4, (" ,dim(pattern4b)[1] , " measures)" ) )
lines(1:4,as.vector(colMeans(pattern4b[,2:5])))
abline(h=1/4, col="cadetblue3")
axis(1, at = 1:4, labels = 1:4)

dev.off()


# Dynamic patterns

# Choose k and create the clusters
fviz_nbclust(scale(crotchets38[,6:9]),kmeans,method="silhouette", k.max = 16) 
fviz_nbclust(scale(crotchets38[,6:9]),kmeans,method="wss")

m <- matrix(,nrow = 750, ncol=3)
n <- matrix(,nrow = 750, ncol=10620)
a <- 0
for (k in 2:8){
  for (i in 1:5){
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
u<-p[,2]-p[,1]
which.max(u)

# Plot the cluster in a PCA plot
crotchets38$cluster.seqi <- as.factor(n[which.max(u),])
pca.r<-prcomp(crotchets38[,6:9], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi',data=crotchets38)

#Separate clusters into dataframes
pattern1i<-crotchets38[crotchets38$cluster.seqi == 1,c(1,6:9,11)]
pattern2i<-crotchets38[crotchets38$cluster.seqi == 2,c(1,6:9,11)]
pattern3i<-crotchets38[crotchets38$cluster.seqi == 3,c(1,6:9,11)]
pattern4i<-crotchets38[crotchets38$cluster.seqi == 4,c(1,6:9,11)]

#Centers of the clusters
colMeans(pattern1i[,2:5]) 
colMeans(pattern2i[,2:5]) 
colMeans(pattern3i[,2:5]) 
colMeans(pattern4i[,2:5])

colMeans(crotchets38[crotchets38$type==1,][,6:9])
colMeans(crotchets38[crotchets38$type==2,][,6:9])

# Create plots of the patterns

v1<-numeric(5)
v2<-numeric(5)
v3<-numeric(5)
v4<-numeric(5)
v1[1]<-0
v2[1]<-0
v3[1]<-0
v4[1]<-0
v1[2]<- colMeans(pattern1i[,2:5])[1] 
v2[2]<- colMeans(pattern2i[,2:5])[1] 
v3[2]<- colMeans(pattern3i[,2:5])[1] 
v4[2]<- colMeans(pattern4i[,2:5])[1] 

for (i in 3:5){
  v1[i]<-v1[i-1]+colMeans(pattern1i[,2:5])[i-1] 
  v2[i]<-v2[i-1]+colMeans(pattern2i[,2:5])[i-1] 
  v3[i]<-v3[i-1]+colMeans(pattern3i[,2:5])[i-1] 
  v4[i]<-v4[i-1]+colMeans(pattern4i[,2:5])[i-1] 
}

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynamic38.pdf",
    width = 16, height = 8)

par(mfrow=c(2,2))

plot(0:4,v1, ylim = c(-0.12,0.12), xlab = "beat",
     ylab = "change", main = paste0("(+ - + -), " ,dim(pattern1i)[1] , " measures" ) )
lines(0:4,v1)
abline(h=0, col="cadetblue3")

plot(0:4,v2, ylim = c(-0.12,0.12), xlab = "beat",
     ylab = "change", main = paste0("(- + - +), " ,dim(pattern2i)[1] , " measures" ) )
lines(0:4,v2)
abline(h=0, col="cadetblue3")

plot(0:4,v3, ylim = c(-0.12,0.12), xlab = "beat",
     , ylab = "change", main = paste0("(+ - - +), " ,dim(pattern3i)[1] , " measures" ) )
lines(0:4,v3)
abline(h=0, col="cadetblue3")

plot(0:4,v4, ylim = c(-0.12,0.12), xlab = "beat",
     , ylab = "change", main = paste0("(- + + -), " ,dim(pattern4i)[1] , " measures" ) )
lines(0:4,v4)
abline(h=0, col="cadetblue3")

dev.off()

# Is pattern frequency distribution very different between ops? yes

table(crotchets38[crotchets38$type == 1,]$cluster.seqb)/sum(table(crotchets38[crotchets38$type == 1,]$cluster.seqb))
table(crotchets38[crotchets38$type == 2,]$cluster.seqb)/sum(table(crotchets38[crotchets38$type == 2,]$cluster.seqb))

table(crotchets38[crotchets38$type == 1,]$cluster.seqi)/sum(table(crotchets38[crotchets38$type == 1,]$cluster.seqi))
table(crotchets38[crotchets38$type == 2,]$cluster.seqi)/sum(table(crotchets38[crotchets38$type == 2,]$cluster.seqi))


# Pattern frequency

# Rhythmic

patternsb<-matrix(,nrow=20,ncol=8)

patternsb[,1]<-as.vector(table(pattern1b[pattern1b$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsb[,2]<-as.vector(table(pattern2b[pattern2b$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsb[,3]<-as.vector(table(pattern3b[pattern3b$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsb[,4]<-as.vector(table(pattern4b[pattern4b$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsb[,5]<-as.vector(table(pattern1b[pattern1b$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))
patternsb[,6]<-as.vector(table(pattern2b[pattern2b$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))
patternsb[,7]<-as.vector(table(pattern3b[pattern3b$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))
patternsb[,8]<-as.vector(table(pattern4b[pattern4b$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))


colnames(patternsb)<-c("L1","L2","L3","L4","L1","L2","L3","L4")
rownames(patternsb)<-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                      "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")


# Create the barplot
mx <- t(as.matrix(patternsb[c(1:20),]))
colnames(mx) <-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                 "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")


colours = c("#4285F4","#02A62D","#FBBC05","red","#4285F4","#02A62D","#FBBC05","red")

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/frecrhy38.pdf",
    width = 16, height = 8)

barplot(mx, xlab='Performance',beside = F, 
        col=colours,ylim=c(0,max(mx)*5), yaxt='n')
# to add a box around the plot
box()

# add a legend
legend('topleft',fill=colours,legend=c('L1: Long 1st beat','L2: Long 2nd beat','L3: Long 3rd beat','L4: Long 4th beat'))


dev.off()


# Dynamic

patternsi<-matrix(,nrow=20,ncol=8)

patternsi[,1]<-as.vector(table(pattern1i[pattern1i$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsi[,2]<-as.vector(table(pattern2i[pattern2i$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsi[,3]<-as.vector(table(pattern3i[pattern3i$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsi[,4]<-as.vector(table(pattern4i[pattern4i$type==1,]$performer)/as.vector(table(crotchets38[crotchets38$type==1,]$performer)))
patternsi[,5]<-as.vector(table(pattern1i[pattern1i$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))
patternsi[,6]<-as.vector(table(pattern2i[pattern2i$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))
patternsi[,7]<-as.vector(table(pattern3i[pattern3i$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))
patternsi[,8]<-as.vector(table(pattern4i[pattern4i$type==2,]$performer)/as.vector(table(crotchets38[crotchets38$type==2,]$performer)))

colnames(patternsi)<-c("+-+-","-+-+","+--+","-++-","+-+-","-+-+","+--+","-++-")
rownames(patternsi)<-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                       "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")

# Create the barplot
mx <- t(as.matrix(patternsi[c(1:20),]))
colnames(mx) <-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                 "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")


colours = c("#FF5C77","#30CF82","#FFEC59","skyblue","#FF5C77","#30CF82","#FFEC59","skyblue")

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/frecdyn38.pdf",
    width = 16, height = 8)
# barplot
barplot(mx, xlab='Performance',beside = F, 
        col=colours,ylim=c(0,max(mx)*5), yaxt='n')
# to add a box around the plot
box()

# add a legend
legend('topleft',fill=colours,legend=c("(+ - + -)","(- + - +)","(+ - - +)","(- + + -)"))



dev.off()

