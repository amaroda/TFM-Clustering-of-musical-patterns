crotchets38ii$type<-1
crotchets38ii$bar <- as.numeric(crotchets38ii$bar)
crotchets99i$type<-2

combi<-rbind(crotchets38ii,crotchets99i)

pca.r<-prcomp(combi[,5:7], center=T,scale. = T)
autoplot(pca.r, colour='type',data=combi)


fviz_nbclust(scale(combi[,2:4]),kmeans,method="silhouette", k.max = 16) 

m <- matrix(,nrow = 900, ncol=3)
n <- matrix(,nrow = 900, ncol=8470)
a <- 0
for (k in 2:16){
  for (i in 1:10){
    a <- kmeans(scale(combi[,2:4]), centers = k,iter.max = 5000)
    m[i+(k-2)*10,1] <- a$tot.withinss
    n[i+(k-2)*10,] <- a$cluster
    m[i+(k-2)*10,2] <- mean(silhouette(as.vector(n[i+(k-2)*10,]),dist(scale(combi[,2:4])))[,3])
    m[i+(k-2)*10,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-1*p[,1]
which.max(u)

combi$cluster.seqb <- as.factor(n[which.max(u),])
pca.r<-prcomp(combi[,2:4], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqb',data=combi)

pattern1b<-combi[combi$cluster.seqb == 1,c(1:5,10)]
pattern2b<-combi[combi$cluster.seqb == 2,c(1:5,10)]
pattern3b<-combi[combi$cluster.seqb == 3,c(1:5,10)]
pattern4b<-combi[combi$cluster.seqb == 4,c(1:5,10)]

pattern5b<-combi[combi$cluster.seqb == 5,c(1:5,10)]
pattern6b<-combi[combi$cluster.seqb == 6,c(1:5,10)]
pattern7b<-combi[combi$cluster.seqb == 7,c(1:5,10)]
pattern8b<-combi[combi$cluster.seqb == 8,c(1:5,10)]

pattern9b<-combi[combi$cluster.seqb == 9,c(1:5,10)]
pattern10b<-combi[combi$cluster.seqb == 10,c(1:5,10)]

pattern11b<-combi[combi$cluster.seqb == 11,c(1:5,10)]
pattern12b<-combi[combi$cluster.seqb == 12,c(1:5,10)]
pattern13b<-combi[combi$cluster.seqb == 13,c(1:5,10)]
pattern14b<-combi[combi$cluster.seqb == 14,c(1:5,10)]

pattern15b<-combi[combi$cluster.seqb == 15,c(1:5,10)]
pattern16b<-combi[combi$cluster.seqb == 16,c(1:5,10)]

colMeans(pattern1b[,2:4])
colMeans(pattern2b[,2:4]) 
colMeans(pattern3b[,2:4]) 
colMeans(pattern4b[,2:4])

colMeans(pattern5b[,2:4])
colMeans(pattern6b[,2:4]) 
colMeans(pattern7b[,2:4]) 
colMeans(pattern8b[,2:4])

colMeans(pattern9b[,2:4]) 
colMeans(pattern10b[,2:4])

colMeans(pattern11b[,2:4])
colMeans(pattern12b[,2:4]) 
colMeans(pattern13b[,2:4]) 
colMeans(pattern14b[,2:4])

colMeans(pattern15b[,2:4])
colMeans(pattern16b[,2:4]) 

colMeans(combi[,2:4])

all_levels <- c("CH","CH36","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")
missing_levels <- setdiff(all_levels, levels(pattern7b$performer))
pattern7b$performer <- factor(pattern7b$performer, levels = c(levels(pattern7b$performer), missing_levels))

patterns<-matrix(,nrow=21,ncol=16)

patterns[,1]<-as.vector(table(pattern1b$performer)/as.vector(table(combi$performer)))
patterns[,2]<-as.vector(table(pattern2b$performer)/as.vector(table(combi$performer)))
patterns[,3]<-as.vector(table(pattern3b$performer)/as.vector(table(combi$performer)))
patterns[,4]<-as.vector(table(pattern4b$performer)/as.vector(table(combi$performer)))
patterns[,5]<-as.vector(table(pattern5b$performer)/as.vector(table(combi$performer)))
patterns[,6]<-as.vector(table(pattern6b$performer)/as.vector(table(combi$performer)))
patterns[,7]<-as.vector(table(pattern7b$performer)/as.vector(table(combi$performer)))
patterns[,8]<-as.vector(table(pattern8b$performer)/as.vector(table(combi$performer)))

patterns[,9]<-as.vector(table(pattern9b$performer)/as.vector(table(combi$performer)))
patterns[,10]<-as.vector(table(pattern10b$performer)/as.vector(table(combi$performer)))
patterns[,11]<-as.vector(table(pattern11b$performer)/as.vector(table(combi$performer)))
patterns[,12]<-as.vector(table(pattern12b$performer)/as.vector(table(combi$performer)))
patterns[,13]<-as.vector(table(pattern13b$performer)/as.vector(table(combi$performer)))
patterns[,14]<-as.vector(table(pattern14b$performer)/as.vector(table(combi$performer)))
patterns[,15]<-as.vector(table(pattern15b$performer)/as.vector(table(combi$performer)))
patterns[,16]<-as.vector(table(pattern16b$performer)/as.vector(table(combi$performer)))


rownames(patterns)<-c("CH","CH36","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
  "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")

dist<-dist(patterns)

hc<-hclust(dist,method = 'ward.D2')
plot(hc)







# Choose and create the clusters
fviz_nbclust(scale(combi[,5:7]),kmeans,method="silhouette", k.max = 16) 
fviz_nbclust(scale(combi[,5:7]),kmeans,method="wss")

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=8470)
a <- 0
for (k in 5:9){
  for (i in 1:50){
    a <- kmeans(scale(combi[,5:7]), centers = k,iter.max = 5000)
    m[i+(k-5)*100,1] <- a$tot.withinss
    n[i+(k-5)*100,] <- a$cluster
    m[i+(k-5)*100,2] <- mean(silhouette(as.vector(n[i+(k-5)*100,]),dist(scale(combi[,5:7])))[,3])
    m[i+(k-5)*100,3] <- k
    
  }
  print(k)
}

m
p<-scale(m)
u<-p[,2]-0.75*p[,1]
which.max(u)

combi$cluster.seqi9 <- as.factor(n[447,])
pca.r<-prcomp(combi[,5:7], center=T,scale. = T)
autoplot(pca.r, colour='cluster.seqi9',data=combi)

pattern1i<-combi[combi$cluster.seqi9 == 1,c(1,5:7)]
pattern2i<-combi[combi$cluster.seqi9 == 2,c(1,5:7)]
pattern3i<-combi[combi$cluster.seqi9 == 3,c(1,5:7)]

pattern4i<-combi[combi$cluster.seqi9 == 4,c(1,5:7)]
pattern5i<-combi[combi$cluster.seqi9 == 5,c(1,5:7)]
pattern6i<-combi[combi$cluster.seqi9 == 6,c(1,5:7)]

pattern7i<-combi[combi$cluster.seqi9 == 7,c(1,5:7)]
pattern8i<-combi[combi$cluster.seqi9 == 8,c(1,5:7)]
pattern9i<-combi[combi$cluster.seqi9 == 9,c(1,5:7)]

pattern10i<-combi[combi$cluster.seqi16 == 10,c(1,5:7)]
pattern11i<-combi[combi$cluster.seqi16 == 11,c(1,5:7)]
pattern12i<-combi[combi$cluster.seqi16 == 12,c(1,5:7)]

pattern13i<-combi[combi$cluster.seqi16 == 13,c(1,5:7)]
pattern14i<-combi[combi$cluster.seqi16 == 14,c(1,5:7)]
pattern15i<-combi[combi$cluster.seqi16 == 15,c(1,5:7)]

pattern16i<-combi[combi$cluster.seqi16 == 16,c(1,5:7)]


colMeans(pattern1i[,2:4]) 
colMeans(pattern2i[,2:4]) 
colMeans(pattern3i[,2:4]) 
colMeans(pattern4i[,2:4]) 
colMeans(pattern5i[,2:4]) 
colMeans(pattern6i[,2:4]) 

colMeans(combi[,5:7])


# Create plots of the patterns
v1<-numeric(4)
v2<-numeric(4)
v3<-numeric(4)
v4<-numeric(4)

v5<-numeric(4)
v6<-numeric(4)
v7<-numeric(4)
v8<-numeric(4)

v9<-numeric(4)
v10<-numeric(4)
v11<-numeric(4)
v12<-numeric(4)

v13<-numeric(4)
v14<-numeric(4)
v15<-numeric(4)
v16<-numeric(4)

v1[1]<-0
v2[1]<-0
v3[1]<-0
v4[1]<-0

v5[1]<-0
v6[1]<-0
v7[1]<-0
v8[1]<-0

v9[1]<-0
v10[1]<-0
v11[1]<-0
v12[1]<-0

v13[1]<-0
v14[1]<-0
v15[1]<-0
v16[1]<-0


v1[2]<- colMeans(pattern1i[,2:4])[1] 
v2[2]<- colMeans(pattern2i[,2:4])[1] 
v3[2]<- colMeans(pattern3i[,2:4])[1]
v4[2]<- colMeans(pattern1i[,2:4])[1] 

v5[2]<- colMeans(pattern2i[,2:4])[1] 
v6[2]<- colMeans(pattern3i[,2:4])[1] 
v7[2]<- colMeans(pattern1i[,2:4])[1] 
v8[2]<- colMeans(pattern2i[,2:4])[1] 

v9[2]<- colMeans(pattern3i[,2:4])[1] 
v10[2]<- colMeans(pattern1i[,2:4])[1] 
v11[2]<- colMeans(pattern2i[,2:4])[1] 
v12[2]<- colMeans(pattern3i[,2:4])[1] 

v13[2]<- colMeans(pattern1i[,2:4])[1] 
v14[2]<- colMeans(pattern2i[,2:4])[1] 
v15[2]<- colMeans(pattern3i[,2:4])[1] 
v16[2]<- colMeans(pattern1i[,2:4])[1] 


for (i in 3:4){
  v1[i]<-v1[i-1]+colMeans(pattern1i[,2:4])[i-1] 
  v2[i]<-v2[i-1]+colMeans(pattern2i[,2:4])[i-1] 
  v3[i]<-v3[i-1]+colMeans(pattern3i[,2:4])[i-1] 
  v4[i]<-v4[i-1]+colMeans(pattern4i[,2:4])[i-1] 
  v5[i]<-v5[i-1]+colMeans(pattern5i[,2:4])[i-1] 
  v6[i]<-v6[i-1]+colMeans(pattern6i[,2:4])[i-1] 
  v7[i]<-v7[i-1]+colMeans(pattern7i[,2:4])[i-1] 
  v8[i]<-v8[i-1]+colMeans(pattern8i[,2:4])[i-1] 
  v9[i]<-v9[i-1]+colMeans(pattern9i[,2:4])[i-1] 
  v10[i]<-v10[i-1]+colMeans(pattern10i[,2:4])[i-1] 
  v11[i]<-v11[i-1]+colMeans(pattern11i[,2:4])[i-1] 
  v12[i]<-v12[i-1]+colMeans(pattern12i[,2:4])[i-1] 
  v13[i]<-v13[i-1]+colMeans(pattern13i[,2:4])[i-1] 
  v14[i]<-v14[i-1]+colMeans(pattern14i[,2:4])[i-1] 
  v15[i]<-v15[i-1]+colMeans(pattern15i[,2:4])[i-1] 
  v16[i]<-v16[i-1]+colMeans(pattern16i[,2:4])[i-1] 
}

par(mfrow=c(3,3))

plot(0:3,v1, ylim = range(c(-0.275,0.275)),
     ylab = " ", xlab = paste0("(+ - - - + -), " ,dim(pattern1i)[1] , " bars" ) )
lines(0:3,v1)
abline(h=0, col="cadetblue3")

plot(0:3,v2, ylim = range(c(-0.275,0.275)),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v2)
abline(h=0, col="cadetblue3")

plot(0:3,v3, ylim = range(c(-0.275,0.275))
     , ylab = " ", xlab = paste0("small fluctuations, " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v3)
abline(h=0, col="cadetblue3")

plot(0:3,v4, ylim = range(c(-0.375,0.175)),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v4)
abline(h=0, col="cadetblue3")


plot(0:3,v5, ylim = range(c(-0.275,0.275))
     , ylab = " ", xlab = paste0("small fluctuations, " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v5)
abline(h=0, col="cadetblue3")
plot(0:3,v6, ylim = range(c(-0.275,0.275)),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v6)
abline(h=0, col="cadetblue3")
plot(0:3,v7, ylim = range(c(-0.3,0.25)),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v7)
abline(h=0, col="cadetblue3")
plot(0:3,v8, ylim = range(c(-0.275,0.275))
     , ylab = " ", xlab = paste0("small fluctuations, " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v8)
abline(h=0, col="cadetblue3")


plot(0:3,v9, ylim = range(c(-0.275,0.275)),
     ylab = " ", xlab = paste0("(+ - - - + -), " ,dim(pattern1i)[1] , " bars" ) )
lines(0:3,v9)
abline(h=0, col="cadetblue3")
plot(0:3,v10, ylim = range(c(-0.1,0.6)),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v10)
abline(h=0, col="cadetblue3")
plot(0:3,v11, ylim = range(c(-0.2,0.4))
     , ylab = " ", xlab = paste0("small fluctuations, " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v11)
abline(h=0, col="cadetblue3")
plot(0:3,v12, ylim = range(c(-0.2,0.5)/1.15),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v12)
abline(h=0, col="cadetblue3")

plot(0:3,v13, ylim = range(c(-0.15,0.55)),
     , ylab = " ", xlab = paste0("small fluctuations, " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v13)
abline(h=0, col="cadetblue3")
plot(0:3,v14, ylim = range(c(-0.3,0.4)),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v14)
abline(h=0, col="cadetblue3")
plot(0:3,v15, ylim = range(c(-0.2,0.5)/1.15),
     , ylab = " ", xlab = paste0("small fluctuations, " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v15)
abline(h=0, col="cadetblue3")
plot(0:3,v16, ylim = range(c(-0.1,0.5)),
     ylab = " ", xlab = paste0("(+ - - - + +), " ,dim(pattern5b)[1] , " bars" ) )
lines(0:3,v16)
abline(h=0, col="cadetblue3")

