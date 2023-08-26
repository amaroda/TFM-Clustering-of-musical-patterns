## ----setup, include=FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- include=FALSE--------------------------------------------------------------------------------
library(ggpubr)
library(factoextra)
library(Rtsne)
library(kableExtra)
library(cluster)
library(ggfortify)
library(ggrepel)
library(dfms)
library(zoo)


## --------------------------------------------------------------------------------------------------
tbeats <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats.csv")

tintensity <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity.csv")

crotchets38i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/crotchets38i.csv")

notes.cello38i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/notes.cello38i.csv")

beatper38i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38i.csv")

intper38i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38i.csv")

combined <-cbind(tbeats[,1:1120],tintensity)

DatasetdePerformers <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/DatasetdePerformers.csv")


## ---- fig.align='center', fig.dim=c(15,6), echo=FALSE, dev='png', dev.args=list(png=list(bg="transparent"))----

destats<-matrix(,nrow=20,ncol=14)
for (i in 1:20){
  destats[i,1]<-round(sd(as.numeric(tbeats[i,1:1120])),4)
  destats[i,2]<-round(sd(as.numeric(tbeats[i,c(seq(1,1120,4))])),4)
  destats[i,3]<-round(sd(as.numeric(tbeats[i,c(seq(2,1120,4))])),4)
  destats[i,4]<-round(sd(as.numeric(tbeats[i,c(seq(3,1120,4))])),4)
  destats[i,5]<-round(sd(as.numeric(tbeats[i,c(seq(4,1120,4))])),4)
  destats[i,6]<-round(mean(as.numeric(tbeats[i,c(seq(1,1120,4))])),4)
  destats[i,7]<-round(mean(as.numeric(tbeats[i,c(seq(2,1120,4))])),4)
  destats[i,8]<-round(mean(as.numeric(tbeats[i,c(seq(3,1120,4))])),4)
  destats[i,9]<-round(mean(as.numeric(tbeats[i,c(seq(4,1120,4))])),4)
  destats[i,10]<-round(sd(as.numeric(tintensity[i,1:1120])),4)
  destats[i,11]<-round(sd(as.numeric(tintensity[i,c(seq(1,1120,4))])),4)
  destats[i,12]<-round(sd(as.numeric(tintensity[i,c(seq(2,1120,4))])),4)
  destats[i,13]<-round(sd(as.numeric(tintensity[i,c(seq(3,1120,4))])),4)
  destats[i,14]<-round(sd(as.numeric(tintensity[i,c(seq(4,1120,4))])),4)
  
  
}
colnames(destats)<-c("Dur.SD","Dur.B1 SD","Dur.B2 SD","Dur.B3 SD","Dur.B4 SD","Dur.B1 mean",
              "Dur.B2 mean", "Dur.B3 mean", "Dur.B4 mean","Int.SD","Int.B1 SD","Int.B2 SD","Int.B3 SD","Int.B4 SD")
rownames(destats)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

kable(destats, linesep = "|", booktabs = TRUE) %>% kable_styling()

## Boxplots

## All Boxplots

# par(mfrow=c(4,5))
# 
# for (i in 1:20){
#   boxplot(t(tbeats[i,1:1120]),ylim = c(0.15,0.45),xlab = tbeats[i,]$performers)
# }
# 
# for (i in 1:20){
#   boxplot(t(tbeats[i,c(seq(1,1120,4))]),ylim = c(0.15,0.45),xlab = tbeats[i,]$performers)
# }
# 
# for (i in 1:20){
#   boxplot(t(tbeats[i,c(seq(2,1120,4))]),ylim = c(0.15,0.45),xlab = tbeats[i,]$performers)
# }
# 
# for (i in 1:20){
#   boxplot(t(tbeats[i,c(seq(3,1120,4))]),ylim = c(0.15,0.45),xlab = tbeats[i,]$performers)
# }
# 
# for (i in 1:20){
#   boxplot(t(tbeats[i,c(seq(4,1120,4))]),ylim = c(0.15,0.45),xlab = tbeats[i,]$performers)
# }


## Selected Boxplots

## ----distanceheatmap38i, fig.align='center', fig.dim=c(6,6), echo=FALSE, dev='png', dev.args=list(png=list(bg="transparent"))----

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Heatmap38i.pdf",
    width = 8, height = 8)

rownames(combined)<-combined$performers
dist<-dist(scale(combined[,1:2240]))
heatmap(as.matrix(dist), 
        col = colorRampPalette(c( "black","blue", "white"))(256), 
        scale = "none", 
        main = "Distance Matrix Heatmap")

dev.off()

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/recrela.pdf",
    width = 8, height = 8)

# hierarchical clustering
hc2 <- hclust(dist, method = "ward.D2")

plot(hc2)

dev.off


## --------------------------------------------------------------------------------------------------
pca <- prcomp(combined[,1:2240], center = T, scale. = T)
summary(pca)
combined.pca <- as.data.frame(pca$x)
combined.pca$performers <- combined$performers
combined.pca$Formula <- (DatasetdePerformers$Formula-min(DatasetdePerformers$Formula))/
  (max(DatasetdePerformers$Formula)- min(DatasetdePerformers$Formula))
rownames(combined.pca) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
rownames(combined) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")



## --------------------------------------------------------------------------------------------------
pcab<-prcomp(tbeats[,1:1120], center = T, scale. = T)
summary(pcab)
beats.pca<-as.data.frame(pcab$x)
beats.pca$performers <- combined$performers
rownames(tbeats) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

pcai<-prcomp(tintensity[,1:1120], center = T, scale. = T)
summary(pcai)
intensity.pca<-as.data.frame(pcai$x)
intensity.pca$performers <- combined$performers
rownames(tintensity) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")



## ---- eval=F,echo=FALSE----------------------------------------------------------------------------
fviz_nbclust(scale(combined[,1:2240]),kmeans,method="silhouette")
fviz_nbclust(scale(combined[,1:2240]),kmeans,method="wss")


## --------------------------------------------------------------------------------------------------

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=20)
a <- 0

for (k in 4:8){
for (i in 1:100){
  a <- kmeans(scale(combined[,1:2240]), centers = k,iter.max = 100000)
  m[i+(k-4)*100,1] <- a$tot.withinss
  n[i+(k-4)*100,] <- a$cluster
  m[i+(k-4)*100,2] <- mean(silhouette(as.vector(n[i+(k-4)*100,]),dist(scale(combined[,1:2240])))[,3])
  m[i+(k-4)*100,3] <- k

}
}

p<-scale(m)
u<-p[,2]-0.05*p[,1]

combined.pca$cluster <- as.factor(n[which.max(u),])

expvar1<-round(pca$sdev[1]^2/sum((pca$sdev[1:20])^2)*100,2)
expvar2<-round(pca$sdev[2]^2/sum((pca$sdev[1:20])^2)*100,2)

pc1<-ggplot(combined.pca, aes(x = PC1, y = PC2, color = cluster, label = performers)) +
  geom_point() + xlab(paste0("PC1 (",expvar1 ,"%)"))+
  ylab(paste0("PC2 (",expvar2,"%)"))+
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Silhouette: ", round(m[which.max(u),2],4),"  Correlation: ",round(cor(as.vector(dist(combined.pca[,1:2])),as.vector(dist(scale(combined[,1:2240]))), method = c("pearson")),4)))




## ---- eval=FALSE-----------------------------------------------------------------------------------
fviz_nbclust(scale(tbeats[,1:1120]),kmeans,method="silhouette")
fviz_nbclust(scale(tbeats[,1:1120]),kmeans,method="wss")


## --------------------------------------------------------------------------------------------------

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=20)
a <- 0

for (k in 4:8){
  for (i in 1:100){
    a <- kmeans(scale(tbeats[,1:1120]), centers = k,iter.max = 100000)
    m[i+(k-4)*100,1] <- a$tot.withinss
    n[i+(k-4)*100,] <- a$cluster
    m[i+(k-4)*100,2] <- mean(silhouette(as.vector(n[i+(k-4)*100,]),dist(scale(tbeats[,1:1120])))[,3])
    m[i+(k-4)*100,3] <- k
    
  }
}

p<-scale(m)
u<-p[,2]-0.05*p[,1]

beats.pca$cluster <- as.factor(n[which.max(u),])

expvar1<-round(pcab$sdev[1]^2/sum((pcab$sdev[1:20])^2)*100,2)
expvar2<-round(pcab$sdev[2]^2/sum((pcab$sdev[1:20])^2)*100,2)

pc2<-ggplot(beats.pca, aes(x = PC1, y = PC2, color = cluster, label = performers)) +
  geom_point() + xlab(paste0("PC1 (",expvar1 ,"%)"))+
  ylab(paste0("PC2 (",expvar2,"%)"))+
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Silhouette: ", round(m[which.max(u),2],4),"  Correlation: ",round(cor(as.vector(dist(beats.pca[,1:2])),as.vector(dist(scale(tbeats[,1:1120]))), method = c("pearson")),4)))




## ---- eval=FALSE, echo=FALSE-----------------------------------------------------------------------
fviz_nbclust(scale(tintensity[,1:1120]),kmeans,method="silhouette")
fviz_nbclust(scale(tintensity[,1:1120]),kmeans,method="wss")


## --------------------------------------------------------------------------------------------------

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=20)
a <- 0

for (k in 8){
  for (i in 1:100){
    a <- kmeans(scale(tintensity[,1:1120]), centers = k,iter.max = 100000)
    m[i+(k-4)*100,1] <- a$tot.withinss
    n[i+(k-4)*100,] <- a$cluster
    m[i+(k-4)*100,2] <- mean(silhouette(as.vector(n[i+(k-4)*100,]),dist(scale(tintensity[,1:1120])))[,3])
    m[i+(k-4)*100,3] <- k
    
  }
}

p<-scale(m)
u<-p[,2]-0.05*p[,1]

intensity.pca$cluster <- as.factor(n[which.max(u),])

expvar1<-round(pcab$sdev[1]^2/sum((pcab$sdev[1:20])^2)*100,2)
expvar2<-round(pcab$sdev[2]^2/sum((pcab$sdev[1:20])^2)*100,2)

pc3<-ggplot(intensity.pca, aes(x = PC1, y = PC2, color = cluster, label = performers)) +
  geom_point() + xlab(paste0("PC1 (",expvar1 ,"%)"))+
  ylab(paste0("PC2 (",expvar2,"%)"))+
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Silhouette: ", round(m[which.max(u),2],4),"  Correlation: ",round(cor(as.vector(dist(intensity.pca[,1:2])),as.vector(dist(scale(tintensity[,1:1120]))), method = c("pearson")),4)))



## --------------------------------------------------------------------------------------------------
pearson <- 0
for (k in 1:5000){
tsne_obj <- Rtsne(scale(combined[,1:2240]), dims = 2, perplexity = 4, max_iter = 2500, pca = F)
tsne2 <- as.data.frame(tsne_obj$Y)
a <- data.frame(tsne = as.vector(dist(tsne2)), original = as.vector(dist(scale(combined[,1:2240]))))
newpearson <- min(cor(a))

if (newpearson > pearson){
  pearson <- newpearson
  tsne <- tsne2
  tsne$performers <- combined$performers
  }

}




## --------------------------------------------------------------------------------------------------
colnames(tsne)<-c("Dim. 1", "Dim. 2", "performers")
tsne$Formula<-(DatasetdePerformers$Formula-min(DatasetdePerformers$Formula))/
  (max(DatasetdePerformers$Formula)- min(DatasetdePerformers$Formula))
pt1<-ggplot(tsne, aes(x = `Dim. 1`, y = `Dim. 2`, color = Formula, label = performers)) +
  geom_point() +
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Correlation: ", round(pearson,4)))


## --------------------------------------------------------------------------------------------------
pearson <- 0
for (k in 1:500){
tsne_obj <- Rtsne(scale(tbeats[,1:1120]), dims = 2, perplexity = 4, max_iter = 2500, pca = F)
tsne2 <- as.data.frame(tsne_obj$Y)
a <- data.frame(tsne = as.vector(dist(tsne2)), original = as.vector(dist(scale(combined[,1:2240]))))
newpearson <- min(cor(a))

if (newpearson > pearson){
  pearson <- newpearson
  tsne <- tsne2
  tsne$performers <- combined$performers
  }

}



## --------------------------------------------------------------------------------------------------
colnames(tsne)<-c("Dim. 1", "Dim. 2", "performers")
tsne$Formula<-(DatasetdePerformers$Formula-min(DatasetdePerformers$Formula))/
  (max(DatasetdePerformers$Formula)- min(DatasetdePerformers$Formula))
pt2<-ggplot(tsne, aes(x = `Dim. 1`, y = `Dim. 2`, color = Formula, label = performers)) +
  geom_point() +
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Correlation: ", round(pearson,4)))


## --------------------------------------------------------------------------------------------------
pearson <- 0
for (k in 1:500){
tsne_obj <- Rtsne(scale(tintensity[,1:1120]), dims = 2, perplexity = 4, max_iter = 2500, pca = F)
tsne2 <- as.data.frame(tsne_obj$Y)
a <- data.frame(tsne = as.vector(dist(tsne2)), original = as.vector(dist(scale(combined[,1:2240]))))
newpearson <- min(cor(a))

if (newpearson > pearson){
  pearson <- newpearson
  tsne <- tsne2
  tsne$performers <- combined$performers
  }

}


## --------------------------------------------------------------------------------------------------
colnames(tsne)<-c("Dim. 1", "Dim. 2", "performers")
tsne$Formula<-(DatasetdePerformers$Formula-min(DatasetdePerformers$Formula))/
  (max(DatasetdePerformers$Formula)- min(DatasetdePerformers$Formula))
pt3<-ggplot(tsne, aes(x = `Dim. 1`, y = `Dim. 2`, color = Formula, label = performers)) +
  geom_point() +
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Correlation: ", round(pearson,4)))


## ----clustertsnebi, fig.align='center', fig.dim=c(15,6), echo=FALSE, dev='png', dev.args=list(png=list(bg="transparent"))----
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/PCAplotb38i.pdf",
    width = 16, height = 7)

ggarrange( pc1, pt1, nrow = 1, ncol = 2)

dev.off()

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/PCAplotb38i.pdf",
    width = 16, height = 7)

ggarrange( pc2, pt2, nrow = 1, ncol = 2)

dev.off()

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/PCAploti38i.pdf",
    width = 16, height = 7)

ggarrange( pc3, pt3, nrow = 1, ncol = 2)

dev.off()



## --------------------------------------------------------------------------------------------------
beats <- t(beatper38i[,1:1120])
beats <- as.data.frame(beats)
colnames(beats)<-tintensity$performers


## --------------------------------------------------------------------------------------------------
ic<-ICr(beats)
print(ic)
#screeplot(ic)
modelD <- DFM(beats, r = 1)
print(modelD)
modelD$eigen$values
modelD$eigen$values[1]/sum(modelD$eigen$values)
modelD$eigen$vectors
attr(modelD$X_imp,"stats")
#plot(modelD, xlab="Beat")



## --------------------------------------------------------------------------------------------------
intensity <- t(intper38i[,1:1120])
intensity <- as.data.frame(intensity)
colnames(intensity)<-tintensity$performers


## --------------------------------------------------------------------------------------------------
ic<-ICr(intensity)
print(ic)
#screeplot(ic)
modelI <- DFM(intensity, r = 1)
print(modelI)
modelI$eigen$values
modelI$eigen$values[1]/sum(modelI$eigen$values)
modelI$eigen$vectors[,1]
attr(modelI$X_imp,"stats")
#plot(modelI, xlab="Intensity")


## ----dynfactb38i-----------------------------------------------------------------------------------
ma1<-rollmean(modelD$F_pca, k=8)
plot(modelD$F_pca, type = "l", col = "thistle3", ylim = range(modelD$F_pca, ma1), 
     xlab = "Beat", ylab = "Value", main = "First dynamic factor of duration series with rolling mean")

# Add the rolling mean to the plot
lines(ma1, col = "red3", type = "l")

# Add a legend to distinguish the original series and the rolling mean

abline(v=c(360,644,1008))
abline(v=c(132,228,780,872), col="gray65")

legend("topleft", legend = c("First factor of intensity series", "Rolling Mean"), col = c("thistle3", "red3"), 
       lty = c(1, 1), cex = 0.8)

mtext(paste0("Variance explained by the factor: ", round(modelD$eigen$values[1]*100/sum(modelD$eigen$values),2),"%"), side = 1, line = 4, cex=0.7)





## ----dynfacti38i-----------------------------------------------------------------------------------
ma2<-rollmean(modelI$F_pca, k=8)
plot(modelI$F_pca, type = "l", col = "thistle3", ylim = range(modelI$F_pca, ma2), 
     xlab = "Beat", ylab = "Value", main = "First dynamic factor of intensity series with rolling mean")

# Add the rolling mean to the plot
lines(ma2, col = "red3", type = "l")

# Add a legend to distinguish the original series and the rolling mean

abline(v=c(360,644,1008))
abline(v=c(132,228,780,872), col="gray65")

legend("bottomleft", legend = c("First factor of intensity series", "Rolling Mean"), col = c("thistle3", "red3"), 
       lty = c(1, 1), cex = 0.8)

mtext(paste0("Variance explained by the factor: ", round(modelI$eigen$values[1]*100/sum(modelI$eigen$values),2),"%"), side = 1, line = 4, cex=0.7)



## ----movmeansbi38i---------------------------------------------------------------------------------
plot(ma1, type = "l", col = "blue", ylim = range(ma1, ma2), 
     xlab = "Time", ylab = "Value", main = "Dur vs Int")

lines(ma2, col='red')
legend("topleft", legend = c("Duration", "Intensity"), col = c("blue", "red"), 
       lty = c(1, 1), cex = 0.8)

abline(v=c(360,644,1008), col="gray65")
abline(v=c(132,228,780,872), col="gray75")


mtext(paste0("Correlation between series: ", cor(ma1,ma2)), side = 1, line = 4, cex=0.7)


