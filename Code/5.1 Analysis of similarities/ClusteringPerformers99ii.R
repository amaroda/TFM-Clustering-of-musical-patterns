library(ggpubr)
library(Rtsne)
library(cluster)
library(ggrepel)

## --------------------------------------------------------------------------------------------------
tbeats <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats99ii.csv")
tintensity <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity99ii.csv")
combined <-cbind(tbeats[,1:560],tintensity[,2:561])
DatasetdePerformers <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/DatasetdePerformers99.csv")

# Heatmap and dendrogram
## --------------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/Heatmap99ii.pdf",
    width = 8, height = 8)

rownames(combined)<-combined$performers
dist<-dist(scale(combined[,1:1119]))
heatmap(as.matrix(dist), 
        col = colorRampPalette(c("black","blue", "white"))(256), 
        scale = "none", 
        main = "Distance Matrix Heatmap")

dev.off()

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/Dendogram99ii.pdf",
    width = 8, height = 8)

# hierarchical clustering
hc2 <- hclust(dist, method = "ward.D2")

plot(hc2)

dev.off()


# PCA and k-means
## --------------------------------------------------------------------------------------------------
pca <- prcomp(combined[,1:1119], center = T, scale. = T)
summary(pca)
combined.pca <- as.data.frame(pca$x)
combined.pca$performers <- combined$performers
combined.pca$Formula <- (DatasetdePerformers$Formula-min(DatasetdePerformers$Formula))/
  (max(DatasetdePerformers$Formula)- min(DatasetdePerformers$Formula))
rownames(combined.pca) <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                            "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")
rownames(combined) <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                        "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")



## --------------------------------------------------------------------------------------------------
pcab<-prcomp(tbeats[,1:560], center = T, scale. = T)
summary(pcab)
beats.pca<-as.data.frame(pcab$x)
beats.pca$performers <- combined$performers
rownames(tbeats) <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                      "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")

pcai<-prcomp(tintensity[,2:560], center = T, scale. = T)
summary(pcai)
intensity.pca<-as.data.frame(pcai$x)
intensity.pca$performers <- combined$performers
rownames(tintensity) <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                          "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")



## --------------------------------------------------------------------------------------------------

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=15)
a <- 0

for (k in 3:7){
  for (i in 1:100){
    a <- kmeans(scale(combined[,1:1119]), centers = k,iter.max = 100000)
    m[i+(k-3)*100,1] <- a$tot.withinss
    n[i+(k-3)*100,] <- a$cluster
    m[i+(k-3)*100,2] <- mean(silhouette(as.vector(n[i+(k-3)*100,]),dist(scale(combined[,1:1119])))[,3])
    m[i+(k-3)*100,3] <- k
    
  }
}

p<-scale(m)
u<-p[,2]

combined.pca$cluster <- as.factor(n[which.max(u),])

expvar1<-round(pca$sdev[1]^2/sum((pca$sdev[1:15])^2)*100,2)
expvar2<-round(pca$sdev[2]^2/sum((pca$sdev[1:15])^2)*100,2)

pc1<-ggplot(combined.pca, aes(x = PC1, y = PC2, color = cluster, label = performers)) +
  geom_point() + xlab(paste0("PC1 (",expvar1 ,"%)"))+
  ylab(paste0("PC2 (",expvar2,"%)"))+
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Silhouette: ", round(m[which.max(u),2],4)))




## --------------------------------------------------------------------------------------------------

m <- matrix(,nrow = 600, ncol=3)
n <- matrix(,nrow = 600, ncol=15)
a <- 0

for (k in 3:7){
  for (i in 1:100){
    a <- kmeans(scale(tbeats[,1:560]), centers = k,iter.max = 100000)
    m[i+(k-3)*100,1] <- a$tot.withinss
    n[i+(k-3)*100,] <- a$cluster
    m[i+(k-3)*100,2] <- mean(silhouette(as.vector(n[i+(k-3)*100,]),dist(scale(tbeats[,1:560])))[,3])
    m[i+(k-3)*100,3] <- k
    
  }
}

p<-scale(m)
u<-p[,2]

beats.pca$cluster <- as.factor(n[which.max(u),])

expvar1<-round(pcab$sdev[1]^2/sum((pcab$sdev[1:15])^2)*100,2)
expvar2<-round(pcab$sdev[2]^2/sum((pcab$sdev[1:15])^2)*100,2)

pc2<-ggplot(beats.pca, aes(x = PC1, y = PC2, color = cluster, label = performers)) +
  geom_point() + xlab(paste0("PC1 (",expvar1 ,"%)"))+
  ylab(paste0("PC2 (",expvar2,"%)"))+
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Silhouette: ", round(m[which.max(u),2],4)))



## --------------------------------------------------------------------------------------------------

m <- matrix(,nrow = 500, ncol=3)
n <- matrix(,nrow = 500, ncol=15)
a <- 0

for (k in 3:7){
  for (i in 1:100){
    a <- kmeans(scale(tintensity[,2:560]), centers = k,iter.max = 100000)
    m[i+(k-3)*100,1] <- a$tot.withinss
    n[i+(k-3)*100,] <- a$cluster
    m[i+(k-3)*100,2] <- mean(silhouette(as.vector(n[i+(k-3)*100,]),dist(scale(tintensity[,2:560])))[,3])
    m[i+(k-3)*100,3] <- k
    
  }
}

p<-scale(m)
u<-p[,2]

intensity.pca$cluster <- as.factor(n[which.max(u),])

expvar1<-round(pcab$sdev[1]^2/sum((pcab$sdev[1:15])^2)*100,2)
expvar2<-round(pcab$sdev[2]^2/sum((pcab$sdev[1:15])^2)*100,2)

pc3<-ggplot(intensity.pca, aes(x = PC1, y = PC2, color = cluster, label = performers)) +
  geom_point() + xlab(paste0("PC1 (",expvar1 ,"%)"))+
  ylab(paste0("PC2 (",expvar2,"%)"))+
  geom_text_repel(nudge_y = 0.2) + labs(caption = paste0("Silhouette: ", round(m[which.max(u),2],4)))


# t-SNE
## --------------------------------------------------------------------------------------------------
KL <- 9999
for (k in 1:1000){
  tsne_obj <- Rtsne(scale(combined[,1:1119]), dims = 2, perplexity = 3, max_iter = 2500, pca = F)
  tsne2 <- as.data.frame(tsne_obj$Y)
  newKL<-tsne_obj$itercosts[50]
  
  if (newKL < KL){
    KL <- newKL
    tsne <- tsne2
    tsne$performers <- combined$performers
  }
  
}


# t-SNE

## --------------------------------------------------------------------------------------------------
colnames(tsne)<-c("Dim. 1", "Dim. 2", "performers")
tsne$Formula<-(DatasetdePerformers$Formula-min(DatasetdePerformers$Formula))/
  (max(DatasetdePerformers$Formula)- min(DatasetdePerformers$Formula))
pt1<-ggplot(tsne, aes(x = `Dim. 1`, y = `Dim. 2`, color = Formula, label = performers)) +
  geom_point() +
  geom_text_repel(nudge_y = 0.2)


## --------------------------------------------------------------------------------------------------
KL <- 9999
for (k in 1:500){
  tsne_obj <- Rtsne(scale(tbeats[,1:560]), dims = 2, perplexity = 4, max_iter = 2500, pca = F)
  tsne2 <- as.data.frame(tsne_obj$Y)
  newKL<-tsne_obj$itercosts[50]
  
  if (newKL < KL){
    KL <- newKL
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
  geom_text_repel(nudge_y = 0.2)


## --------------------------------------------------------------------------------------------------
KL <- 9999
for (k in 1:500){
  tsne_obj <- Rtsne(scale(tintensity[,2:560]), dims = 2, perplexity = 4, max_iter = 2500, pca = F)
  tsne2 <- as.data.frame(tsne_obj$Y)
  newKL<-tsne_obj$itercosts[50]
  
  if (newKL < KL){
    KL <- newKL
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
  geom_text_repel(nudge_y = 0.2)


## --------------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/PCAplotbi99ii.pdf",
    width = 16, height = 4.5)

ggarrange( pc1, pt1, nrow = 1, ncol = 2)

dev.off()

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/PCAplotb99ii.pdf",
    width = 16, height = 4.5)

ggarrange( pc2, pt2, nrow = 1, ncol = 2)

dev.off()

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/PCAploti99ii.pdf",
    width = 16, height = 4.5)

ggarrange( pc3, pt3, nrow = 1, ncol = 2)

dev.off()

beep()
