library(dfms)

# op.38i

eigv38<-matrix(,nrow = 20,ncol = 6)
eigv99<-matrix(,nrow = 15,ncol = 4)

beatper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38i.csv")
intper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38i.csv")


## --------------------------------------------------------------------------------------------------
beats <- t(beatper[,1:1120]) # traspose the dataset
beats <- as.data.frame(beats)
colnames(beats)<-intper$performers

ic<-ICr(beats) # choose the number of factors
screeplot(ic) # one component with a lot of variance (70%),the rest less than 5%
modelD <- DFM(beats, r = 1, idio.ar1 = T, p=4) # apply the model

#eigenvector
modelD$eigen$values
modelD$eigen$values[1]/sum(modelD$eigen$values)
eigv38[,1]<-modelD$eigen$vectors[,1]




## --------------------------------------------------------------------------------------------------
intensity <- t(intper[,1:1120]) # traspose the dataset
intensity <- as.data.frame(intensity)
colnames(intensity)<-intper$performers

ic<-ICr(intensity) # choose the number of factors
screeplot(ic) # one component with a lot of variance (79%),the rest less than 3%
modelI <- DFM(intensity, r = 1, idio.ar1 = T) # apply the model

#eigenvector
modelI$eigen$values
modelI$eigen$values[1]/sum(modelI$eigen$values)
eigv38[,4]<-modelI$eigen$vectors[,1]




# Graphs
## ------------------------------------------------------------------------------------------

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacdur38i.pdf",
    width = 18, height = 4.5)

plot(modelD$F_qml, type = "l", col = "red2", ylim=c(-7.5,40), xaxt='n',
     xlab = "Beat", ylab = "Value") # first dynamic factor
abline(h=0,col="black") # average beat 
abline(v=c(360,644,1008)) # musical sections
abline(v=c(132,228,780,872), col="gray65") # musical subsections

mtext(paste0("Variance explained by the factor: ", round(modelD$eigen$values[1]*100/sum(modelD$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=36), cex.axis=0.9)

dev.off()
## ------------------------------------------------------------------------------------------

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacint38i.pdf",
    width = 18, height = 4.5)

plot(modelI$F_qml, type = "l", col = "blue2", ylim=c(-17.5,10), xaxt='n',
     xlab = "Beat", ylab = "Value") # first dynamic factor
abline(h=0,col="black") # average beat 
abline(v=c(360,644,1008)) # musical sections
abline(v=c(132,228,780,872), col="gray65") # musical subsections

mtext(paste0("Variance explained by the factor: ", round(modelI$eigen$values[1]*100/sum(modelI$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=36), cex.axis=0.9)

dev.off()


# op. 38ii

beatper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38ii.csv")
intper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38ii.csv")


## --------------------------------------------------------------------------------------------------
beats <- t(beatper[,1:1325])
beats <- as.data.frame(beats)
colnames(beats)<-intper$performers

ic<-ICr(beats)

screeplot(ic) # one component with a lot of variance
modelD <- DFM(beats, r = 1, idio.ar1 = T)

modelD$eigen$values
modelD$eigen$values[1]/sum(modelD$eigen$values)
eigv38[,2]<-modelD$eigen$vectors[,1]




## --------------------------------------------------------------------------------------------------
intensity <- t(intper[,1:1325])
intensity <- as.data.frame(intensity)
colnames(intensity)<-intper$performers
ic<-ICr(intensity)

screeplot(ic)
modelI <- DFM(intensity, r = 1, idio.ar1 = T)

modelI$eigen$values
modelI$eigen$values[1]/sum(modelI$eigen$values)
eigv38[,5]<-modelI$eigen$vectors[,1]




## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacdur38ii.pdf",
    width = 18, height = 4.5)

plot(modelD$F_qml, type = "l", col = "red2", ylim=c(-7.5,40), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(76*6,145*6))
abline(v=c(28*6,57*6,(89+18)*6,(99+18)*6,138*6,(145+28)*6,(145+57)*6), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelD$eigen$values[1]*100/sum(modelD$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=48), cex.axis=0.9)

dev.off()
## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacint38ii.pdf",
    width = 18, height = 4.5)

plot(modelI$F_qml, type = "l", col = "blue2", ylim=c(-17.5,10), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(76*6,145*6))
abline(v=c(28*6,57*6,(89+18)*6,(99+18)*6,138*6,(145+28)*6,(145+57)*6), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelI$eigen$values[1]*100/sum(modelI$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=48), cex.axis=0.9)

dev.off()


# op. 38iii

beatper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38iii.csv")
intper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38iii.csv")


## --------------------------------------------------------------------------------------------------
beats <- t(beatper[,1:788])
beats <- as.data.frame(beats)
colnames(beats)<-intper$performers

ic<-ICr(beats)

screeplot(ic) # one component with a lot of variance 
modelD <- DFM(beats, r = 1, idio.ar1 = T)

modelD$eigen$values
modelD$eigen$values[1]/sum(modelD$eigen$values)
eigv38[,3]<-modelD$eigen$vectors[,1]




## --------------------------------------------------------------------------------------------------
intensity <- t(intper[,1:788])
intensity <- as.data.frame(intensity)
colnames(intensity)<-intper$performers
ic<-ICr(intensity)

screeplot(ic)
modelI <- DFM(intensity, r = 1, idio.ar1 = T)

modelI$eigen$values
modelI$eigen$values[1]/sum(modelI$eigen$values)
eigv38[,6]<-modelI$eigen$vectors[,1]




## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacdur38iii.pdf",
    width = 18, height = 4.5)

plot(modelD$F_qml, type = "l", col = "red2", ylim=c(-7.5,40), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(75*4,122*4,174*4))
abline(v=c(30*4,52*4,131*4,157*4), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelD$eigen$values[1]*100/sum(modelD$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=24), cex.axis=0.9)

dev.off()
## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacint38iii.pdf",
    width = 18, height = 4.5)

plot(modelI$F_qml, type = "l", col = "blue2", ylim=c(-17.5,10), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(75*4,122*4,174*4))
abline(v=c(30*4,52*4,131*4,157*4), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelI$eigen$values[1]*100/sum(modelI$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=24), cex.axis=0.9)

dev.off()


# Op. 99i

beatper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper99i.csv")
intper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper99i.csv")


## --------------------------------------------------------------------------------------------------
beats <- t(beatper[,1:825])
beats <- as.data.frame(beats)
colnames(beats)<-intper$performers

ic<-ICr(beats)

screeplot(ic) # one component with a lot of variance 
modelD <- DFM(beats, r = 1, idio.ar1 = T)

modelD$eigen$values
modelD$eigen$values[1]/sum(modelD$eigen$values)
eigv99[,1]<-modelD$eigen$vectors[,1]




## --------------------------------------------------------------------------------------------------
intensity <- t(intper[,1:825])
intensity <- as.data.frame(intensity)
colnames(intensity)<-intper$performers
ic<-ICr(intensity)

screeplot(ic)
modelI <- DFM(intensity, r = 1, idio.ar1 = T)

modelI$eigen$values
modelI$eigen$values[1]/sum(modelI$eigen$values)
eigv99[,3]<-modelI$eigen$vectors[,1]




## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacdur99i.pdf",
    width = 18, height = 4.5)

plot(modelD$F_qml, type = "l", col = "red2", ylim=c(-7.5,40), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(65*3,130*3,(127+65)*3,(177+65)*3))
abline(v=c(14*3,33*3,59*3,(14+65)*3,(33+65)*3,(59+65)*3,(139+65)*3,(144+65)*3,(170+65)*3,(193+65)*3), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelD$eigen$values[1]*100/sum(modelD$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=27), cex.axis=0.9)

dev.off()
## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacint99i.pdf",
    width = 18, height = 4.5)

plot(modelI$F_qml, type = "l", col = "blue2", ylim=c(-17.5,10), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(65*3,130*3,(127+65)*3,(177+65)*3))
abline(v=c(14*3,33*3,59*3,(14+65)*3,(33+65)*3,(59+65)*3,(139+65)*3,(144+65)*3,(170+65)*3,(193+65)*3), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelI$eigen$values[1]*100/sum(modelI$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=27), cex.axis=0.9)

dev.off()


# Op. 99ii

beatper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper99ii.csv")
intper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper99ii.csv")


## --------------------------------------------------------------------------------------------------
beats <- t(beatper[,1:560])
beats <- as.data.frame(beats)
colnames(beats)<-intper$performers

ic<-ICr(beats)

screeplot(ic) # one component with a lot of variance
modelD <- DFM(beats, r = 1, idio.ar1 = T)

modelD$eigen$values
modelD$eigen$values[1]/sum(modelD$eigen$values)
eigv99[,2]<-modelD$eigen$vectors[,1]




## --------------------------------------------------------------------------------------------------
intensity <- t(intper[,1:560])
intensity <- as.data.frame(intensity)
colnames(intensity)<-intper$performers
ic<-ICr(intensity)

screeplot(ic)
modelI <- DFM(intensity, r = 1, idio.ar1 = T)

modelI$eigen$values
modelI$eigen$values[1]/sum(modelI$eigen$values)
eigv99[,4]<-modelI$eigen$vectors[,1]




## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacdur99ii.pdf",
    width = 18, height = 4.5)

plot(modelD$F_qml, type = "l", col = "red2", ylim=c(-7.5,40), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(19*8,43*8,62*8))
abline(v=c(11*8,32*8,55*8), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelD$eigen$values[1]*100/sum(modelD$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=24), cex.axis=0.9)

dev.off()
## ------------------------------------------------------------------------------------------
pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/dynfacint99ii.pdf",
    width = 18, height = 4.5)

plot(modelI$F_qml, type = "l", col = "blue2", ylim=c(-17.5,10), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0,col="black")
abline(v=c(19*8,43*8,62*8))
abline(v=c(11*8,32*8,55*8), col="gray65")

mtext(paste0("Variance explained by the factor: ", round(modelI$eigen$values[1]*100/sum(modelI$eigen$values),2),"%"), side = 1, line = 4, cex=0.9)
axis(side = 1, at = seq(1,dim(beats)[1],by=24), cex.axis=0.9)

dev.off()


## ------------------------------------------------------------------------------------------

colnames(eigv38)<-c("F1 Dur. 38i", "F1 Dur. 38ii", "F1 Dur. 38iii",
                     "F1 Int. 38i", "F1 Int. 38ii","F1 Int. 38iii")
rownames(eigv38)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
eigv38<-round(eigv38,4)

library("gridExtra")
pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/eigv38.pdf",
    width = 8, height = 8)       
grid.table(eigv38)
dev.off()


colnames(eigv99)<-c("F1 Dur. 99i", "F1 Dur. 99ii",
                    "F1 Int. 99i", "F1 Int. 99ii")
rownames(eigv99)<-c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                    "ML"  ,"PN" ,"PR66"  ,"PV" ,"RS" ,"SB" ,"SS")
eigv99<-round(eigv99,4)

library("gridExtra")
pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/eigv99.pdf",
    width = 6, height = 6)       
grid.table(eigv99)
dev.off()


# Idiosyncratic series (models of 38i)

beatper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38i.csv")
intper <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38i.csv")

# Apply models for op. 38i again
beats <- t(beatper[,1:1120])
beats <- as.data.frame(beats)
colnames(beats)<-intper$performers
intensity <- t(intper[,1:1120])
intensity <- as.data.frame(intensity)
colnames(intensity)<-intper$performers

modelD <- DFM(beats, r = 1, idio.ar1 = T)
modelI <- DFM(intensity, r = 1, idio.ar1 = T)


Dur_idi<-modelD$e
Int_idi<-modelI$e

#FvdP

pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/durFvdP.pdf",
    width = 9, height = 4.5)

plot(Dur_idi[,4], type = "l", col = "red2", xaxt='n',
     xlab = "Beat", ylab = "Value") #idiosyncratic duration series
abline(h=0)
abline(v=c(360,644,1008)) # musical sections
abline(v=c(132,228,780,872), col="gray65") # musical subsections
axis(side = 1, at = seq(1,dim(beats)[1],by=72), cex.axis=0.85)

dev.off()

pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/intFvdP.pdf",
    width = 9, height = 4.5)

plot(Int_idi[,4], type = "l", col = "blue2", ylim=c(-2.75,2.75),xaxt='n',
     xlab = "Beat", ylab = "Value") #idiosyncratic intensity series
abline(h=0)
abline(v=c(360,644,1008)) # musical sections
abline(v=c(132,228,780,872), col="gray65") # musical subsections
axis(side = 1, at = seq(1,dim(beats)[1],by=72), cex.axis=0.85)

dev.off()

#DPB

pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/durDPB.pdf",
    width = 9, height = 4.5)

plot(Dur_idi[,2], type = "l", col = "red2", ylim=c(-3.25,3.25),xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0)
abline(v=c(360,644,1008))
abline(v=c(132,228,780,872), col="gray65")
axis(side = 1, at = seq(1,dim(beats)[1],by=72), cex.axis=0.85)

dev.off()

pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/intDPB.pdf",
    width = 9, height = 4.5)

plot(Int_idi[,2], type = "l", col = "blue2", ylim=c(-2.75,2.75),xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0)
abline(v=c(360,644,1008))
abline(v=c(132,228,780,872), col="gray65")
axis(side = 1, at = seq(1,dim(beats)[1],by=72), cex.axis=0.85)

dev.off()

#MG

pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/durMG.pdf",
    width = 9, height = 4.5)

plot(Dur_idi[,10], type = "l", col = "red2", ylim=c(-3.25,3.25),xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0)
abline(v=c(360,644,1008))
abline(v=c(132,228,780,872), col="gray65")
axis(side = 1, at = seq(1,dim(beats)[1],by=72), cex.axis=0.85)

dev.off()

pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/intMG.pdf",
    width = 9, height = 4.5)

plot(Int_idi[,10], type = "l", col = "blue2", ylim=c(-2.75,2.75), xaxt='n',
     xlab = "Beat", ylab = "Value")
abline(h=0)
abline(v=c(360,644,1008))
abline(v=c(132,228,780,872), col="gray65")
axis(side = 1, at = seq(1,dim(beats)[1],by=72), cex.axis=0.85)

dev.off()

v<-numeric(560)
for (i in 1:560){
ma1<-rollmean(modelD$F_qml, k=i)
ma2<-rollmean(modelI$F_qml, k=i)
v[i]<-cor(ma1,ma2)
}

ma1<-rollmean(modelD$F_qml, k=20)
ma2<-rollmean(modelI$F_qml, k=20)
plot(ma1, col='red', type='l',ylim=c(-11,14))
lines(ma2, col='blue')
