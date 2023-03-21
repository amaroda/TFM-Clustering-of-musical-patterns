
# Clustering of musical patterns

## Creating a proper datasets that will contain the duration and dynamic fluctuations of each performance

### Libraries

library(plyr)
library(readr)
library(caret)
library(ggfortify)


### Loading the raw datasets

#setwd("C:/Users/samte/OneDrive/Desktop/Op38i")
setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 38ii")

mydir = as.vector(c("CyH58","DPyB","FyB","FyvdP","GyG","GyGr","GyGu",
                    "IyH","MyA","MyG","MyL","MyP","PyN","PyR36","PyR66",
                    "PyS","PyV","RyS","SyB","SyS"))
#mytype = as.vector(c("bars","crotchets","minims","notes.cello","notes.piano"))
mytype = as.vector(c("bars","crotchets","notes.cello","notes.piano","quavers"))

for (i in 1:length(mydir)){
midir <- mydir[i]
myfiles = list.files(path=midir, pattern="*.csv", full.names=TRUE)

  for (j in c(5)){
   type <- mytype[j]
   assign(paste0(midir,type), read.csv(myfiles[j],sep=";")) 

  }
 
}

### Tidying functions

crotchet_fun38i <- function(dataframe, performerName){
  
  cont <- 0
  newdf <- matrix(,ncol=4,nrow=floor(dim(dataframe)[1]/4))
  for (i in seq(1,dim(dataframe)[1] - 1, by=4)){
    newdf[i-3*cont,1] <- dataframe[i,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    newdf[i-3*cont,2] <- dataframe[i+1,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    newdf[i-3*cont,3] <- dataframe[i+2,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    newdf[i-3*cont,4] <- dataframe[i+3,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    cont = cont + 1
  }
  
  
  newdf2 <- matrix(,ncol=4,nrow=floor(dim(dataframe)[1]/4))
  cont <- 1
  
  newdf2[1,1] <- 0
  newdf2[1,2] <- (dataframe[2,]$Intensity..dB. - dataframe[1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,3] <- (dataframe[3,]$Intensity..dB. - dataframe[2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,4] <- (dataframe[4,]$Intensity..dB. - dataframe[3,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  
  for (i in seq(5,dim(dataframe)[1] - 1, by=4)){
    
    
    newdf2[i-3*cont,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-3*cont,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-3*cont,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-3*cont,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    cont = cont + 1
    
  }
  performers <- deparse(substitute(performerName))
  
  performerName <- cbind(newdf,newdf2)
  performerName <- as.data.frame(performerName)
  performerName$performer <- performers
  
  return(performerName)
  
}

quaver_fun38ii <- function(dataframe, performerName){
  cont <- 0
  newdf <- matrix(,ncol=6,nrow=(dim(dataframe)[1]-6)/6)
  for (i in seq(3,(dim(dataframe)[1]-6), by=6)){
    newdf[i-5*cont-2,1] <- dataframe[i,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s.)
    newdf[i-5*cont-2,2] <- dataframe[i+1,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                         +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s.)
    newdf[i-5*cont-2,3] <- dataframe[i+2,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                         +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s.)
    newdf[i-5*cont-2,4] <- dataframe[i+3,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                         +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s.)
    newdf[i-5*cont-2,5] <- dataframe[i+4,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                         +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s.)
    newdf[i-5*cont-2,6] <- dataframe[i+5,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                         +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. )
    cont = cont + 1
  }
  
  
  newdf2 <- matrix(,ncol=6,nrow=(dim(dataframe)[1]-6)/6)
  cont<-0
  for (i in seq(3,(dim(dataframe)[1]-6), by=6)){
    
    
    newdf2[i-5*cont-2,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,5] <- (dataframe[i+4,]$Intensity..dB. - dataframe[i+3,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,6] <- (dataframe[i+5,]$Intensity..dB. - dataframe[i+4,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    cont = cont + 1
    
  }
  
  performers <- deparse(substitute(performerName))
  
  performerName <- cbind(newdf,newdf2)
  performerName <- as.data.frame(performerName)
  performerName$performer <- performers
  
  return(performerName)
}

### Op. 38i crotches

CH <- crotchet_fun38i(CyH58crotchets,CH)
DPB <- crotchet_fun38i(DPyBcrotchets,DPB)
FB <- crotchet_fun38i(FyBcrotchets,FB)
FvdP <- crotchet_fun38i(FyvdPcrotchets,FvdP)
GG <- crotchet_fun38i(GyGcrotchets,GG)
GGr <- crotchet_fun38i(GyGrcrotchets,GGr)
GGu <- crotchet_fun38i(GyGucrotchets,GGu)
IH <- crotchet_fun38i(IyHcrotchets,IH)
MA <- crotchet_fun38i(MyAcrotchets,MA)
MG <- crotchet_fun38i(MyGcrotchets,MG)
ML <- crotchet_fun38i(MyLcrotchets,ML)
MP <- crotchet_fun38i(MyPcrotchets,MP)
PN <- crotchet_fun38i(PyNcrotchets,PN)
PR36 <- crotchet_fun38i(PyR36crotchets,PR36)
PR66 <- crotchet_fun38i(PyR66crotchets,PR66)
PS <- crotchet_fun38i(PyScrotchets,PS)
PV <- crotchet_fun38i(PyVcrotchets,PV)
RS <- crotchet_fun38i(RyScrotchets,RS)
SB <- crotchet_fun38i(SyBcrotchets,SB)
SS <- crotchet_fun38i(SyScrotchets,SS)

crotches38i <- rbind(CH,DPB,FB,FvdP,GG,  GGr,GGu,IH,MA,MG,  ML,MP,PR36,PR66,PS,  PN,PV,RS,SB,SS)

# Tidying the crotchet 38i dataset

crotches38i$beat_1 <- crotches38i$V1
crotches38i$beat_2 <- crotches38i$V2
crotches38i$beat_3 <- crotches38i$V3
crotches38i$beat_4 <- crotches38i$V4

crotches38i$intensity_1 <- crotches38i$V5
crotches38i$intensity_2 <- crotches38i$V6
crotches38i$intensity_3 <- crotches38i$V7
crotches38i$intensity_4 <- crotches38i$V8

crotches38i$V1<-NULL
crotches38i$V2<-NULL
crotches38i$V3<-NULL
crotches38i$V4<-NULL
crotches38i$V5<-NULL
crotches38i$V6<-NULL
crotches38i$V7<-NULL
crotches38i$V8<-NULL

write.csv(crotches38i, "C:/Users/samte/OneDrive/Desktop/crotches38i.csv", row.names=FALSE)

### Op. 38ii quavers

CH <- quaver_fun38ii(CyH58quavers,CH)
DPB <- quaver_fun38ii(DPyBquavers,DPB)
FB <- quaver_fun38ii(FyBquavers,FB)
FvdP <- quaver_fun38ii(FyvdPquavers,FvdP)
GG <- quaver_fun38ii(GyGquavers,GG)
GGr <- quaver_fun38ii(GyGrquavers,GGr)
GGu <- quaver_fun38ii(GyGuquavers,GGu)
IH <- quaver_fun38ii(IyHquavers,IH)
MA <- quaver_fun38ii(MyAquavers,MA)
MG <- quaver_fun38ii(MyGquavers,MG)
ML <- quaver_fun38ii(MyLquavers,ML)
MP <- quaver_fun38ii(MyPquavers,MP)
PN <- quaver_fun38ii(PyNquavers,PN)
PR36 <- quaver_fun38ii(PyR36quavers,PR36)
PR66 <- quaver_fun38ii(PyR66quavers,PR66)
PS <- quaver_fun38ii(PySquavers,PS)
PV <- quaver_fun38ii(PyVquavers,PV)
RS <- quaver_fun38ii(RySquavers,RS)
SB <- quaver_fun38ii(SyBquavers,SB)
SS <- quaver_fun38ii(SySquavers,SS)


quavers38ii <- rbind(CH,DPB,FB,FvdP,GG,  GGr,GGu,IH,MA,MG,  ML,MP,PR36,PR66,PS,  PN,PV,RS,SB,SS)


# Tidying the 38ii quaver dataset

quavers38ii$beat_1 <- quavers38ii$V1
quavers38ii$beat_2 <- quavers38ii$V2
quavers38ii$beat_3 <- quavers38ii$V3
quavers38ii$beat_4 <- quavers38ii$V4
quavers38ii$beat_5 <- quavers38ii$V5
quavers38ii$beat_6 <- quavers38ii$V6

quavers38ii$intensity_1 <- quavers38ii$V7
quavers38ii$intensity_2 <- quavers38ii$V8
quavers38ii$intensity_3 <- quavers38ii$V9
quavers38ii$intensity_4 <- quavers38ii$V10
quavers38ii$intensity_5 <- quavers38ii$V11
quavers38ii$intensity_6 <- quavers38ii$V12

quavers38ii$V1<-NULL
quavers38ii$V2<-NULL
quavers38ii$V3<-NULL
quavers38ii$V4<-NULL
quavers38ii$V5<-NULL
quavers38ii$V6<-NULL
quavers38ii$V7<-NULL
quavers38ii$V8<-NULL
quavers38ii$V9<-NULL
quavers38ii$V10<-NULL
quavers38ii$V11<-NULL
quavers38ii$V12<-NULL

write.csv(quavers38ii, "C:/Users/samte/OneDrive/Desktop/quavers38ii.csv", row.names=FALSE)

# random data analysis
library(ggfortify)
x<-prcomp(crotches38i[,c(1:8)], center=T, scale=T)
x
summary(x)
autoplot(x, data=crotches38i, colour='performer')

#crotches38i$performer <- as.factor(crotches38i$performer)
trainRows=createDataPartition(crotches38i$performer,p=0.8,list=F)
trainData=crotches38i[trainRows,]
testData=crotches38i[-trainRows,]

# testData <- r[c(363:370,643:650,923:930,1203:1210,1483:1490,1763:1770,2043:2050),]
# trainData <- r[-c(363:370,643:650,923:930,1203:1210,1483:1490,1763:1770,2043:6680),]
# 
testData$performer <- as.factor(testData$performer)
trainData$performer <- as.factor(trainData$performer)


model <- randomForest::randomForest(performer~., data = trainData, ntree=2000)
summary(model)
lpred <- predict(model, newdata = testData)
confusionMatrix(lpred,testData$performer)

# model <- glm(performer~., data=trainData)
# lprob <- predict(model, newdata = testData)


### Prueba (cada fila es una "frase")

cont<-0
new <- matrix(, nrow =835, ncol=65)
for (i in 1:835){
  for (j in 1:64){
    if (j<=8){
    new[i,j] <- as.numeric(crotches38i[i+7*cont,j])
    new[i,j] <-as.numeric(new[i,j])
    
    }
    if(8<j&& j<=16){
      new[i,j] <-crotches38i[i+1+7*cont,j-8]
      new[i,j] <-as.numeric(new[i,j])
    }
    if(16<j&& j<=24){
      new[i,j] <-as.numeric(crotches38i[i+2+7*cont,j-2*8])
      new[i,j] <-as.numeric(new[i,j])
    }
    if(24<j&& j<=32){
      new[i,j] <-as.numeric(crotches38i[i+3+7*cont,j-3*8])
      new[i,j] <-as.numeric(new[i,j])
    }
    if(32<j&& j<=40){
      new[i,j] <-as.numeric(crotches38i[i+4+7*cont,j-4*8])
      new[i,j] <-as.numeric(new[i,j])
    }
    if(40<j&& j<=48){
      new[i,j] <-as.numeric(crotches38i[i+5+7*cont,j-5*8])
      new[i,j] <-as.numeric(new[i,j])
    }
    if(48<j&& j<=56){
      new[i,j] <-as.numeric(crotches38i[i+6+7*cont,j-6*8])
      new[i,j] <-as.numeric(new[i,j])
    }
    if(56<j&& j<=64){
      new[i,j] <-as.numeric(crotches38i[i+7+7*cont,j-7*8])
      new[i,j] <-as.numeric(new[i,j])
      new[i,65] <- as.character(crotches38i[i+7+7*cont,]$performer)
    }
  }
  cont = cont+1
}


crophrases<- as.data.frame(new)

for (i in 1:64){
  crophrases[,i]<-as.numeric(crophrases[,i])
}


x<-prcomp(crophrases[,c(1:64)], center=T, scale=T)
x
summary(x)
autoplot(x, data=crophrases, colour='V65')

crophrases$V65 <- as.factor(crophrases$V65)
trainRows=createDataPartition(crophrases$V65,p=0.8,list=F)
trainData=crophrases[trainRows,]
testData=crophrases[-trainRows,]

model <- randomForest::randomForest(V65~., data = trainData, ntree=2000)
summary(model)
lpred <- predict(model, newdata = testData)
confusionMatrix(lpred,testData$V65)


