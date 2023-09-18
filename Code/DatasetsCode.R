# Clustering of musical patterns

## This script contains the code that created the datasets:
## crotchets38i, quavers38ii, crotchets38iii, crotchets99i, semiquavers99ii

### Libraries

library(plyr)
library(readr)
library(caret)
library(ggfortify)


# Loading the raw datasets

# Op. 38
# 20 recordings

#setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 38i")
#setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 38ii")
setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 38iii")

mydir = as.vector(c("C&H(58)","DP&B","F&B","F&vdP","G&G","G&Gr","G&Gu",
                    "I&H","M&A","M&G","M&L","M&P","P&N","P&R(36)","P&R(66)",
                    "P&S","P&V","R&S","S&B","S&S"))

myname = as.vector(c("CyH58","DPyB","FyB","FyvdP","GyG","GyGr","GyGu",
                     "IyH","MyA","MyG","MyL","MyP","PyN","PyR36","PyR66",
                     "PyS","PyV","RyS","SyB","SyS"))

#mytype = as.vector(c("bars","crotchets","minims","notes.cello","notes.piano"))
#mytype = as.vector(c("bars","crotchets","notes.cello","notes.piano","quavers"))
mytype = as.vector(c("bars","crotchets","minims","notes.cello","notes.piano"))

for (i in 1:length(mydir)){
  midir <- mydir[i]
  miname <- myname[i]
  myfiles = list.files(path=midir, pattern="*.csv", full.names=TRUE)
  
  for (j in c(2)){
    type <- mytype[j]
    assign(paste0(miname,type), read.csv(myfiles[j],sep=";")) 
    
  }
}


# Op 99
# 15 recordings

#setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 99i")
setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 99ii")

mydir = as.vector(c("C&H(36)","DP&B","F&B","G&G","G&Gu",
                    "I&H","M&A","M&G","M&L","P&N","P&R(66)",
                    "P&V","R&S","S&B","S&S"))

myname = as.vector(c("CyH36","DPyB","FyB","GyG","GyGu",
                     "IyH","MyA","MyG","MyL","PyN","PyR66",
                     "PyV","RyS","SyB","SyS"))

#mytype = as.vector(c("bars","crotchets","notes.cello","notes.piano"))
mytype = as.vector(c("bars","crotchets","notes.cello","notes.piano","quavers","semiquavers"))


for (i in 1:length(mydir)){
  midir <- mydir[i]
  miname <-myname[i]
  myfiles = list.files(path=midir, pattern="*.csv", full.names=TRUE)
  
  for (j in c(5)){
    type <- mytype[j]
    assign(paste0(miname,type), read.csv(myfiles[j],sep=";")) 
    
  }
  
}

# Tidying functions for bar datasets

crotchet_fun <- function(dataframe, performerName){
  
  cont <- 0
  newdf <- matrix(,ncol=4,nrow=floor(dim(dataframe)[1]/4))
  for (i in seq(1,dim(dataframe)[1] - 1, by=4)){
    newdf[i-3*cont,1] <- dataframe[i,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    newdf[i-3*cont,2] <- dataframe[i+1,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    newdf[i-3*cont,3] <- dataframe[i+2,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    newdf[i-3*cont,4] <- dataframe[i+3,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.)
    cont = cont + 1
  }
  
  
  newdf2 <- matrix(,ncol=5,nrow=floor(dim(dataframe)[1]/4))
  cont <- 1
  
  newdf2[1,1] <- 0
  newdf2[1,2] <- (dataframe[2,]$Intensity..dB. - dataframe[1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,3] <- (dataframe[3,]$Intensity..dB. - dataframe[2,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,4] <- (dataframe[4,]$Intensity..dB. - dataframe[3,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,5] <- floor(dataframe[2,]$Bar.Crotchet)
  
  for (i in seq(5,dim(dataframe)[1] - 1, by=4)){
    
    
    newdf2[i-3*cont,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-3*cont,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-3*cont,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-3*cont,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-3*cont,5] <- floor(dataframe[i+1,]$Bar.Crotchet)
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
  
  
  newdf2 <- matrix(,ncol=7,nrow=(dim(dataframe)[1]-6)/6)
  cont<-0
  for (i in seq(3,(dim(dataframe)[1]-6), by=6)){
    
    
    newdf2[i-5*cont-2,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-5*cont-2,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-5*cont-2,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-5*cont-2,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-5*cont-2,5] <- (dataframe[i+4,]$Intensity..dB. - dataframe[i+3,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-5*cont-2,6] <- (dataframe[i+5,]$Intensity..dB. - dataframe[i+4,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-5*cont-2,7] <- floor(dataframe[i+1,]$Bar.Quaver)
    cont = cont + 1
    
  }
  
  performers <- deparse(substitute(performerName))
  
  performerName <- cbind(newdf,newdf2)
  performerName <- as.data.frame(performerName)
  performerName$performer <- performers
  
  return(performerName)
}


crotchet_fun99i <- function(dataframe, performerName){
  
  cont <- 0
  newdf <- matrix(,ncol=3,nrow=floor(dim(dataframe)[1]/3))
  for (i in seq(1,dim(dataframe)[1] - 1, by=3)){
    newdf[i-2*cont,1] <- dataframe[i,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s.)
    newdf[i-2*cont,2] <- dataframe[i+1,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s.)
    newdf[i-2*cont,3] <- dataframe[i+2,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s.)
    cont = cont + 1
  }
  
  
  newdf2 <- matrix(,ncol=4,nrow=floor(dim(dataframe)[1]/3))
  cont <- 1
  
  newdf2[1,1] <- 0
  newdf2[1,2] <- (dataframe[2,]$Intensity..dB. - dataframe[1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,3] <- (dataframe[3,]$Intensity..dB. - dataframe[2,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,4] <- floor(dataframe[2,]$Bar.Crotchet)
  
  for (i in seq(4,dim(dataframe)[1] - 1, by=3)){
    
    
    newdf2[i-2*cont,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-2*cont,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-2*cont,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-2*cont,4] <- floor(dataframe[i+1,]$Bar.Crotchet)
    cont = cont + 1
    
  }
  performers <- deparse(substitute(performerName))
  
  performerName <- cbind(newdf,newdf2)
  performerName <- as.data.frame(performerName)
  performerName$performer <- performers
  
  return(performerName)
  
}

semiquaver_fun99ii <- function(dataframe, performerName){
  cont <- 0
  newdf <- matrix(,ncol=8,nrow=floor(dim(dataframe)[1]/8))
  
  for (i in seq(1, dim(dataframe)[1] - 1, by=8)){
    
    newdf[i-7*cont,1] <- dataframe[i,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                     +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s.)
    newdf[i-7*cont,2] <- dataframe[i+1,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s.)
    newdf[i-7*cont,3] <- dataframe[i+2,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s.)
    newdf[i-7*cont,4] <- dataframe[i+3,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s.)
    newdf[i-7*cont,5] <- dataframe[i+4,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s.)
    newdf[i-7*cont,6] <- dataframe[i+5,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s.)
    newdf[i-7*cont,7] <- dataframe[i+6,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s. )
    newdf[i-7*cont,8] <- dataframe[i+7,]$Duration..s./(dataframe[i,]$Duration..s. + dataframe[i+1,]$Duration..s. + dataframe[i+2,]$Duration..s. + dataframe[i+3,]$Duration..s.
                                                       +dataframe[i+4,]$Duration..s. + dataframe[i+5,]$Duration..s. + dataframe[i+6,]$Duration..s. + dataframe[i+7,]$Duration..s. )
    cont = cont + 1
  }
  
  
  newdf2 <- matrix(,ncol=9,nrow=floor(dim(dataframe)[1]/8))
  cont <- 1
  
  newdf2[1,1] <- 0
  newdf2[1,2] <- (dataframe[2,]$Intensity..dB. - dataframe[1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,3] <- (dataframe[3,]$Intensity..dB. - dataframe[2,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,4] <- (dataframe[4,]$Intensity..dB. - dataframe[3,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,5] <- (dataframe[5,]$Intensity..dB. - dataframe[4,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,6] <- (dataframe[6,]$Intensity..dB. - dataframe[5,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,7] <- (dataframe[7,]$Intensity..dB. - dataframe[6,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,8] <- (dataframe[8,]$Intensity..dB. - dataframe[7,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
  newdf2[1,9] <- floor(dataframe[2,]$Bar.Semiquaver)
  
  for (i in seq(9,dim(dataframe)[1] - 1, by=8)){
    
    
    newdf2[i-7*cont,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,5] <- (dataframe[i+4,]$Intensity..dB. - dataframe[i+3,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,6] <- (dataframe[i+5,]$Intensity..dB. - dataframe[i+4,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,7] <- (dataframe[i+6,]$Intensity..dB. - dataframe[i+5,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,8] <- (dataframe[i+7,]$Intensity..dB. - dataframe[i+6,]$Intensity..dB.)/(max(dataframe$Intensity..dB.) - min(dataframe$Intensity..dB.))
    newdf2[i-7*cont,9] <- floor(dataframe[i+1,]$Bar.Semiquaver)
    cont = cont + 1
    
  }
  
  performers <- deparse(substitute(performerName))
  
  performerName <- cbind(newdf,newdf2)
  performerName <- as.data.frame(performerName)
  performerName$performer <- performers
  
  return(performerName)
}

### Op. 38i crotchets

CH <- crotchet_fun(CyH58crotchets,CH)
DPB <- crotchet_fun(DPyBcrotchets,DPB)
FB <- crotchet_fun(FyBcrotchets,FB)
FvdP <- crotchet_fun(FyvdPcrotchets,FvdP)
GG <- crotchet_fun(GyGcrotchets,GG)
GGr <- crotchet_fun(GyGrcrotchets,GGr)
GGu <- crotchet_fun(GyGucrotchets,GGu)
IH <- crotchet_fun(IyHcrotchets,IH)
MA <- crotchet_fun(MyAcrotchets,MA)
MG <- crotchet_fun(MyGcrotchets,MG)
ML <- crotchet_fun(MyLcrotchets,ML)
MP <- crotchet_fun(MyPcrotchets,MP)
PN <- crotchet_fun(PyNcrotchets,PN)
PR36 <- crotchet_fun(PyR36crotchets,PR36)
PR66 <- crotchet_fun(PyR66crotchets,PR66)
PS <- crotchet_fun(PyScrotchets,PS)
PV <- crotchet_fun(PyVcrotchets,PV)
RS <- crotchet_fun(RyScrotchets,RS)
SB <- crotchet_fun(SyBcrotchets,SB)
SS <- crotchet_fun(SyScrotchets,SS)

crotchets38i <- rbind(CH,DPB,FB,FvdP,GG,  GGr,GGu,IH,MA,MG,  ML,MP,PR36,PR66,PS,  PN,PV,RS,SB,SS)

# Tidying the crotchet 38i dataset

crotchets38i$beat_1 <- crotchets38i$V1
crotchets38i$beat_2 <- crotchets38i$V2
crotchets38i$beat_3 <- crotchets38i$V3
crotchets38i$beat_4 <- crotchets38i$V4

crotchets38i$intensity_1 <- crotchets38i$V5
crotchets38i$intensity_2 <- crotchets38i$V6
crotchets38i$intensity_3 <- crotchets38i$V7
crotchets38i$intensity_4 <- crotchets38i$V8
crotchets38i$bar <- crotchets38i$V9

crotchets38i$V1<-NULL
crotchets38i$V2<-NULL
crotchets38i$V3<-NULL
crotchets38i$V4<-NULL
crotchets38i$V5<-NULL
crotchets38i$V6<-NULL
crotchets38i$V7<-NULL
crotchets38i$V8<-NULL
crotchets38i$V9<-NULL

crotchets38i$bar <- as.factor(crotchets38i$bar)

write.csv(crotchets38i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/crotchets38i.csv", row.names=FALSE)

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
quavers38ii$bar <- quavers38ii$V13

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
quavers38ii$V13<-NULL

quavers38ii$bar <- as.factor(quavers38ii$bar)

write.csv(quavers38ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/quavers38ii.csv", row.names=FALSE)


### Op. 38iii crotchets

CH <- crotchet_fun(CyH58crotchets,CH)
DPB <- crotchet_fun(DPyBcrotchets,DPB)
FB <- crotchet_fun(FyBcrotchets,FB)
FvdP <- crotchet_fun(FyvdPcrotchets,FvdP)
GG <- crotchet_fun(GyGcrotchets,GG)
GGr <- crotchet_fun(GyGrcrotchets,GGr)
GGu <- crotchet_fun(GyGucrotchets,GGu)
IH <- crotchet_fun(IyHcrotchets,IH)
MA <- crotchet_fun(MyAcrotchets,MA)
MG <- crotchet_fun(MyGcrotchets,MG)
ML <- crotchet_fun(MyLcrotchets,ML)
MP <- crotchet_fun(MyPcrotchets,MP)
PN <- crotchet_fun(PyNcrotchets,PN)
PR36 <- crotchet_fun(PyR36crotchets,PR36)
PR66 <- crotchet_fun(PyR66crotchets,PR66)
PS <- crotchet_fun(PyScrotchets,PS)
PV <- crotchet_fun(PyVcrotchets,PV)
RS <- crotchet_fun(RyScrotchets,RS)
SB <- crotchet_fun(SyBcrotchets,SB)
SS <- crotchet_fun(SyScrotchets,SS)

crotchets38iii <- rbind(CH,DPB,FB,FvdP,GG,  GGr,GGu,IH,MA,MG,  ML,MP,PR36,PR66,PS,  PN,PV,RS,SB,SS)

# Tidying the crotchet 38iii dataset

crotchets38iii$beat_1 <- crotchets38iii$V1
crotchets38iii$beat_2 <- crotchets38iii$V2
crotchets38iii$beat_3 <- crotchets38iii$V3
crotchets38iii$beat_4 <- crotchets38iii$V4

crotchets38iii$intensity_1 <- crotchets38iii$V5
crotchets38iii$intensity_2 <- crotchets38iii$V6
crotchets38iii$intensity_3 <- crotchets38iii$V7
crotchets38iii$intensity_4 <- crotchets38iii$V8
crotchets38iii$bar <- crotchets38iii$V9

crotchets38iii$V1<-NULL
crotchets38iii$V2<-NULL
crotchets38iii$V3<-NULL
crotchets38iii$V4<-NULL
crotchets38iii$V5<-NULL
crotchets38iii$V6<-NULL
crotchets38iii$V7<-NULL
crotchets38iii$V8<-NULL
crotchets38iii$V9<-NULL

crotchets38iii$bar <- as.factor(crotchets38iii$bar)

write.csv(crotchets38iii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/crotchets38iii.csv", row.names=FALSE)


### Op. 99i crotchets

CH36 <- crotchet_fun99i(CyH36crotchets,CH36)
DPB <- crotchet_fun99i(DPyBcrotchets,DPB)
FB <- crotchet_fun99i(FyBcrotchets,FB)
GG <- crotchet_fun99i(GyGcrotchets,GG)
GGu <- crotchet_fun99i(GyGucrotchets,GGu)

IH <- crotchet_fun99i(IyHcrotchets,IH)
MA <- crotchet_fun99i(MyAcrotchets,MA)
MG <- crotchet_fun99i(MyGcrotchets,MG)
ML <- crotchet_fun99i(MyLcrotchets,ML)
PN <- crotchet_fun99i(PyNcrotchets,PN)

PR66 <- crotchet_fun99i(PyR66crotchets,PR66)
PV <- crotchet_fun99i(PyVcrotchets,PV)
RS <- crotchet_fun99i(RyScrotchets,RS)
SB <- crotchet_fun99i(SyBcrotchets,SB)
SS <- crotchet_fun99i(SyScrotchets,SS)

FB<-rbind(FB[1:65,],FB[1:65,],FB[66:210,])

crotchets99i <- rbind(CH36,DPB,FB,GG,GGu,  IH,MA,MG,ML,PR66,  PN,PV,RS,SB,SS)

# Tidying the crotchet 99i dataset

crotchets99i$beat_1 <- crotchets99i$V1
crotchets99i$beat_2 <- crotchets99i$V2
crotchets99i$beat_3 <- crotchets99i$V3

crotchets99i$intensity_1 <- crotchets99i$V4
crotchets99i$intensity_2 <- crotchets99i$V5
crotchets99i$intensity_3 <- crotchets99i$V6

crotchets99i$bar <- crotchets99i$V7

crotchets99i$V1<-NULL
crotchets99i$V2<-NULL
crotchets99i$V3<-NULL
crotchets99i$V4<-NULL
crotchets99i$V5<-NULL
crotchets99i$V6<-NULL
crotchets99i$V7<-NULL

crotchets99i$bar <- as.factor(crotchets99i$bar)

write.csv(crotchets99i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/crotchets99i.csv", row.names=FALSE)

### Op. 99ii semiquavers

CH36 <- semiquaver_fun99ii(CyH36semiquavers,CH36)
DPB <- semiquaver_fun99ii(DPyBsemiquavers,DPB)
FB <- semiquaver_fun99ii(FyBsemiquavers,FB)
GG <- semiquaver_fun99ii(GyGsemiquavers,GG)
GGu <- semiquaver_fun99ii(GyGusemiquavers,GGu)

IH <- semiquaver_fun99ii(IyHsemiquavers,IH)
MA <- semiquaver_fun99ii(MyAsemiquavers,MA)
MG <- semiquaver_fun99ii(MyGsemiquavers,MG)
ML <- semiquaver_fun99ii(MyLsemiquavers,ML)
PN <- semiquaver_fun99ii(PyNsemiquavers,PN)

PR66 <- semiquaver_fun99ii(PyR66semiquavers,PR66)
PV <- semiquaver_fun99ii(PyVsemiquavers,PV)
RS <- semiquaver_fun99ii(RySsemiquavers,RS)
SB <- semiquaver_fun99ii(SyBsemiquavers,SB)
SS <- semiquaver_fun99ii(SySsemiquavers,SS)

semiquavers99ii <- rbind(CH36,DPB,FB,GG,GGu,  IH,MA,MG,ML,PR66,  PN,PV,RS,SB,SS)


# Tidying the 99ii semiquaver dataset

semiquavers99ii$beat_1 <- semiquavers99ii$V1
semiquavers99ii$beat_2 <- semiquavers99ii$V2
semiquavers99ii$beat_3 <- semiquavers99ii$V3
semiquavers99ii$beat_4 <- semiquavers99ii$V4
semiquavers99ii$beat_5 <- semiquavers99ii$V5
semiquavers99ii$beat_6 <- semiquavers99ii$V6
semiquavers99ii$beat_7 <- semiquavers99ii$V7
semiquavers99ii$beat_8 <- semiquavers99ii$V8

semiquavers99ii$intensity_1 <- semiquavers99ii$V9
semiquavers99ii$intensity_2 <- semiquavers99ii$V10
semiquavers99ii$intensity_3 <- semiquavers99ii$V11
semiquavers99ii$intensity_4 <- semiquavers99ii$V12
semiquavers99ii$intensity_5 <- semiquavers99ii$V13
semiquavers99ii$intensity_6 <- semiquavers99ii$V14
semiquavers99ii$intensity_7 <- semiquavers99ii$V15
semiquavers99ii$intensity_8 <- semiquavers99ii$V16
semiquavers99ii$bar <- semiquavers99ii$V17

semiquavers99ii$V1<-NULL
semiquavers99ii$V2<-NULL
semiquavers99ii$V3<-NULL
semiquavers99ii$V4<-NULL
semiquavers99ii$V5<-NULL
semiquavers99ii$V6<-NULL
semiquavers99ii$V7<-NULL
semiquavers99ii$V8<-NULL
semiquavers99ii$V9<-NULL
semiquavers99ii$V10<-NULL
semiquavers99ii$V11<-NULL
semiquavers99ii$V12<-NULL
semiquavers99ii$V13<-NULL
semiquavers99ii$V14<-NULL
semiquavers99ii$V15<-NULL
semiquavers99ii$V16<-NULL
semiquavers99ii$V17<-NULL

semiquavers99ii$bar <- as.factor(semiquavers99ii$bar)

write.csv(semiquavers99ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/semiquavers99ii.csv", row.names=FALSE)


# Datasets of rows of performers

## Op. 38i

### columns of performers 38i

vc38i <- matrix(,nrow=2240,ncol=20)
var <-0
for (k in c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")){
  var=var+1
  dataframe <- crotchets38i[crotchets38i$performer == k,]
  if (dim(dataframe)[1]>300){
    dataframe <- dataframe[91:370,]
  }
  
  for(j in 1:280){
    for (i in 1:8){
      vc38i[i+8*(j-1),var]<-dataframe[j,i+1]
      
    }
  }
}


vc38i <- as.data.frame(vc38i)

colnames(vc38i)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                       "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(vc38i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/vc38i.csv", row.names=FALSE)

### rows of performers

tc38i <- t(vc38i)
tc38i <- as.data.frame(tc38i)
tc38i$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                      "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(tc38i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tc38i.csv", row.names=FALSE)

### beats and intensity

v <- numeric(1120)
for (i in seq(1,560,by=2)){
  v[1+2*(i-1)] <- 1+4*(i-1)
  v[2+2*(i-1)] <- 2+4*(i-1)
  v[3+2*(i-1)] <- 3+4*(i-1)
  v[4+2*(i-1)] <- 4+4*(i-1)
  
}

tbeats <- tc38i[,v]
tintensity <- tc38i[,-v]
tbeats$performers <- tintensity$performers

write.csv(tbeats, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats.csv", row.names=FALSE)
write.csv(tintensity, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity.csv", row.names=FALSE)



# Percentage of time that each beat takes per performer
beatper38i<-rbind(CyH58crotchets[c(1:360,721:1480),]$Duration..s./sum(CyH58crotchets[c(1:360,721:1480),]$Duration..s./100), 
                  DPyBcrotchets[1:1120,]$Duration..s./sum(DPyBcrotchets[1:1120,]$Duration..s./100),
                  FyBcrotchets[1:1120,]$Duration..s./sum(FyBcrotchets[1:1120,]$Duration..s./100),
                  FyvdPcrotchets[1:1120,]$Duration..s./sum(FyvdPcrotchets[1:1120,]$Duration..s./100),
                  GyGcrotchets[c(1:360,721:1480),]$Duration..s./sum(GyGcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  GyGrcrotchets[c(1:360,721:1480),]$Duration..s./sum(GyGrcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  GyGucrotchets[c(1:360,721:1480),]$Duration..s./sum(GyGucrotchets[c(1:360,721:1480),]$Duration..s./100),
                  IyHcrotchets[c(1:360,721:1480),]$Duration..s./sum(IyHcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  MyAcrotchets[1:1120,]$Duration..s./sum(MyAcrotchets[1:1120,]$Duration..s./100),
                  MyGcrotchets[c(1:360,721:1480),]$Duration..s./sum(MyGcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  MyLcrotchets[c(1:360,721:1480),]$Duration..s./sum(MyLcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  MyPcrotchets[c(1:360,721:1480),]$Duration..s./sum(MyPcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  PyNcrotchets[c(1:360,721:1480),]$Duration..s./sum(PyNcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  PyR36crotchets[1:1120,]$Duration..s./sum(PyR36crotchets[1:1120,]$Duration..s./100),
                  PyR66crotchets[1:1120,]$Duration..s./sum(PyR66crotchets[1:1120,]$Duration..s./100),
                  PyScrotchets[1:1120,]$Duration..s./sum(PyScrotchets[1:1120,]$Duration..s/100),
                  PyVcrotchets[c(1:360,721:1480),]$Duration..s./sum(PyVcrotchets[c(1:360,721:1480),]$Duration..s./100),
                  RyScrotchets[c(1:360,721:1480),]$Duration..s./sum(RyScrotchets[c(1:360,721:1480),]$Duration..s./100),
                  SyBcrotchets[1:1120,]$Duration..s./sum(SyBcrotchets[1:1120,]$Duration..s./100),
                  SyScrotchets[c(1:360,721:1480),]$Duration..s./sum(SyScrotchets[c(1:360,721:1480),]$Duration..s./100))

beatper38i<- as.data.frame(beatper38i)
rownames(beatper38i) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                          "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
beatper38i$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                           "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(beatper38i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38i.csv", row.names=FALSE)


# Intensity percentage with respect to the maximum intensity of the performance that each beat takes
intper38i<-rbind((CyH58crotchets[c(1:360,721:1480),]$Intensity..dB. - min(CyH58crotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(CyH58crotchets[c(1:360,721:1480),]$Intensity..dB. )- min(CyH58crotchets[c(1:360,721:1480),]$Intensity..dB. )), 
                 (DPyBcrotchets[1:1120,]$Intensity..dB.- min(DPyBcrotchets[1:1120,]$Intensity..dB. ))/(max(DPyBcrotchets[1:1120,]$Intensity..dB. )- min(DPyBcrotchets[1:1120,]$Intensity..dB. )),
                 (FyBcrotchets[1:1120,]$Intensity..dB.- min(FyBcrotchets[1:1120,]$Intensity..dB. ))/(max(FyBcrotchets[1:1120,]$Intensity..dB. )- min(FyBcrotchets[1:1120,]$Intensity..dB. )),
                 (FyvdPcrotchets[1:1120,]$Intensity..dB.- min(FyvdPcrotchets[1:1120,]$Intensity..dB. ))/(max(FyvdPcrotchets[1:1120,]$Intensity..dB. )- min(FyvdPcrotchets[1:1120,]$Intensity..dB. )),
                 (GyGcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(GyGcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(GyGcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(GyGcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                  
                 (GyGrcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(GyGrcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(GyGrcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(GyGrcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (GyGucrotchets[c(1:360,721:1480),]$Intensity..dB. - min(GyGucrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(GyGucrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(GyGucrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (IyHcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(IyHcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(IyHcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(IyHcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (MyAcrotchets[1:1120,]$Intensity..dB.- min(MyAcrotchets[1:1120,]$Intensity..dB. ))/(max(MyAcrotchets[1:1120,]$Intensity..dB. )- min(MyAcrotchets[1:1120,]$Intensity..dB. )),
                 (MyGcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(MyGcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(MyGcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(MyGcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                  
                 (MyLcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(MyLcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(MyLcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(MyLcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (MyPcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(MyPcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(MyPcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(MyPcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (PyNcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(PyNcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(PyNcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(PyNcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (PyR36crotchets[1:1120,]$Intensity..dB.- min(PyR36crotchets[1:1120,]$Intensity..dB. ))/(max(PyR36crotchets[1:1120,]$Intensity..dB. )- min(PyR36crotchets[1:1120,]$Intensity..dB. )),
                 (PyR66crotchets[1:1120,]$Intensity..dB.- min(PyR66crotchets[1:1120,]$Intensity..dB. ))/(max(PyR66crotchets[1:1120,]$Intensity..dB. )- min(PyR66crotchets[1:1120,]$Intensity..dB. )),
                  
                 (PyScrotchets[1:1120,]$Intensity..dB.- min(PyScrotchets[1:1120,]$Intensity..dB. ))/(max(PyScrotchets[1:1120,]$Intensity..dB. )- min(PyScrotchets[1:1120,]$Intensity..dB. )),
                 (PyVcrotchets[c(1:360,721:1480),]$Intensity..dB. - min(PyVcrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(PyVcrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(PyVcrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (RyScrotchets[c(1:360,721:1480),]$Intensity..dB. - min(RyScrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(RyScrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(RyScrotchets[c(1:360,721:1480),]$Intensity..dB. )),
                 (SyBcrotchets[1:1120,]$Intensity..dB.- min(SyBcrotchets[1:1120,]$Intensity..dB. ))/(max(SyBcrotchets[1:1120,]$Intensity..dB. )- min(SyBcrotchets[1:1120,]$Intensity..dB. )),
                 (SyScrotchets[c(1:360,721:1480),]$Intensity..dB. - min(SyScrotchets[c(1:360,721:1480),]$Intensity..dB. ))/(max(SyScrotchets[c(1:360,721:1480),]$Intensity..dB. )- min(SyScrotchets[c(1:360,721:1480),]$Intensity..dB. )))

intper38i<- as.data.frame(intper38i)
rownames(intper38i) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                          "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
intper38i$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                           "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(intper38i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38i.csv", row.names=FALSE)



###
# Op. 38ii
###

vc38ii <- matrix(,nrow=220*12,ncol=20)
var <-0
for (k in c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")){
  var=var+1
  dataframe <- quavers38ii[quavers38ii$performer == k,]
  if (dim(dataframe)[1]<200){
    dataframe <- dataframe[c(1:89,79:108,90:190),]
  }
  
  for(j in 1:220){
    for (i in 1:12){
      vc38ii[i+12*(j-1),var]<-dataframe[j,i+1]
      
    }
  }
}


vc38ii <- as.data.frame(vc38ii)
colnames(vc38ii) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                      "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")


write.csv(vc38ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/vc38ii.csv", row.names=FALSE)

### rows of performers

tc38ii <- t(vc38ii)
tc38ii <- as.data.frame(tc38ii)
tc38ii$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                       "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(tc38ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tc38ii.csv", row.names=FALSE)

### beats and intensity

v <- numeric(1320)
for (i in seq(1,440,by=2)){
  v[1+3*(i-1)] <- 1+6*(i-1)
  v[2+3*(i-1)] <- 2+6*(i-1)
  v[3+3*(i-1)] <- 3+6*(i-1)
  v[4+3*(i-1)] <- 4+6*(i-1)
  v[5+3*(i-1)] <- 5+6*(i-1)
  v[6+3*(i-1)] <- 6+6*(i-1)
  
}

tbeats38ii <- tc38ii[,v]
tintensity38ii <- tc38ii[,-v]
tbeats38ii$performers <- tintensity38ii$performers

write.csv(tbeats38ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats38ii.csv", row.names=FALSE)
write.csv(tintensity38ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity38ii.csv", row.names=FALSE)

## Beats

beatper38ii<-rbind(CyH58quavers$Duration..s./sum(CyH58quavers$Duration..s./100), 
                   DPyBquavers$Duration..s./sum(DPyBquavers$Duration..s./100),
                   FyBquavers$Duration..s./sum(FyBquavers$Duration..s./100),
                   FyvdPquavers[c(1:536,471:650,537:1146),]$Duration..s./sum(FyvdPquavers[c(1:536,471:650,537:1146),]$Duration..s./100),
                   GyGquavers$Duration..s./sum(GyGquavers$Duration..s./100),
                   GyGrquavers$Duration..s./sum(GyGrquavers$Duration..s./100),
                   GyGuquavers$Duration..s./sum(GyGuquavers$Duration..s./100),
                   IyHquavers$Duration..s./sum(IyHquavers$Duration..s./100),
                   MyAquavers$Duration..s./sum(MyAquavers$Duration..s./100),
                   MyGquavers$Duration..s./sum(MyGquavers$Duration..s./100),
                   MyLquavers$Duration..s./sum(MyLquavers$Duration..s./100),
                   MyPquavers$Duration..s./sum(MyPquavers$Duration..s./100),
                   PyNquavers$Duration..s./sum(PyNquavers$Duration..s./100),
                   PyR36quavers[c(1:536,471:650,537:1146),]$Duration..s./sum(PyR36quavers[c(1:536,471:650,537:1146),]$Duration..s./100),
                   PyR66quavers$Duration..s./sum(PyR66quavers$Duration..s./100),
                   PySquavers$Duration..s./sum(PySquavers$Duration..s/100),
                   PyVquavers$Duration..s./sum(PyVquavers$Duration..s./100),
                   RySquavers$Duration..s./sum(RySquavers$Duration..s./100),
                   SyBquavers$Duration..s./sum(SyBquavers$Duration..s./100),
                   SySquavers$Duration..s./sum(SySquavers$Duration..s./100))

beatper38ii<- as.data.frame(beatper38ii)
beatper38ii$V1326<-NULL

rownames(beatper38ii) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                           "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
beatper38ii$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(beatper38ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38ii.csv", row.names=FALSE)

## Intensity

intper38ii<-rbind((CyH58quavers$Intensity..dB. - min(CyH58quavers$Intensity..dB. ))/(max(CyH58quavers$Intensity..dB. )- min(CyH58quavers$Intensity..dB. )), 
                  (DPyBquavers$Intensity..dB.- min(DPyBquavers$Intensity..dB. ))/(max(DPyBquavers$Intensity..dB. )- min(DPyBquavers$Intensity..dB. )),
                  (FyBquavers$Intensity..dB.- min(FyBquavers$Intensity..dB. ))/(max(FyBquavers$Intensity..dB. )- min(FyBquavers$Intensity..dB. )),
                  (FyvdPquavers[c(1:536,471:650,537:1146),]$Intensity..dB.- min(FyvdPquavers[c(1:536,471:650,537:1146),]$Intensity..dB. ))/(max(FyvdPquavers[c(1:536,471:650,537:1146),]$Intensity..dB. )- min(FyvdPquavers[c(1:536,471:650,537:1146),]$Intensity..dB. )),
                  (GyGquavers$Intensity..dB. - min(GyGquavers$Intensity..dB. ))/(max(GyGquavers$Intensity..dB. )- min(GyGquavers$Intensity..dB. )),
                  
                  (GyGrquavers$Intensity..dB. - min(GyGrquavers$Intensity..dB. ))/(max(GyGrquavers$Intensity..dB. )- min(GyGrquavers$Intensity..dB. )),
                  (GyGuquavers$Intensity..dB. - min(GyGuquavers$Intensity..dB. ))/(max(GyGuquavers$Intensity..dB. )- min(GyGuquavers$Intensity..dB. )),
                  (IyHquavers$Intensity..dB. - min(IyHquavers$Intensity..dB. ))/(max(IyHquavers$Intensity..dB. )- min(IyHquavers$Intensity..dB. )),
                  (MyAquavers$Intensity..dB.- min(MyAquavers$Intensity..dB. ))/(max(MyAquavers$Intensity..dB. )- min(MyAquavers$Intensity..dB. )),
                  (MyGquavers$Intensity..dB. - min(MyGquavers$Intensity..dB. ))/(max(MyGquavers$Intensity..dB. )- min(MyGquavers$Intensity..dB. )),
                  
                  (MyLquavers$Intensity..dB. - min(MyLquavers$Intensity..dB. ))/(max(MyLquavers$Intensity..dB. )- min(MyLquavers$Intensity..dB. )),
                  (MyPquavers$Intensity..dB. - min(MyPquavers$Intensity..dB. ))/(max(MyPquavers$Intensity..dB. )- min(MyPquavers$Intensity..dB. )),
                  (PyNquavers$Intensity..dB. - min(PyNquavers$Intensity..dB. ))/(max(PyNquavers$Intensity..dB. )- min(PyNquavers$Intensity..dB. )),
                  (PyR36quavers[c(1:536,471:650,537:1146),]$Intensity..dB.- min(PyR36quavers[c(1:536,471:650,537:1146),]$Intensity..dB. ))/(max(PyR36quavers[c(1:536,471:650,537:1146),]$Intensity..dB. )- min(PyR36quavers[c(1:536,471:650,537:1146),]$Intensity..dB. )),
                  (PyR66quavers$Intensity..dB.- min(PyR66quavers$Intensity..dB. ))/(max(PyR66quavers$Intensity..dB. )- min(PyR66quavers$Intensity..dB. )),
                  
                  (PySquavers$Intensity..dB.- min(PySquavers$Intensity..dB. ))/(max(PySquavers$Intensity..dB. )- min(PySquavers$Intensity..dB. )),
                  (PyVquavers$Intensity..dB. - min(PyVquavers$Intensity..dB. ))/(max(PyVquavers$Intensity..dB. )- min(PyVquavers$Intensity..dB. )),
                  (RySquavers$Intensity..dB. - min(RySquavers$Intensity..dB. ))/(max(RySquavers$Intensity..dB. )- min(RySquavers$Intensity..dB. )),
                  (SyBquavers$Intensity..dB.- min(SyBquavers$Intensity..dB. ))/(max(SyBquavers$Intensity..dB. )- min(SyBquavers$Intensity..dB. )),
                  (SySquavers$Intensity..dB. - min(SySquavers$Intensity..dB. ))/(max(SySquavers$Intensity..dB. )- min(SySquavers$Intensity..dB. )))


intper38ii<- as.data.frame(intper38ii)
intper38ii$V1326<-NULL

rownames(intper38ii) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                          "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
intper38ii$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                           "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(intper38ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38ii.csv", row.names=FALSE)


###
# Op.38iii
###

vc38iii <- matrix(,nrow=197*8,ncol=20)
var <-0
for (k in c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")){
  var=var+1
  dataframe <- crotchets38iii[crotchets38iii$performer == k,]
  
  for(j in 1:197){
    for (i in 1:8){
      vc38iii[i+8*(j-1),var]<-dataframe[j,i+1]
      
    }
  }
}


vc38iii <- as.data.frame(vc38iii)
colnames(vc38iii)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(vc38iii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/vc38iii.csv", row.names=FALSE)

### rows of performers

tc38iii <- t(vc38iii)
tc38iii <- as.data.frame(tc38iii)
tc38iii$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                        "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(tc38iii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tc38iii.csv", row.names=FALSE)


v <- numeric(788)
for (i in seq(1,394,by=2)){
  v[1+2*(i-1)] <- 1+4*(i-1)
  v[2+2*(i-1)] <- 2+4*(i-1)
  v[3+2*(i-1)] <- 3+4*(i-1)
  v[4+2*(i-1)] <- 4+4*(i-1)
  
}

tbeats38iii <- tc38iii[,v]
tintensity38iii <- tc38iii[,-v]
tbeats38iii$performers <- tintensity38iii$performers

write.csv(tbeats38iii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats38iii.csv", row.names=FALSE)
write.csv(tintensity38iii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity38iii.csv", row.names=FALSE)



## Beats

beatper38iii<-rbind(CyH58crotchets$Duration..s./sum(CyH58crotchets$Duration..s./100), 
                    DPyBcrotchets$Duration..s./sum(DPyBcrotchets$Duration..s./100),
                    FyBcrotchets$Duration..s./sum(FyBcrotchets$Duration..s./100),
                    FyvdPcrotchets$Duration..s./sum(FyvdPcrotchets$Duration..s./100),
                    GyGcrotchets$Duration..s./sum(GyGcrotchets$Duration..s./100),
                    GyGrcrotchets$Duration..s./sum(GyGrcrotchets$Duration..s./100),
                    GyGucrotchets$Duration..s./sum(GyGucrotchets$Duration..s./100),
                    IyHcrotchets$Duration..s./sum(IyHcrotchets$Duration..s./100),
                    MyAcrotchets$Duration..s./sum(MyAcrotchets$Duration..s./100),
                    MyGcrotchets$Duration..s./sum(MyGcrotchets$Duration..s./100),
                    MyLcrotchets$Duration..s./sum(MyLcrotchets$Duration..s./100),
                    MyPcrotchets$Duration..s./sum(MyPcrotchets$Duration..s./100),
                    PyNcrotchets$Duration..s./sum(PyNcrotchets[1:788,]$Duration..s./100),
                    PyR36crotchets$Duration..s./sum(PyR36crotchets$Duration..s./100),
                    PyR66crotchets$Duration..s./sum(PyR66crotchets$Duration..s./100),
                    PyScrotchets$Duration..s./sum(PyScrotchets$Duration..s/100),
                    PyVcrotchets$Duration..s./sum(PyVcrotchets$Duration..s./100),
                    RyScrotchets$Duration..s./sum(RyScrotchets$Duration..s./100),
                    SyBcrotchets$Duration..s./sum(SyBcrotchets$Duration..s./100),
                    SyScrotchets$Duration..s./sum(SyScrotchets$Duration..s./100))

beatper38iii<- as.data.frame(beatper38iii)
beatper38iii$V789<-NULL

rownames(beatper38iii) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
beatper38iii$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                             "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(beatper38iii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper38iii.csv", row.names=FALSE)

## Intensity

intper38iii<-rbind((CyH58crotchets$Intensity..dB. - min(CyH58crotchets$Intensity..dB. ))/(max(CyH58crotchets$Intensity..dB. )- min(CyH58crotchets$Intensity..dB. )), 
                   (DPyBcrotchets$Intensity..dB.- min(DPyBcrotchets$Intensity..dB. ))/(max(DPyBcrotchets$Intensity..dB. )- min(DPyBcrotchets$Intensity..dB. )),
                   (FyBcrotchets$Intensity..dB.- min(FyBcrotchets$Intensity..dB. ))/(max(FyBcrotchets$Intensity..dB. )- min(FyBcrotchets$Intensity..dB. )),
                   (FyvdPcrotchets$Intensity..dB.- min(FyvdPcrotchets$Intensity..dB. ))/(max(FyvdPcrotchets$Intensity..dB. )- min(FyvdPcrotchets$Intensity..dB. )),
                   (GyGcrotchets$Intensity..dB. - min(GyGcrotchets$Intensity..dB. ))/(max(GyGcrotchets$Intensity..dB. )- min(GyGcrotchets$Intensity..dB. )),
                   
                   (GyGrcrotchets$Intensity..dB. - min(GyGrcrotchets$Intensity..dB. ))/(max(GyGrcrotchets$Intensity..dB. )- min(GyGrcrotchets$Intensity..dB. )),
                   (GyGucrotchets$Intensity..dB. - min(GyGucrotchets$Intensity..dB. ))/(max(GyGucrotchets$Intensity..dB. )- min(GyGucrotchets$Intensity..dB. )),
                   (IyHcrotchets$Intensity..dB. - min(IyHcrotchets$Intensity..dB. ))/(max(IyHcrotchets$Intensity..dB. )- min(IyHcrotchets$Intensity..dB. )),
                   (MyAcrotchets$Intensity..dB.- min(MyAcrotchets$Intensity..dB. ))/(max(MyAcrotchets$Intensity..dB. )- min(MyAcrotchets$Intensity..dB. )),
                   (MyGcrotchets$Intensity..dB. - min(MyGcrotchets$Intensity..dB. ))/(max(MyGcrotchets$Intensity..dB. )- min(MyGcrotchets$Intensity..dB. )),
                   
                   (MyLcrotchets$Intensity..dB. - min(MyLcrotchets$Intensity..dB. ))/(max(MyLcrotchets$Intensity..dB. )- min(MyLcrotchets$Intensity..dB. )),
                   (MyPcrotchets$Intensity..dB. - min(MyPcrotchets$Intensity..dB. ))/(max(MyPcrotchets$Intensity..dB. )- min(MyPcrotchets$Intensity..dB. )),
                   (PyNcrotchets$Intensity..dB. - min(PyNcrotchets$Intensity..dB. ))/(max(PyNcrotchets$Intensity..dB. )- min(PyNcrotchets$Intensity..dB. )),
                   (PyR36crotchets$Intensity..dB.- min(PyR36crotchets$Intensity..dB. ))/(max(PyR36crotchets$Intensity..dB. )- min(PyR36crotchets$Intensity..dB. )),
                   (PyR66crotchets$Intensity..dB.- min(PyR66crotchets$Intensity..dB. ))/(max(PyR66crotchets$Intensity..dB. )- min(PyR66crotchets$Intensity..dB. )),
                   
                   (PyScrotchets$Intensity..dB.- min(PyScrotchets$Intensity..dB. ))/(max(PyScrotchets$Intensity..dB. )- min(PyScrotchets$Intensity..dB. )),
                   (PyVcrotchets$Intensity..dB. - min(PyVcrotchets$Intensity..dB. ))/(max(PyVcrotchets$Intensity..dB. )- min(PyVcrotchets$Intensity..dB. )),
                   (RyScrotchets$Intensity..dB. - min(RyScrotchets$Intensity..dB. ))/(max(RyScrotchets$Intensity..dB. )- min(RyScrotchets$Intensity..dB. )),
                   (SyBcrotchets$Intensity..dB.- min(SyBcrotchets$Intensity..dB. ))/(max(SyBcrotchets$Intensity..dB. )- min(SyBcrotchets$Intensity..dB. )),
                   (SyScrotchets$Intensity..dB. - min(SyScrotchets$Intensity..dB. ))/(max(SyScrotchets$Intensity..dB. )- min(SyScrotchets$Intensity..dB. )))


intper38iii<- as.data.frame(intper38iii)
intper38iii$V789<-NULL

rownames(intper38iii) <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                           "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
intper38iii$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(intper38iii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper38iii.csv", row.names=FALSE)


###
# Op.99i
###

vc99i <- matrix(,nrow=275*6,ncol=15)
var <-0
for (k in c("CH36","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"PN" ,"PR66"  ,"PV" ,"RS" ,"SB" ,"SS")){
  var=var+1
  dataframe <- crotchets99i[crotchets99i$performer == k,]
  
  for(j in 1:275){
    for (i in 1:6){
      vc99i[i+6*(j-1),var]<-dataframe[j,i+1]
      
    }
  }
}


vc99i <- as.data.frame(vc99i)
colnames(vc99i)<-c("CH36","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                   "ML" ,"PN"  ,"PR66"  ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(vc99i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/vc99i.csv", row.names=FALSE)

### rows of performers

tc99i <- t(vc99i)
tc99i <- as.data.frame(tc99i)
tc99i$performers <- c("CH36","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA", "MG",
                      "ML" ,"PN"  ,"PR66"  ,"PV" ,"RS" ,"SB" ,"SS")


write.csv(tc99i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tc99i.csv", row.names=FALSE)

v <- numeric(825)
for (i in seq(1,275,by=1)){
  v[1+3*(i-1)] <- 1+6*(i-1)
  v[2+3*(i-1)] <- 2+6*(i-1)
  v[3+3*(i-1)] <- 3+6*(i-1)
  
  
}

tbeats99i <- tc99i[,v]
tintensity99i <- tc99i[,-v]
tbeats99i$performers <- tintensity99i$performers

write.csv(tbeats99i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats99i.csv", row.names=FALSE)
write.csv(tintensity99i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity99i.csv", row.names=FALSE)


## Beats

beatper99i<-rbind(CyH36crotchets$Duration..s./sum(CyH36crotchets$Duration..s./100), 
                  DPyBcrotchets$Duration..s./sum(DPyBcrotchets$Duration..s./100),
                  FyBcrotchets[c(1:195,1:631),]$Duration..s./sum(FyBcrotchets$Duration..s./100),
                  
                  GyGcrotchets$Duration..s./sum(GyGcrotchets$Duration..s./100),
                  
                  GyGucrotchets$Duration..s./sum(GyGucrotchets$Duration..s./100),
                  IyHcrotchets$Duration..s./sum(IyHcrotchets$Duration..s./100),
                  MyAcrotchets$Duration..s./sum(MyAcrotchets$Duration..s./100),
                  MyGcrotchets$Duration..s./sum(MyGcrotchets$Duration..s./100),
                  MyLcrotchets$Duration..s./sum(MyLcrotchets$Duration..s./100),
                  
                  PyNcrotchets$Duration..s./sum(PyNcrotchets$Duration..s./100),
                  
                  PyR66crotchets$Duration..s./sum(PyR66crotchets$Duration..s./100),
                  
                  PyVcrotchets$Duration..s./sum(PyVcrotchets$Duration..s./100),
                  RyScrotchets$Duration..s./sum(RyScrotchets$Duration..s./100),
                  SyBcrotchets$Duration..s./sum(SyBcrotchets$Duration..s./100),
                  SyScrotchets$Duration..s./sum(SyScrotchets$Duration..s./100))

beatper99i<- as.data.frame(beatper99i)
beatper99i$V826<-NULL


beatper99i$performers <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                           "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(beatper99i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper99i.csv", row.names=FALSE)

## Intensity

intper99i<-rbind((CyH36crotchets$Intensity..dB. - min(CyH36crotchets$Intensity..dB. ))/(max(CyH36crotchets$Intensity..dB. )- min(CyH36crotchets$Intensity..dB. )), 
                 (DPyBcrotchets$Intensity..dB.- min(DPyBcrotchets$Intensity..dB. ))/(max(DPyBcrotchets$Intensity..dB. )- min(DPyBcrotchets$Intensity..dB. )),
                 (FyBcrotchets[c(1:195,1:631),]$Intensity..dB.- min(FyBcrotchets[c(1:195,1:631),]$Intensity..dB. ))/(max(FyBcrotchets[c(1:195,1:631),]$Intensity..dB. )- min(FyBcrotchets[c(1:195,1:631),]$Intensity..dB. )),
                 
                 (GyGcrotchets$Intensity..dB. - min(GyGcrotchets$Intensity..dB. ))/(max(GyGcrotchets$Intensity..dB. )- min(GyGcrotchets$Intensity..dB. )),
                 
                 
                 (GyGucrotchets$Intensity..dB. - min(GyGucrotchets$Intensity..dB. ))/(max(GyGucrotchets$Intensity..dB. )- min(GyGucrotchets$Intensity..dB. )),
                 (IyHcrotchets$Intensity..dB. - min(IyHcrotchets$Intensity..dB. ))/(max(IyHcrotchets$Intensity..dB. )- min(IyHcrotchets$Intensity..dB. )),
                 (MyAcrotchets$Intensity..dB.- min(MyAcrotchets$Intensity..dB. ))/(max(MyAcrotchets$Intensity..dB. )- min(MyAcrotchets$Intensity..dB. )),
                 (MyGcrotchets$Intensity..dB. - min(MyGcrotchets$Intensity..dB. ))/(max(MyGcrotchets$Intensity..dB. )- min(MyGcrotchets$Intensity..dB. )),
                 
                 (MyLcrotchets$Intensity..dB. - min(MyLcrotchets$Intensity..dB. ))/(max(MyLcrotchets$Intensity..dB. )- min(MyLcrotchets$Intensity..dB. )),
                 
                 (PyNcrotchets$Intensity..dB. - min(PyNcrotchets$Intensity..dB. ))/(max(PyNcrotchets$Intensity..dB. )- min(PyNcrotchets$Intensity..dB. )),
                 
                 (PyR66crotchets$Intensity..dB.- min(PyR66crotchets$Intensity..dB. ))/(max(PyR66crotchets$Intensity..dB. )- min(PyR66crotchets$Intensity..dB. )),
                 
                 
                 (PyVcrotchets$Intensity..dB. - min(PyVcrotchets$Intensity..dB. ))/(max(PyVcrotchets$Intensity..dB. )- min(PyVcrotchets$Intensity..dB. )),
                 (RyScrotchets$Intensity..dB. - min(RyScrotchets$Intensity..dB. ))/(max(RyScrotchets$Intensity..dB. )- min(RyScrotchets$Intensity..dB. )),
                 (SyBcrotchets$Intensity..dB.- min(SyBcrotchets$Intensity..dB. ))/(max(SyBcrotchets$Intensity..dB. )- min(SyBcrotchets$Intensity..dB. )),
                 (SyScrotchets$Intensity..dB. - min(SyScrotchets$Intensity..dB. ))/(max(SyScrotchets$Intensity..dB. )- min(SyScrotchets$Intensity..dB. )))


intper99i<- as.data.frame(intper99i)
intper99i$V826<-NULL


intper99i$performers <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                          "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(intper99i, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper99i.csv", row.names=FALSE)


###
# Op. 99ii
###

vc99ii <- matrix(,nrow=70*16,ncol=15)
var <-0
for (k in c("CH36","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"PN" ,"PR66"  ,"PV" ,"RS" ,"SB" ,"SS")){
  var=var+1
  dataframe <- semiquavers99ii[semiquavers99ii$performer == k,]
  
  for(j in 1:70){
    for (i in 1:16){
      vc99ii[i+16*(j-1),var]<-dataframe[j,i+1]
      
    }
  }
}


vc99ii <- as.data.frame(vc99ii)
colnames(vc99ii)<-c("CH36","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                   "ML" ,"PN"  ,"PR66"  ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(vc99ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/vc99ii.csv", row.names=FALSE)

### rows of performers

tc99ii <- t(vc99ii)
tc99ii <- as.data.frame(tc99ii)
tc99ii$performers <- c("CH36","DPB" ,"FB"  ,"GG" ,"GGu" ,"IH" ,"MA", "MG",
                      "ML" ,"PN"  ,"PR66"  ,"PV" ,"RS" ,"SB" ,"SS")


write.csv(tc99ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tc99ii.csv", row.names=FALSE)

v <- numeric(560)
for (i in seq(1,560,by=2)){
  v[1+4*(i-1)] <- 1+8*(i-1)
  v[2+4*(i-1)] <- 2+8*(i-1)
  v[3+4*(i-1)] <- 3+8*(i-1)
  v[4+4*(i-1)] <- 4+8*(i-1)
  v[5+4*(i-1)] <- 5+8*(i-1)
  v[6+4*(i-1)] <- 6+8*(i-1)
  v[7+4*(i-1)] <- 7+8*(i-1)
  v[8+4*(i-1)] <- 8+8*(i-1)
  
}

tbeats99ii <- tc99ii[,v[1:560]]
tintensity99ii <- tc99ii[,-v[1:560]]
tbeats99ii$performers <- tintensity99ii$performers

write.csv(tbeats99ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats99ii.csv", row.names=FALSE)
write.csv(tintensity99ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity99ii.csv", row.names=FALSE)

## Beat

beatper99ii<-rbind(CyH36semiquavers$Duration..s./sum(CyH36semiquavers$Duration..s./100), 
                   DPyBsemiquavers$Duration..s./sum(DPyBsemiquavers$Duration..s./100),
                   FyBsemiquavers$Duration..s./sum(FyBsemiquavers$Duration..s./100),
                   
                   GyGsemiquavers$Duration..s./sum(GyGsemiquavers$Duration..s./100),
                   
                   GyGusemiquavers$Duration..s./sum(GyGusemiquavers$Duration..s./100),
                   IyHsemiquavers$Duration..s./sum(IyHsemiquavers$Duration..s./100),
                   MyAsemiquavers$Duration..s./sum(MyAsemiquavers$Duration..s./100),
                   MyGsemiquavers$Duration..s./sum(MyGsemiquavers$Duration..s./100),
                   MyLsemiquavers$Duration..s./sum(MyLsemiquavers$Duration..s./100),
                   
                   PyNsemiquavers$Duration..s./sum(PyNsemiquavers$Duration..s./100),
                   
                   PyR66semiquavers$Duration..s./sum(PyR66semiquavers$Duration..s./100),
                   
                   PyVsemiquavers$Duration..s./sum(PyVsemiquavers$Duration..s./100),
                   RySsemiquavers$Duration..s./sum(RySsemiquavers$Duration..s./100),
                   SyBsemiquavers$Duration..s./sum(SyBsemiquavers$Duration..s./100),
                   SySsemiquavers$Duration..s./sum(SySsemiquavers$Duration..s./100))

beatper99ii<- as.data.frame(beatper99ii)
beatper99ii$V561<-NULL


beatper99ii$performers <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                            "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(beatper99ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/beatper99ii.csv", row.names=FALSE)

## Intensity

intper99ii<-rbind((CyH36semiquavers$Intensity..dB. - min(CyH36semiquavers$Intensity..dB. ))/(max(CyH36semiquavers$Intensity..dB. )- min(CyH36semiquavers$Intensity..dB. )), 
                 (DPyBsemiquavers$Intensity..dB.- min(DPyBsemiquavers$Intensity..dB. ))/(max(DPyBsemiquavers$Intensity..dB. )- min(DPyBsemiquavers$Intensity..dB. )),
                 (FyBsemiquavers$Intensity..dB.- min(FyBsemiquavers$Intensity..dB. ))/(max(FyBsemiquavers$Intensity..dB. )- min(FyBsemiquavers$Intensity..dB. )),
                 
                 (GyGsemiquavers$Intensity..dB. - min(GyGsemiquavers$Intensity..dB. ))/(max(GyGsemiquavers$Intensity..dB. )- min(GyGsemiquavers$Intensity..dB. )),
                 
                 
                 (GyGusemiquavers$Intensity..dB. - min(GyGusemiquavers$Intensity..dB. ))/(max(GyGusemiquavers$Intensity..dB. )- min(GyGusemiquavers$Intensity..dB. )),
                 (IyHsemiquavers$Intensity..dB. - min(IyHsemiquavers$Intensity..dB. ))/(max(IyHsemiquavers$Intensity..dB. )- min(IyHsemiquavers$Intensity..dB. )),
                 (MyAsemiquavers$Intensity..dB.- min(MyAsemiquavers$Intensity..dB. ))/(max(MyAsemiquavers$Intensity..dB. )- min(MyAsemiquavers$Intensity..dB. )),
                 (MyGsemiquavers$Intensity..dB. - min(MyGsemiquavers$Intensity..dB. ))/(max(MyGsemiquavers$Intensity..dB. )- min(MyGsemiquavers$Intensity..dB. )),
                 
                 (MyLsemiquavers$Intensity..dB. - min(MyLsemiquavers$Intensity..dB. ))/(max(MyLsemiquavers$Intensity..dB. )- min(MyLsemiquavers$Intensity..dB. )),
                 
                 (PyNsemiquavers$Intensity..dB. - min(PyNsemiquavers$Intensity..dB. ))/(max(PyNsemiquavers$Intensity..dB. )- min(PyNsemiquavers$Intensity..dB. )),
                 
                 (PyR66semiquavers$Intensity..dB.- min(PyR66semiquavers$Intensity..dB. ))/(max(PyR66semiquavers$Intensity..dB. )- min(PyR66semiquavers$Intensity..dB. )),
                 (PyVsemiquavers$Intensity..dB. - min(PyVsemiquavers$Intensity..dB. ))/(max(PyVsemiquavers$Intensity..dB. )- min(PyVsemiquavers$Intensity..dB. )),
                 (RySsemiquavers$Intensity..dB. - min(RySsemiquavers$Intensity..dB. ))/(max(RySsemiquavers$Intensity..dB. )- min(RySsemiquavers$Intensity..dB. )),
                 (SyBsemiquavers$Intensity..dB.- min(SyBsemiquavers$Intensity..dB. ))/(max(SyBsemiquavers$Intensity..dB. )- min(SyBsemiquavers$Intensity..dB. )),
                 (SySsemiquavers$Intensity..dB. - min(SySsemiquavers$Intensity..dB. ))/(max(SySsemiquavers$Intensity..dB. )- min(SySsemiquavers$Intensity..dB. )))


intper99ii<- as.data.frame(intper99ii)
intper99ii$V561<-NULL


intper99ii$performers <- c("CH","DPB" ,"FB" ,"GG" ,"GGu" ,"IH" ,"MA","MG",
                          "ML" ,"PN" ,"PR66" ,"PV" ,"RS" ,"SB" ,"SS")
write.csv(intper99ii, "C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/intper99ii.csv", row.names=FALSE)


