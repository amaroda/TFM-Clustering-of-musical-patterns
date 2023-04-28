# Clustering of musical patterns

## This script contains the code that created the datasets:
## crotchets38i, quavers38ii, crotchets38iii, crotchets99i, semiquavers99ii

### Libraries

library(plyr)
library(readr)
library(caret)
library(ggfortify)


### Loading the raw datasets

# Op. 38
# 20 recordings

setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 38i")
#setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 38ii")
#setwd("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Data/Op. 38iii")

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
  
  for (j in c(4,5)){
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
  
  for (j in c(6)){
    type <- mytype[j]
    assign(paste0(miname,type), read.csv(myfiles[j],sep=";")) 
    
  }
  
}

### Tidying functions

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
  newdf2[1,2] <- (dataframe[2,]$Intensity..dB. - dataframe[1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,3] <- (dataframe[3,]$Intensity..dB. - dataframe[2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,4] <- (dataframe[4,]$Intensity..dB. - dataframe[3,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,5] <- floor(dataframe[2,]$Bar.Crotchet)
  
  for (i in seq(5,dim(dataframe)[1] - 1, by=4)){
    
    
    newdf2[i-3*cont,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-3*cont,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-3*cont,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-3*cont,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
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
    
    
    newdf2[i-5*cont-2,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,5] <- (dataframe[i+4,]$Intensity..dB. - dataframe[i+3,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-5*cont-2,6] <- (dataframe[i+5,]$Intensity..dB. - dataframe[i+4,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
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
  newdf2[1,2] <- (dataframe[2,]$Intensity..dB. - dataframe[1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,3] <- (dataframe[3,]$Intensity..dB. - dataframe[2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,4] <- floor(dataframe[2,]$Bar.Crotchet)
  
  for (i in seq(4,dim(dataframe)[1] - 1, by=3)){
    
    
    newdf2[i-2*cont,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-2*cont,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-2*cont,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
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
  newdf2[1,2] <- (dataframe[2,]$Intensity..dB. - dataframe[1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,3] <- (dataframe[3,]$Intensity..dB. - dataframe[2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,4] <- (dataframe[4,]$Intensity..dB. - dataframe[3,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,5] <- (dataframe[5,]$Intensity..dB. - dataframe[4,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,6] <- (dataframe[6,]$Intensity..dB. - dataframe[5,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,7] <- (dataframe[7,]$Intensity..dB. - dataframe[6,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,8] <- (dataframe[8,]$Intensity..dB. - dataframe[7,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
  newdf2[1,9] <- floor(dataframe[2,]$Bar.Semiquaver)
  
  for (i in seq(9,dim(dataframe)[1] - 1, by=8)){
    
    
    newdf2[i-7*cont,1] <- (dataframe[i,]$Intensity..dB. - dataframe[i-1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-7*cont,2] <- (dataframe[i+1,]$Intensity..dB. - dataframe[i,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-7*cont,3] <- (dataframe[i+2,]$Intensity..dB. - dataframe[i+1,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-7*cont,4] <- (dataframe[i+3,]$Intensity..dB. - dataframe[i+2,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-7*cont,5] <- (dataframe[i+4,]$Intensity..dB. - dataframe[i+3,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-7*cont,6] <- (dataframe[i+5,]$Intensity..dB. - dataframe[i+4,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-7*cont,7] <- (dataframe[i+6,]$Intensity..dB. - dataframe[i+5,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
    newdf2[i-7*cont,8] <- (dataframe[i+7,]$Intensity..dB. - dataframe[i+6,]$Intensity..dB.)/max(dataframe$Intensity..dB.)
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

write.csv(crotchets38i, "C:/Users/samte/OneDrive/Desktop/crotchets38i.csv", row.names=FALSE)

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

write.csv(quavers38ii, "C:/Users/samte/OneDrive/Desktop/quavers38ii.csv", row.names=FALSE)


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

write.csv(crotchets38iii, "C:/Users/samte/OneDrive/Desktop/crotchets38iii.csv", row.names=FALSE)


### Op. 99i crotchets

CH <- crotchet_fun99i(CyH36crotchets,CH)
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

crotchets99i <- rbind(CH,DPB,FB,GG,GGu,  IH,MA,MG,ML,PR66,  PN,PV,RS,SB,SS)

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

write.csv(crotchets99i, "C:/Users/samte/OneDrive/Desktop/crotchets99i.csv", row.names=FALSE)

### Op. 99ii semiquavers

CH <- semiquaver_fun99ii(CyH36semiquavers,CH)
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

semiquavers99ii <- rbind(CH,DPB,FB,GG,GGu,  IH,MA,MG,ML,PR66,  PN,PV,RS,SB,SS)


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

write.csv(crotchets99ii, "C:/Users/samte/OneDrive/Desktop/crotchets99ii.csv", row.names=FALSE)

### columns of performers

vc38i <- matrix(,nrow=2160,ncol=20)
var <-0
for (k in c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
            "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")){
  var=var+1
  dataframe <- crotchets38i[crotchets38i$performer == k,]
  if (dim(dataframe)[1]>300){
    dataframe <- dataframe[91:370,]
  }
  
  for(j in 1:270){
    for (i in 1:8){
      vc38i[i+8*(j-1),var]<-dataframe[j,i+1]
      
    }
  }
}


vc38i <- as.data.frame(vc38i)

vc38i$CH <- vc38i$V1
vc38i$DPB <- vc38i$V2
vc38i$FB <- vc38i$V3
vc38i$FvdP <- vc38i$V4
vc38i$GG <- vc38i$V5

vc38i$GGr <- vc38i$V6
vc38i$GGu <- vc38i$V7
vc38i$IH <- vc38i$V8
vc38i$MA <- vc38i$V9
vc38i$MG <- vc38i$V10

vc38i$ML <- vc38i$V11
vc38i$MP <- vc38i$V12
vc38i$PN <- vc38i$V13
vc38i$PR36 <- vc38i$V14
vc38i$PR66 <- vc38i$V15

vc38i$PS <- vc38i$V16
vc38i$PV <- vc38i$V17
vc38i$RS <- vc38i$V18
vc38i$SB <- vc38i$V19
vc38i$SS <- vc38i$V20

vc38i$V1 <- NULL
vc38i$V2 <- NULL
vc38i$V3 <- NULL
vc38i$V4 <- NULL
vc38i$V5 <- NULL

vc38i$V6 <- NULL
vc38i$V7 <- NULL
vc38i$V8 <- NULL
vc38i$V9 <- NULL
vc38i$V10 <- NULL

vc38i$V11 <- NULL
vc38i$V12 <- NULL
vc38i$V13 <- NULL
vc38i$V14 <- NULL
vc38i$V15 <- NULL

vc38i$V16 <- NULL
vc38i$V17 <- NULL
vc38i$V18 <- NULL
vc38i$V19 <- NULL
vc38i$V20 <- NULL

write.csv(vc38i, "C:/Users/samte/OneDrive/Desktop/vc38i.csv", row.names=FALSE)

### rows of performers

tc38i <- t(vc38i)
tc38i <- as.data.frame(tc38i)
tc38i$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                      "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(tc38i, "C:/Users/samte/OneDrive/Desktop/tc38i.csv", row.names=FALSE)

tscaled <- scale(tc38i[,1:2160])
tscaled <- as.data.frame(tscaled)
tscaled$performers <- tc38i$performers

write.csv(tscaled, "C:/Users/samte/OneDrive/Desktop/tscaled.csv", row.names=FALSE)

### unused for now

t<-t(tscaled)
t<- as.data.frame(t)
t<-t[1:2160,]

t$CH <- as.numeric(t$CH)
t$DPB <- as.numeric(t$DPB)
t$FB <- as.numeric(t$FB)
t$FvdP <- as.numeric(t$FvdP)
t$GG <- as.numeric(t$GG)

t$GGr <- as.numeric(t$GGr)
t$GGu <- as.numeric(t$GGu)
t$IH <- as.numeric(t$IH)
t$MA <- as.numeric(t$MA)
t$MG <- as.numeric(t$MG)

t$ML <- as.numeric(t$ML)
t$MP <- as.numeric(t$MP)
t$PN <- as.numeric(t$PN)
t$PR36 <- as.numeric(t$PR36)
t$PR66 <- as.numeric(t$PR66)

t$PS <- as.numeric(t$PS)
t$PV <- as.numeric(t$PV)
t$RS <- as.numeric(t$RS)
t$SB <- as.numeric(t$SB)
t$SS <- as.numeric(t$SS)

cor(t)

### beats and intensity

v <- numeric(1080)
for (i in seq(1,540,by=2)){
  v[1+2*(i-1)] <- 1+4*(i-1)
  v[2+2*(i-1)] <- 2+4*(i-1)
  v[3+2*(i-1)] <- 3+4*(i-1)
  v[4+2*(i-1)] <- 4+4*(i-1)
  
}

tbeats <- tc38i[,v]
tintensity <- tc38i[,-v]
tbeats$performers <- tintensity$performers

write.csv(tbeats, "C:/Users/samte/OneDrive/Desktop/tbeats.csv", row.names=FALSE)
write.csv(tintensity, "C:/Users/samte/OneDrive/Desktop/tintensity.csv", row.names=FALSE)

### notes cello

notes.cello38i<-rbind(CyH58notes.cello[310:1248,]$Duration..s.,DPyBnotes.cello$Duration..s.,FyBnotes.cello$Duration..s.,
                      FyvdPnotes.cello$Duration..s.,GyGnotes.cello[310:1248,]$Duration..s.,GyGrnotes.cello[310:1248,]$Duration..s.,
                      GyGunotes.cello[310:1248,]$Duration..s.,IyHnotes.cello[310:1248,]$Duration..s.,MyAnotes.cello$Duration..s.,
                      MyGnotes.cello[310:1248,]$Duration..s.,MyLnotes.cello[310:1248,]$Duration..s.,MyPnotes.cello[310:1248,]$Duration..s.,
                      PyNnotes.cello[310:1248,]$Duration..s.,PyR36notes.cello$Duration..s.,PyR66notes.cello$Duration..s.,
                      PySnotes.cello$Duration..s.,PyVnotes.cello[310:1248,]$Duration..s.,RySnotes.cello[310:1248,]$Duration..s.,SyBnotes.cello$Duration..s.,SySnotes.cello[310:1248,]$Duration..s.)
# error en PN, en algun momento faltan dos notas (633,634)
# revisar IH, PR66
notes.cello38i <- as.data.frame(notes.cello38i)
notes.cello38i$V939 <- NULL
notes.cello38i$V938 <- NULL
notes.cello38i$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                               "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(notes.cello38i, "C:/Users/samte/OneDrive/Desktop/notes.cello38i.csv", row.names=FALSE)

### transposed notes

tcello<-t(notes.cello38i[,1:937])
tcello <- as.data.frame(tcello)

tcello$CH <- tcello$V1
tcello$DPB <- tcello$V2
tcello$FB <- tcello$V3
tcello$FvdP <- tcello$V4
tcello$GG <- tcello$V5

tcello$GGr <- tcello$V6
tcello$GGu <- tcello$V7
tcello$IH <- tcello$V8
tcello$MA <- tcello$V9
tcello$MG <- tcello$V10

tcello$ML <- tcello$V11
tcello$MP <- tcello$V12
tcello$PN <- tcello$V13
tcello$PR36 <- tcello$V14
tcello$PR66 <- tcello$V15

tcello$PS <- tcello$V16
tcello$PV <- tcello$V17
tcello$RS <- tcello$V18
tcello$SB <- tcello$V19
tcello$SS <- tcello$V20

tcello$V1 <- NULL
tcello$V2 <- NULL
tcello$V3 <- NULL
tcello$V4 <- NULL
tcello$V5 <- NULL

tcello$V6 <- NULL
tcello$V7 <- NULL
tcello$V8 <- NULL
tcello$V9 <- NULL
tcello$V10 <- NULL

tcello$V11 <- NULL
tcello$V12 <- NULL
tcello$V13 <- NULL
tcello$V14 <- NULL
tcello$V15 <- NULL

tcello$V16 <- NULL
tcello$V17 <- NULL
tcello$V18 <- NULL
tcello$V19 <- NULL
tcello$V20 <- NULL

cor(tcello)

write.csv(tcello, "C:/Users/samte/OneDrive/Desktop/tcello.csv", row.names=FALSE)

### notes piano

notes.piano38i<-rbind(CyH58notes.piano[526:2221,]$Duration..s.,DPyBnotes.piano$Duration..s.,FyBnotes.piano$Duration..s.,
                      FyvdPnotes.piano$Duration..s.,GyGnotes.piano[526:2221,]$Duration..s.,GyGrnotes.piano[526:2221,]$Duration..s.,
                      GyGunotes.piano[526:2221,]$Duration..s.,IyHnotes.piano[526:2221,]$Duration..s.,MyAnotes.piano$Duration..s.,
                      MyGnotes.piano[526:2221,]$Duration..s.,MyLnotes.piano[526:2221,]$Duration..s.,MyPnotes.piano[526:2221,]$Duration..s.,
                      PyNnotes.piano[526:2221,]$Duration..s.,PyR36notes.piano$Duration..s.,PyR66notes.piano$Duration..s.,
                      PySnotes.piano$Duration..s.,PyVnotes.piano[526:2221,]$Duration..s.,RySnotes.piano[526:2221,]$Duration..s.,SyBnotes.piano$Duration..s.,SySnotes.piano[526:2221,]$Duration..s.)
# revisar IH
notes.piano38i <- as.data.frame(notes.piano38i)
notes.piano38i$V1696 <- NULL
notes.piano38i$performers <- c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                               "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS")

write.csv(notes.piano38i, "C:/Users/samte/OneDrive/Desktop/notes.piano38i.csv", row.names=FALSE)

### transposed notes

tpiano<-t(notes.piano38i[,1:1695])
tpiano <- as.data.frame(tpiano)

tpiano$CH <- tpiano$V1
tpiano$DPB <- tpiano$V2
tpiano$FB <- tpiano$V3
tpiano$FvdP <- tpiano$V4
tpiano$GG <- tpiano$V5

tpiano$GGr <- tpiano$V6
tpiano$GGu <- tpiano$V7
tpiano$IH <- tpiano$V8
tpiano$MA <- tpiano$V9
tpiano$MG <- tpiano$V10

tpiano$ML <- tpiano$V11
tpiano$MP <- tpiano$V12
tpiano$PN <- tpiano$V13
tpiano$PR36 <- tpiano$V14
tpiano$PR66 <- tpiano$V15

tpiano$PS <- tpiano$V16
tpiano$PV <- tpiano$V17
tpiano$RS <- tpiano$V18
tpiano$SB <- tpiano$V19
tpiano$SS <- tpiano$V20

tpiano$V1 <- NULL
tpiano$V2 <- NULL
tpiano$V3 <- NULL
tpiano$V4 <- NULL
tpiano$V5 <- NULL

tpiano$V6 <- NULL
tpiano$V7 <- NULL
tpiano$V8 <- NULL
tpiano$V9 <- NULL
tpiano$V10 <- NULL

tpiano$V11 <- NULL
tpiano$V12 <- NULL
tpiano$V13 <- NULL
tpiano$V14 <- NULL
tpiano$V15 <- NULL

tpiano$V16 <- NULL
tpiano$V17 <- NULL
tpiano$V18 <- NULL
tpiano$V19 <- NULL
tpiano$V20 <- NULL

write.csv(tpiano, "C:/Users/samte/OneDrive/Desktop/tpiano.csv", row.names=FALSE)
