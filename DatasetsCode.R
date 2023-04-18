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

# Tidying the crotchet 38i dataset

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

# Tidying the crotchet 38i dataset

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

# Tidying the 38ii quaver dataset

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