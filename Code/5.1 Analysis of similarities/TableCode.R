# Code to create the table in section 5.1

tbeats38i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats.csv")
tintensity38i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity.csv")

tbeats38ii <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats38ii.csv")
tintensity38ii <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity38ii.csv")

tbeats38iii <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats38iii.csv")
tintensity38iii <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity38iii.csv")

tbeats99i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats99i.csv")
tintensity99i <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity99i.csv")

tbeats99ii <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tbeats99ii.csv")
tintensity99ii <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/tintensity99ii.csv")


destats<-matrix(,nrow=21,ncol=10)
for (i in 1:20){
  destats[i,1]<-round(sd(as.numeric(tbeats38i[i,1:1120])),4)
  destats[i,2]<-round(sd(as.numeric(tbeats38ii[i,1:1320])),4)
  destats[i,3]<-round(sd(as.numeric(tbeats38iii[i,1:788])),4)
  destats[i,4]<-round(sd(as.numeric(tbeats99i[i,1:825])),4)
  destats[i,5]<-round(sd(as.numeric(tbeats99ii[i,1:560])),4)
  
  destats[i,6]<-round(sd(as.numeric(tintensity38i[i,1:1120])),4)
  destats[i,7]<-round(sd(as.numeric(tintensity38ii[i,1:1320])),4)
  destats[i,8]<-round(sd(as.numeric(tintensity38iii[i,1:788])),4)
  destats[i,9]<-round(sd(as.numeric(tintensity99i[i,1:825])),4)
  destats[i,10]<-round(sd(as.numeric(tintensity99ii[i,1:560])),4)
  
  
}


for (j in c(4,5,9,10)){
  u<-destats[,j]
  destats[,j]<-c(u[1:3],NA,u[4],NA,u[5:9],NA,u[10],NA,u[11],NA,u[12:15],NA)
  
}

destats[21,]<-round(colMeans(destats, na.rm = T),4)

colnames(destats)<-c("Dur.SD 38i", "Dur.SD 38ii", "Dur.SD 38iii", "Dur.SD 99i", "Dur.SD 99ii",
                     "Int.SD 38i", "Int.SD 38ii","Int.SD 38iii","Int.SD 99i","Int.SD 99ii")
rownames(destats)<-c("CH","DPB" ,"FB" ,"FvdP" ,"GG" ,"GGr" ,"GGu" ,"IH" ,"MA","MG",
                     "ML" ,"MP" ,"PN" ,"PR36" ,"PR66" ,"PS" ,"PV" ,"RS" ,"SB" ,"SS","Mean")


library("gridExtra")
pdf("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/plots/Table.pdf",
    width = 12, height = 8.1)       # Export PDF
grid.table(destats)
dev.off()

heatmap(
  as.matrix(scale(destats)),
  col = colorRampPalette(c("blue", "white", "red"))(256)[45:256],
  scale = "none"
)







