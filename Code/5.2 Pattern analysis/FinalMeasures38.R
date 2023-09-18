# End of phrase clustering:
library(magrittr)

crotchets38 <- read.csv("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/curatedData/crotchets38.csv")

# Measures that occur at the end of phrases
ending_measures_38i <- c(20, 33, 41, 53, 57, 65, 77, 90, 112, 125, 137, 144, 161, 181, 195, 202, 214, 218, 226, 238, 252, 280)
ending_measures_38iii <- c(15, 30, 37, 43, 52, 61, 69, 75, 90, 98, 114, 122, 131, 142, 157, 164, 174, 188, 198)

crotchets38i <- crotchets38[crotchets38$type == 1,]
crotchets38iii <- crotchets38[crotchets38$type == 2,]

# Filter the measures
filtered_df1 <- crotchets38i[crotchets38i$bar %in% ending_measures_38i, ]
filtered_df2 <- crotchets38iii[crotchets38iii$bar %in% ending_measures_38iii, ]

# New dataset
FinalMeasures <- rbind(filtered_df1, filtered_df2)

# Create the frequency pattern matrix
patternsb[,1]<-as.vector(table(pattern1b$performer)/as.vector(table(FinalMeasures$performer)))
patternsb[,2]<-as.vector(table(pattern2b$performer)/as.vector(table(FinalMeasures$performer)))
patternsb[,3]<-as.vector(table(pattern3b$performer)/as.vector(table(FinalMeasures$performer)))
patternsb[,4]<-as.vector(table(pattern4b$performer)/as.vector(table(FinalMeasures$performer)))
colnames(patternsb)<-c("L1","L2","L3","L4")
rownames(patternsb)<-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                       "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")


# Create the barplot
mx <- t(as.matrix(patternsb[c(1:20),]))
colnames(mx) <-c("CH","DPB" ,"FB", "FvdP" ,"GG",  "GGr","GGu" ,"IH" ,"MA","MG",
                 "ML" , "MP" ,"PN", "PR36" ,"PR66" , "PS", "PV" ,"RS" ,"SB" ,"SS")

colours = c("#4285F4","#02A62D","#FBBC05","red","#4285F4","#02A62D","#FBBC05","red")

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/FinalMeasures38.pdf",
    width = 16, height = 8)

# barplot
barplot(mx, xlab='Performance',beside = F, 
        col=colours,ylim=c(0,max(mx)*2), yaxt='n')
# to add a box around the plot
box()

# add a legend
legend('topleft',fill=colours,legend=c('L1: Long 1st beat','L2: Long 2nd beat','L3: Long 3rd beat','L4: Long 4th beat'))

FinalMeasures$cluster <- as.factor(n[which.max(u),])
