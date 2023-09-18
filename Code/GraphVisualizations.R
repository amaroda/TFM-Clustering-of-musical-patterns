library(grDevices)
library(igraph)
library(readxl)

### Recordings

Relationships <- read_excel("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/RelationshipsG.xlsx")

# Create an igraph graph object
g <- graph_from_data_frame(Relationships, directed = F)

# Define the colors for each type of link
edge_colors <- c(master = "blue",  same_country = "gold2", same_teacher = "red",
                 masterclass = "cyan", pupil_of_pupil = "darkgreen", teacher_was_the_teacher_of_Du_Prés_teacher = "black",
                 same_cellist = "darkred", same_pianist = "darkblue", same_country_pianist = "pink", same_generation = "gray", same_recording_era = "lightseagreen")

# Get the edge colors based on the relationship type
edge_colors_vector <- edge_colors[Relationships$relationship]
E(g)$weight <- Relationships$weight


## A priori graph cluster

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/recrela.pdf",
    width = 10, height = 8)

set.seed(94)
plot.igraph(g, vertex.color = c("#6495ED" ,"#867EC0", "#CB5167" , "#867EC0", "#6495ED" ,"#EE3B3B",
                                "#EE3B3B","#EE3B3B","#EE3B3B","#6495ED","#867EC0",
                                "#867EC0","#EE3B3B","#A96894","#A96894","#A96894", 
                                "#EE3B3B", "#EE3B3B", "#EE3B3B","#EE3B3B","#EE3B3B"),
            edge.color = edge_colors_vector, edge.width = E(g)$weight, vertex.size = 20, vertex.label.cex = 1.1, layout=layout_with_dh)

legend("topright", legend = c("Master/Teacher" , "Same country", "Same teacher", "Pupil of pupil","Shared past teacher", "Masterclass", "Same cellist","Same pianist","Same country pianist", "Same generation", "Same recording era"), col = c("blue","gold2", "red", "darkgreen", "black","cyan","darkred","darkblue","pink","gray","lightseagreen"), lty = 1, bty = "n")

dev.off()

### Cellists

Relationships <- read_excel("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/Relationships.xlsx")

# Create an igraph graph object
g <- graph_from_data_frame(Relationships, directed = F)

# Define the colors for each type of link
edge_colors <- c(master = "blue",  same_country = "gold2", same_teacher = "red",
                 masterclass = "cyan", pupil_of_pupil = "green", teacher_was_the_teacher_of_Du_Prés_teacher = "black"
)

# Get the edge colors based on the relationship type
edge_colors_vector <- edge_colors[Relationships$relationship]


## graph

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/cellorela.pdf",
    width = 8, height = 8)

set.seed(183)
plot.igraph(g, vertex.color = c("#867EC0", "#CB5167" , "#867EC0", "#6495ED" ,"#EE3B3B",
                                "#EE3B3B","#EE3B3B","#EE3B3B","#6495ED","#867EC0",
                                "#867EC0","#EE3B3B","#A96894","#A96894","#A96894", 
                                "#EE3B3B", "#EE3B3B", "#EE3B3B","#EE3B3B","#EE3B3B"),
            edge.color = edge_colors_vector, edge.width = 1.5, vertex.size = 1, vertex.label.cex = 1.1, layout=layout_with_dh)

legend("bottomleft", legend = c("Master/teacher" , "Same country", "Same teacher", "Pupil of pupil","Shared past teacher", "Masterclass"), col = c("blue","gold2", "red", "green", "black","cyan"), lty = 1, bty = "n")

dev.off()


### Pianists

Relationships <- read_excel("C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/RelationshipsP.xlsx")

# Create an igraph graph object
g <- graph_from_data_frame(Relationships, directed = F)

# Define the colors for each type of link
edge_colors <- c(lived_USA = "blue",  same_country = "black", born_pre_wwi = "red",
                 born_between_wars = "cyan", born_post_wwii = "green", young = "gold2"
)

# Get the edge colors based on the relationship type
edge_colors_vector <- edge_colors[Relationships$relationship]

pdf(file="C:/Users/samte/OneDrive/Desktop/TFM Clustering of musical patterns/pianorela.pdf",
    width = 8, height = 8)


## graph
set.seed(97)
plot.igraph(g, vertex.color = c("#867EC0", "#CB5167" , "#867EC0", "#6495ED" ,"#EE3B3B",
                                "#EE3B3B","#EE3B3B","#EE3B3B","#6495ED","#867EC0",
                                "#867EC0","#EE3B3B","#A96894","#A96894","#A96894", 
                                "#EE3B3B", "#EE3B3B", "#EE3B3B","#EE3B3B","#EE3B3B"),
            edge.color = edge_colors_vector, edge.width = 1, vertex.size = 1, vertex.label.cex = 1.1, layout=layout_with_dh)

legend("topright", legend = c("Lived in USA" , "Same country", "Born before WWI", "Born between WWI-WWII","Born post WWII", "Young"), col = c("blue","black", "red","cyan", "green", "gold2"), lty = 1, bty = "n")

dev.off()
