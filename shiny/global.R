library(data.table)
library(FNN)
library(igraph)
library(rgl)
library(shinyBS)
library(shinyRGL)
library(scrapeR)
library(rARPACK)
library(irlba)
library(Matrix)
library(DT)
library(scales)
library(mclust)
library(Rtsne)
library(rvest)
#library(corrplot)
#library(dendextend)
#library(fpc)
#library(FlashGraphR)
for(file in list.files("R",full.names=TRUE)){
   source(file)
}

source('utils.R')

seed <- 100

colors.list <- unique(c("gray","red","green","cyan","magenta","yellow",
                "orange","gray30","salmon","gray60","honeydew","greenyellow",
                "gray90",colors(distinct=TRUE)))
colors.list <- colors.list[-grep("white",colors.list)]
colors.list <- colors.list[-grep("black",colors.list)]

options(shiny.maxRequestSize = 8000*1024^2)

plot.methods <- c('Auto','Random','circle',
                  'sphere','Fruchterman Reingold',
                  'Fruchterman Reingold Grid',
                  'Kamada Kawai',
                  'Spring',
                  'Reingold Tilford',
                  'LGL',
                  'star',
                  'Graphopt',
                  'RDPG',
                  'Laplacian',
                  't-SNE',
                  'Coordinates')

plot.invariants <- c("Degree Distribution",
                "Alpha Centrality",
                "Betweenness",
                "Closeness",
                "Eigenvalue Centrality",
                "Boncich Power Centrality",
                "Authority Score",
                "Hub Score",
                "Articulation Points",
                "Burt's Constraint",
                "Diversity",
                "Average KNN Degree",
                "Page Rank")


#openconnectome.dir <- "http://openconnecto.me/graph-services/download/"
#openconnectome.graphs <- getOpenConnectome(openconnectome.dir)

invariants <- data.frame(Invariant=c(
                           "Order",
                           "Size",
                           "#Components",
                           "Max Component Size",
                           "Max Degree",
                           "Average Degree",
                           "Min Degree",
                           "Degree Variance",
                           "Diameter",
                           "Girth",
                           "Density",
                           "Clique Number",
                           "Average Path Length",
                           "Centralization Degree",
                           "Reciprocity",
                           "Transitivity",
                           "Degree Assortativity",
                           "Maximal Coreness",
                           "Fast Greedy Communities",
                           "Greedy Domination Number",
                           "Power Law Fit (Exponent)"),
                        Function=c(
                           'vcount',
                           'ecount',
                           'no.clusters',
                           'max.component.size',
                           'get.max.degree',
                           'get.avg.degree',
                           'get.min.degree',
                           'get.var.degree',
                           'diameter',
                           'get.girth',
                           'graph.density',
                           'clique.number',
                           'average.path.length',
                           'get.cent',
                           'get.rec',
                           'transitivity',
                           'get.core',
                           'assortativity.degree',
                           'get.fastgreedy',
                           'dominate.num.greedy',
                           'get.pl'),
                       stringsAsFactors=FALSE)
                           

communities.list <- c(
   "Fast Greedy",
   "Edge Betweenness",
   "Infomap",
   "Leading Eigenvector",
   "Label Propagation",
   "Multilevel",
   "Walktrap",
   "RDPG",
   "Laplacian",
   "t-SNE",
   "Spinglass")

variables <-initializeVariables()

famous.graphs <- c("Bull","Chvatal","Coxeter","Cubical",
                   "Diamond","Dodecahedron","Folkman",
						 "Franklin","Frucht","Grotzsch",
						 "Heawood","Herschel","House","HouseX",
						 "Icosahedral,","Krackhardt","Levi",
						 "McGee","Meredith","Noperfectmatching",
						 "Nonline","Octahedral","Petersen",
						 "Robertson","Smallestcyclicgroup",
						 "Tetrahedron","Thomassen","Tutte",
						 "Uniquely3colorable","Walther","Zachary")
