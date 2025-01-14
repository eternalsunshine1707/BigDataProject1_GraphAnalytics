install.packages("igraph")
library(igraph)

opinions<-read.table("C:\\Users\\Sravani\\OneDrive\\Documents\\2024_SPRING\\CS6444 - BIG DATA\\Projects\\Project_1\\soc-Epinions1_adj.tsv")
optab<-as.matrix(opinions)
n <- 811480

#Function to create a graph from row 'a' to row 'b'
directed_graph<-function(a,b)
{
  relations<-data.frame(from = optab[a:b, 1], to = optab[a:b, 2])
  g<-graph.data.frame(relations, directed = TRUE)
  return(g)
}

#Plotting the first 100 rows
g_first100 <- directed_graph(1,100)
plot(g_first100,vertex.size=5,edge.arrow.size=0.1)

#Plotting the first 1000 rows
g_first1000 <- directed_graph(1,1000)
plot(g_first1000,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#Plotting the first 10000 rows
g_first10000 <- directed_graph(1,10000)
plot(g_first10000,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#Plotting the last 100 rows
g_last100 <- directed_graph(n-100,n)
plot(g_last100,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#Plotting the last 1000 rows
g_last1000 <- directed_graph(n-1000,n)
plot(g_last1000,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#Plotting the last 10000 rows
g_last10000 <- directed_graph(n-10000,n)
plot(g_last10000,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#Plotting all the rows
g <- directed_graph(1,n)
plot(g,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)


install.packages("sna")
library(sna)

#Vertices of the graph
V(g)

#Edges of the graph
E(g)

#Adjacency matrix
g.adj = igraph::get.adjacency(g)
g.adj

#ANALYTIC FUNCTIONS
#Graph Density
gden(data.frame(from = optab[,1], to = optab[,2]))

#Edge Density
edge_density(g)

#Ego of random vertex 1997
#ego(graph = g)[1997]
#g.ego=ego.extract(get.adjacency(g))

#degree
igraph::degree(g)

#histogram
hist(igraph::degree(g)) 

#Betweenness Centrality
g.between  = igraph::centr_betw(g)
g.between

#Closenesss Centrality
g.closeness  = igraph::centr_clo(g)
g.closeness

#Shortest path - length
igraph::shortest.paths(g)
g_first100.sp = igraph::shortest.paths(g_first100)
g_first100.sp
g_first10000.sp = igraph::shortest.paths(g_first10000)
g_first10000.sp

#Shortest path - Actual path
igraph::get.shortest.paths(g,2675)

#Simplify function
is.simple(g)
sg<-simplify(g)
is.simple(sg)
plot(sg,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#max cliques
node <- c(30)
g.adj_graph <- igraph::graph_from_adjacency_matrix(g.adj)
g.30clique = igraph::max_cliques(g.adj_graph,min = NULL,max = NULL,subset = node)
g.30clique

#Largest clique
g.lgcliques = igraph::clique_num(g.adj_graph)
g.lgcliques

#Detecting Structures using walktrap.community()
wc<-walktrap.community(g_first10000)
plot(wc,g_first10000,vertex.size = 5,vertex.label.cex=0.2, edge.arrow.size = 0.1, layout=layout.fruchterman.reingold,mark.col=c("tan", "pink", "lightgray"))
#Detecting Structures using walktrap.community()
wc<-walktrap.community(g_first100)
plot(wc,g_first100,vertex.size = 5,vertex.label.cex=0.2, edge.arrow.size = 0.1, layout=layout.fruchterman.reingold,mark.col=c("tan", "pink", "lightgray"))

#Central Nodes in the Graph
most_central <- which.max(degree(g, mode="all"))
most_central

bet <- betweenness(g)
most_central <- which.max(bet)
most_central

#Longest Paths
sg = induced.subgraph(g,which(components(g) $membership == 1 ))
V(sg)$degree = degree(sg)
result = dfs(sg,root=1,dist = TRUE)$dist
sort(result,decreasing = TRUE)

#print largest clique
largest.cliques(g)

#EGOS for All Nodes
ego.graph <- igraph::ego(g)
ego.graph

#Power Centrality
pc <- power_centrality(g_first10000,exponent = 0.8)
sort(pc,decreasing = TRUE)

#Alpha Centrality
acg <- alpha.centrality(g_first10000)
sort(acg,decreasing = TRUE)


#Simplify the graph by removing empty entries from the matrix, using these steps:
g_duplicate <-g
E(g_duplicate)$weight <-rnorm(ecount(g_duplicate))
V(g_duplicate)$weight <-rnorm(vcount(g_duplicate))
sg_duplicate <-induced.subgraph(g_duplicate, which(V(g_duplicate)$weight > 2.2))
plot(delete.vertices(sg_duplicate, degree(sg_duplicate)==0), edge.label = round(E(sg_duplicate)$weight,3), edge.arrow.size=0.1, vertex.label.cex=0.5, edgle.label.cex=0.3)

#SIMPLIFICATION
#================
#Nodes with degree 1
igraph::V(g)[igraph::degree(g)<1]
#Nodes with degree 2
igraph::V(g)[igraph::degree(g)<2]
#Nodes with degree 3
igraph::V(g)[igraph::degree(g)<3]
#Simplifying the graph by deleting the nodes with degree<3
v3<-igraph::V(g)[igraph::degree(g)<3]
g3<-igraph::delete.vertices(g,v3)
plot(g3,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#Simplifying the graph by deleting the nodes with degree<25
v25<-igraph::V(g)[igraph::degree(g)<25]
g25<-igraph::delete.vertices(g,v25)
plot(g25,vertex.size=5,edge.arrow.size=0.1,vertex.label = NA)

#Simplifying the graph by deleting the nodes with degree<60
v60<-igraph::V(g)[igraph::degree(g)<60]
g60<-igraph::delete.vertices(g,v60)
plot(g60)
igraph::clique_num(g60)