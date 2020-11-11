#pick a specific (small) graph G
library(igraph)

#should be a RANDOM GRAPH
#g <- erdos.renyi.game(10, 1/2)
#plot(g)
G <- graph( edges=c(1,2, 2,3, 1,4, 4,3, 3,5, 5,6, 5,7),  n=7, directed=F)
plot(G)

#run the library and search our max-cut(G)
library(sdpt3r)

#firstly search to create our adjacency_matrix from G
adj <- as_adjacency_matrix(G, type = "both",
                           attr = NULL, edges = FALSE)
#convert S4 object to matrix
adj <- matrix(adj, ncol = gsize(G), nrow = gsize(G))

#maximum cut
result <- maxcut(adj)

#return the maximum cut, use the value returned
maxcut <- abs(result$pobj)
maxcut

#matrix correlation or... X
result$Z


# Step 2 - Run Max-Cut Algorithm M times ----------------------------------

#first install the package that contains the bernoulli distro 
install.packages('Rlab')
library(Rlab)

#create the function for our goal to obtain our subset of V
subset.V <- function(vertex.names, M){
  U <- c()
  
  for(i in 1:M){
    randBern <- rbern(1, 0.5)
    if(randBern == 1)
      U <- append(U, vertex.names[i])
  }
  
  return(U)
}

" get the vertex's names, in this case this function as_ids() 
create a vector of vertex ... or do as.character(V(G)) "
vertex.names <- as_ids(V(G))

#try M times to generate the subset of V
U <- subset.V(vertex.names, gsize(G))
U


# Average Cut-Size over these M simulations -------------------------------

#create subset U edges for the maxcut..
edges.U <- function(U, list.edges){
  edges <- c()
  
  `%notin%` <- Negate(`%in%`) #define new operator
  for(row in 1:nrow(list.edges)){
    if( (list.edges[row, 1] %in% U) && (list.edges[row, 2] %notin% U) ){
      edges <- append(edges, c(list.edges[row, 1], list.edges[row, 2]) )
    }
  }
  
  subgraph.U <- graph(edges, n=length(U), directed=F)
  return(subgraph.U)
}

#calculate M times and return the average cut size
averageCutSize <- function(vertex.names, M, list.edges){
  avg.cutsize <- c()
  
  for(i in 1:M){
    U <- subset.V(vertex.names, M)
    make.U.graph <- edges.U(U, list.edges)
    
    Ve <- V(make.U.graph)
    Ed <- E(make.U.graph)
    plot(make.U.graph)
    
    #make the adjacency matrix of U like in the previous example
    adjU <- as_adjacency_matrix(make.U.graph, type = "both",
                               attr = NULL, edges = FALSE)
    
    #convert S4 object to matrix
    adjU <- matrix(adjU, ncol = gsize(make.U.graph), nrow = gsize(make.U.graph))
    
    #append the result
    avg.cutsize <- append(avg.cutsize, abs(maxcut(adjU)$pobj) )
  }
  
  #finally return the mean!
  return(mean(avg.cutsize))
}

#set zero the Lower Triangular Part of a Matrix, 
#to avoid catching the duplicates edges...the graph is undirected!
adj[lower.tri(adj, diag=FALSE)] <- 0 

#create the list of edges from our G graph
list.edges <- which(adj==1, arr.ind = TRUE)
colnames(list.edges) <- c("V1", "V2") #change columns name..

plot(make.U.graph)

#call M times the averageCutSize
averageCutSize(vertex.names, gsize(G), list.edges)

# Theoretical Bound OPT(G)/2 ----------------------------------------------

theoretical.bound <- maxcut/2
theoretical.bound


# Change the graph size to see if there is an impact on the performance --------


