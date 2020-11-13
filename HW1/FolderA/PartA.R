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
subset.V <- function(vertex.names, number_of_vertex){
  U <- c()
  
  for(i in 1:number_of_vertex){
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
num.edges.U <- function(U, list.edges){
  edges <- c()
  
  `%notin%` <- Negate(`%in%`) #define new operator
  for(row in 1:nrow(list.edges)){
    if( (list.edges[row, 1] %in% U) && (list.edges[row, 2] %notin% U) ){
      edges <- append(edges, c(list.edges[row, 1], list.edges[row, 2]) )
    }
  }

  return(length(edges))
}

#calculate M times and return the average cut size
averageCutSize <- function(vertex.names, list.edges, M){
  avg.cutsize <- c()
  
  for(i in 1:M){
    U <- subset.V(vertex.names, length(vertex.names)) #define the subset of V
    card.U <- num.edges.U(U, list.edges) #cardinality of U
    
    #append the result
    avg.cutsize <- append(avg.cutsize, card.U)
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

#call M times the averageCutSize
averageCutSize(vertex.names, list.edges, 1000)

# Theoretical Bound OPT(G)/2 ----------------------------------------------

theoretical.bound <- maxcut/2
theoretical.bound

# Change the graph size to see if there is an impact on the performance --------
library(igraph)

G2 <- erdos.renyi.game(20, 1/5)
plot(G2)

#firstly search to create our adjacency_matrix from G
adj2 <- as_adjacency_matrix(G2)
#convert S4 object to matrix
adj2 <- as.matrix(adj2)

#maximum cut
result2 <- maxcut(adj2)

#return the maximum cut, use the value returned
maxcut2 <- abs(result2$pobj)
maxcut2

#matrix correlation or... X
result2$Z

#show the result
vertex.names <- as_ids(V(G2))
U <- subset.V(vertex.names, gsize(G2))

adj2[lower.tri(adj2, diag=FALSE)] <- 0 

list.edges <- which(adj2==1, arr.ind = TRUE)
colnames(list.edges) <- c("V1", "V2")

theoretical.bound2 <- maxcut2/2
theoretical.bound2

averageCutSize(vertex.names, list.edges, 1000)

#Binomial distribution
colo <- c(rgb(32/255, 74/255, 135/255, 0.7))
          
#Set parameters
pp <- 0.5
nn <- 100

#Plot PMF
plot(0:nn, dbinom(0:nn, nn, pp),
     xlim = c(0,100), ylim = c(0,0.10),
     type="b", lty=3,
     xlab= "", ylab ="",
     col=gray(.8), pch=21, bg=colo)

grid()

legend("topleft", c("p = 0.5, n = 20", n = 100),
       col = colo, pch=19, bty="n", cex = .8)