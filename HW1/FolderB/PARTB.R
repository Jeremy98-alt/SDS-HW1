
# Create the graph with 4 nodes and so on.. -------------------------------------------
library(igraph)
library(Rlab)

G <- graph(c(2,3, 3,4, 4,1, 1,2), n = 4, directed=TRUE)
plot(G)

#to get the reproducibility
set.seed(140)

#probability to link to a page
gamma = 0.5

createGraph <- function(G, n, vertex.names){
  
  for(i in 5:n){
    randBern <- rbern(1, gamma) # 1: a link is to a page chosen uniformly at random - 0: copied from existing links.
    if(randBern == 1){
      i.th.vertex <- sample(vertex.names, size=1) #choose uniformly at random our page, see REPLACE=TRUE?
      G <- add_vertices(G, 1) %>%                    #add the link between the new page and the i-th vertex
        add_edges(c(i, i.th.vertex))
    } else {
      edge.list <- get.edgelist(G)
      dest.vertex <- sample(edge.list[,2], size=1) #choose uniformly at random our page, see REPLACE=TRUE?
      G <- add_vertices(G, 1) %>%                    #add the link between the new page and the destination vertex
        add_edges(c(i, dest.vertex))
    }
    vertex.names <- append(vertex.names, i)
  }
  
  return(G)
  
}

" get the vertex's names, in this case this function as_ids() 
create a vector of vertex ... or do as.character(V(G)) "
vertex.names <- as_ids(V(G))

#newg <- createGraph(G, n = 100, vertex.names)
#plot(newg, vertex.size=10, layout= layout.kamada.kawai, vertex.color="green", edge.curverd=.2, arrow.size=.1, arrow.width=.1, edge.arrow.size=.3, vertex.label.cex=.9)


# Find the in-degree distribution for M graphs -----------------------------

#count number of vertex into the k degree
indegree.distribution <- function(indegree.count.vertex){
  
  frequency.x <- rep(0, length(indegree.count.vertex))
  
  for (i in 1:length(indegree.count.vertex)) {
    frequency.x[indegree.count.vertex[i]+1] <- frequency.x[indegree.count.vertex[i]+1] + 1
  }
  
  return(frequency.x)
  
}

#input to get the M simulations
M <- 5
n.vertices <- 10000
#define total indegree 
total.indegree <- rep(0, n.vertices)

#simulate and get the indegree distribution mean
for(i in 1:M){
  newg <- createGraph(G, n = n.vertices, vertex.names)
  
  indegree.count.vertex <- degree( newg, v = V(newg), mode = "in", loops = TRUE, normalized = FALSE ) # find the distribution

  total.indegree <- total.indegree + indegree.distribution(indegree.count.vertex)

}

normalization.indegree <- total.indegree / (length(total.indegree) * M)
normalization.indegree[1:20]

#Plot the PMF of X
plot(c(0:max.freq.value), frequency.x, type="h",
     main = "in-degree distribution", xlab="in-degree", ylab="Number of vertices",
     lty=2, col=gray(.7), xlim=c(0,max.freq.value+1), ylim=c(0, max.freq.value+1), xaxt = "n", yaxt = "n", bty = "n")
axis(side = 1, pos = 0)
axis(side = 2, pos = 0)
points(c(0:max.freq.value), frequency.x, pch=19, col="red")

#Plot the Log-Log plot
plot(c(0:(n.vertices-1)), normalization.indegree,
     main = "in-degree distribution log-log plot", xlab="in-degree", ylab="Number of vertices",
     lty=2, col=gray(.7), log = "xy")
points(c(0:(n.vertices-1)), normalization.indegree, pch=19, col="red")

# Plot the CCDF of X .... I HAVE MY DOUBTS about this
plot(c(0, 0:(n.vertices-1), 0), 1-cumsum(c(0, normalization.indegree, 0)), type="s", log="xy",
     main = "complimentary cumulative degree distribution", xlab="In-degree", ylab="Probability",
     lty=1, lwd=2)
points(c(0:(n.vertices-1)), 1-cumsum(normalization.indegree), pch=19, col="red")



sum(normalization.indegree[1:16])

plot(c(0:(n.vertices-1)), 1-ecdf(normalization.indegree)(normalization.indegree), log="xy")
