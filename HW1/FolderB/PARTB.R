
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
    vertex.names <- as_ids(V(G))
  }
  
  return(G)
  
}

" get the vertex's names, in this case this function as_ids() 
create a vector of vertex ... or do as.character(V(G)) "
vertex.names <- as_ids(V(G))

newg <- createGraph(G, n = 10, vertex.names)
plot(newg, vertex.size=10, layout= layout.kamada.kawai, vertex.color="green", edge.curverd=.2, arrow.size=.1, arrow.width=.1, edge.arrow.size=.3, vertex.label.cex=.9)


# Find the degree distribution of the G graph -----------------------------

indegree.distro <- degree( newg, v = V(newg), mode = "in", loops = TRUE, normalized = FALSE ) # find the distribution

max.freq.value <- max(indegree.distro)
frequency.x <- rep(0, max.freq.value+1)
for (i in 1:length(indegree.distro)) {
  frequency.x[indegree.distro[i]+1] <- frequency.x[indegree.distro[i]+1] + 1
}
frequency.x

# Plot the PMF of X
plot(c(0:3), frequency.x, type="h",
     main = "in-degree distribution", xlab="in-degree", ylab="Number of vertices",
     lty=2, col=gray(.7))
points(c(0:3), frequency.x, pch=19, col="red")
