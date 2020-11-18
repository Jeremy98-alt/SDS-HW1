library(igraph)
library(Rlab)

?graph_from_literal
?graph_from_edgelist

edges = cbind(1:4, c(2:4, 1))
G = graph_from_edgelist(edges, directed = T)
plot(G)

typeof(get.edgelist(G))

g <- make_(ring(10), with_vertex_(name = LETTERS[1:10])) +
  vertices('X', 'Y')
g
plot(g)

G <- G + vertex(5)
plot(G)

M = 10^6
gamma = 0.5
for (i in 1:M) {
  prob = rbern(1, prob = gamma)
  if (prob == 1) {
    
  }
}

for (i in 1:n) {
  frequency[indegree.distro] <- frequency[indegree.distro] + 1
}

indegree.distro = c(1,1,1,1,2,2,3,3,3,4,4,4,4,4,5,0,0)
frequency = rep(0, 6)
for (i in 1:length(indegree.distro)) {
  frequency[indegree.distro[i]+1] <- frequency[indegree.distro[i]+1] + 1
}
frequency
?hist
hist(frequency)

?degree()

?rmarkdown::pandoc_available
?sample
?add.edges
?get.edgelist()
?add.vertices

list <- c(1,2,3,4,5,6,7,8,9)
mask <- c(0,1,0,1,0,1,0,1,0)
list[as.logical(mask)]



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

createGraph <- function(graph, n) {
  for(i in 5:n) {
    randBern <- rbern(1, gamma)
    if(randBern == 1) {
      
    }
    else {
      
    }
  }
}
