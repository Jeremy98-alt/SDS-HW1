library(igraph)
library(Rlab)

set.seed(140)

createLinks <- function(n.vertices, edges, gamma = 0.5) {
  vertices = c(1:n.vertices)
  for(i in 5:n.vertices) {
    randBern <- rbern(1, gamma)
    dest.v <- NULL
    if(randBern == 1) 
      dest.v <- sample(vertices[1:(i-1)], size = 1)
    else 
      dest.v <- sample(edges[,2], size = 1)
    edges <- rbind(edges, c(i, dest.v))
  }
  return(edges)
}

indegree.distribution <- function(indegree.count.vertex){
  
  frequency.x <- rep(0, length(indegree.count.vertex))
  
  for (i in 1:length(indegree.count.vertex)) {
    frequency.x[indegree.count.vertex[i]+1] <- frequency.x[indegree.count.vertex[i]+1] + 1
  }
  
  return(frequency.x)
  
}

n.v <- 10^4
edges <- matrix(c(c(1:4), c(2:4, 1)), nrow = 4, ncol = 2)

M <- 5
#define total indegree 
total.indegree <- rep(0, n.v)

#simulate and get the indegree distribution mean
for(i in 1:M){
  
  newg <- createLinks(n.vertices = n.v, edges)
  
  indegree.count.vertex <- tabulate(newg[,2], nbins = n.v)
  
  total.indegree <- total.indegree + indegree.distribution(indegree.count.vertex)
  
}

normalization.indegree <- total.indegree / (length(total.indegree) * M)


#Plot the Log-Log plot
plot(c(0:(n.v-1)), normalization.indegree,
     main = "in-degree distribution log-log plot", type='l', lwd=3, xlab="in-degree", ylab="Number of vertices",
     lty=2, col=gray(.7), log = "xy")
points(c(0:(n.v-1)), normalization.indegree, pch=19, col="red")

# Plot the CCDF of X .... I HAVE MY DOUBTS about this
plot(c(0, 0:(n.v-1), 0), 1-cumsum(c(0, normalization.indegree, 0)), type="s", log="xy",
     main = "complimentary cumulative degree distribution", xlab="In-degree", ylab="Probability",
     lty=1, lwd=2)
points(c(0:(n.v-1)), 1-cumsum(normalization.indegree), pch=19, col="red")


m <- matrix(c(c(1:4), c(2:4, 2)), nrow = 4, ncol = 2)
tabulate(m[,2])
m

library(VGAM)
plot(1:10000, dzeta(1:10000, shape = 4), type = "l", lwd = 3, las = 1, ylab = "Probability",
     main = "zeta probability function; black: p = 4; blue: p = 2", log = 'xy')
points(0.10 + 1:6, dzeta(1:6, shape = 4), col = "blue")
?plot


?erdos.renyi
