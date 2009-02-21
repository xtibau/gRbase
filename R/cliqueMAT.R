## Find max cliques from adjacency matrix
##
maxCliqueMAT <- function(amat)
{
  ## Notice: No checks are made for whether the graph is undirected
  vn <- colnames(amat)
  nv <- ncol(amat)
  em <- t(which.arr.ind(amat))
  ne <- ncol(em)
  
  ans <- .Call("maxClique", 
               as.integer(nv), as.integer(ne), as.integer(em-1), 
               PACKAGE="RBGL")
  
  ans_names <- lapply(ans, function(x) { vn[x] })
  list("maxCliques"=ans_names)
}
