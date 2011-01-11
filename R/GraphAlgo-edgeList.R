
edgeList <- function(object, matrix=FALSE)
  UseMethod("edgeList")

edgeList.graphNEL <- function(object, matrix=FALSE){
  m <- as.adjMAT(object)
  if (edgemode(object) == "undirected") 
    m[upper.tri(m)] <- 0
  edgeListMAT(m, matrix=matrix,edgemode(object))
}

edgeList.matrix <- function(object, matrix=FALSE){
  if (!all((t.default(object)-object)==0)){
    edgeListMAT(object, matrix=matrix, edgemode="directed")
  } else {
    edgeListMAT(object, matrix=matrix, edgemode="undirected")
  }
}


edgeListMAT <- function(adjmat, matrix=FALSE, edgemode="undirected"){
  if (edgemode == "undirected")
    adjmat[upper.tri(adjmat)] <- 0
  epp <- which.arr.ind(adjmat)
  ans <- matrix(colnames(adjmat)[epp], nc = 2)
  if (!matrix) 
    ans <- split(ans, row(ans))
  ans
}

nonEdgeList <- function(object, matrix=FALSE)
  UseMethod("nonEdgeList")

nonEdgeList.graphNEL <- function(object, matrix=FALSE){
  nonEdgeListMAT(as.adjMAT(object), matrix=matrix)
}

nonEdgeList.matrix <- function(object, matrix=FALSE){
  nonEdgeListMAT(object, matrix=matrix)
}

## Returns edges (pairs of vertices) not in graph adjmat.
## Based on lower triangular part of adjacency matrix; 
## hence for directed graphs it has the form (from, to)
nonEdgeListMAT <- function(adjmat,matrix=FALSE){
  m <- -1 * adjmat + 1
  m[upper.tri(m, diag=TRUE)] <- 0
  epp <- which.arr.ind(m)
  ans <- matrix(colnames(m)[epp],nc=2)
  if (!matrix)
    ans<- split(ans,row(ans))
  ans
}

