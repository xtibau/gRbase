
## Returns matrix n x 2 matrix with indices of non-zero 
## entries in matrix m
which.arr.ind <- function(m){
  d <- nrow(m)
  rr<-rep(1:d, d)
  cc<-rep(1:d, each=d)
  epp<- cbind(rr[m!=0], cc[m!=0])
  epp
}

## Returns edges (pairs of vertices) of graph object.
## Based on lower triangular part of adjacency matrix; 
## hence for directed graphs it has the form (from, to)
edgeList <- function(object,matrix=FALSE){
  m <- as.adjMAT(object)
  m[upper.tri(m)] <- 0
  epp <- which.arr.ind(m)
  ans <- matrix(colnames(m)[epp],nc=2)
  if (!matrix)
    ans<- split(ans,row(ans))
  ans
}

## Returns edges (pairs of vertices) not in graph object.
## Based on lower triangular part of adjacency matrix; 
## hence for directed graphs it has the form (from, to)
nonEdgeList <- function(object,matrix=FALSE){
  m <- -1*as.adjMAT(object) + 1
  m[upper.tri(m, diag=TRUE)] <- 0
  epp <- which.arr.ind(m)
  ans <- matrix(colnames(m)[epp],nc=2)
  if (!matrix)
    ans<- split(ans,row(ans))
  ans
}

## Codes a p x 2 matrix of characters or a list with pairs
## of characters into a vector of numbers. 
pairs2num <- function(x, vn, sort=TRUE){

  if (inherits(x,"list"))
    x <- do.call(rbind,x)
  else {
    if (inherits(x,"character"))
      x <- matrix(x,nrow=1)
  }
  if (sort){    
    i     <- x[,2]< x[,1]
    c1    <- i+1
    c2    <- -1*(i-1) + 1
    x  <- cbind(
          x[cbind(seq_along(c1),c1)], 
          x[cbind(seq_along(c2),c2)])
  }
  ans       <- match(x,vn)
  dim(ans)  <- dim(x)
  ans       <- colSumsPrim(t(ans) * c(100000,1))
  ans
}
