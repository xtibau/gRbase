## #######################################
##
## Interface to RcppEigen functions topological sorting of
## adjacency matrix
##
## Søren Højsgaard, December, 2012
## Known issues: None
## FIXME: Add tests, .Rd files and vignette
## #######################################

topoSort_sp <- function(XX_){
  .Call("C_topoSort_sp", XX_ 
        ,package="gRbase"
        )
}

topoSort_st <- function(XX_){
  .Call("C_topoSort_sp", XX_ 
        ,package="gRbase"
        )
}

topoSortMAT <- function(XX_, index=FALSE){
  if (inherits(XX_, "Matrix")){
    ans <- .Call("C_topoSort_sp", XX_ ,package="gRbase")
  } else {
    if (inherits(XX_, "matrix")){
      ans <- .Call("C_topoSort_st", XX_ ,package="gRbase")
    } else {
      stop("'XX_' must be a matrix or a sparse matrix (a 'dgCMatrix')")
    }
  }
  if (index){
    if (ans[1]!=-1){
      ans
    } else {
      -1L
    }
  } else {
    if (ans[1]!=-1){
      colnames(XX_)[ans]
    } else {
      character(0)
    }
  }
}
