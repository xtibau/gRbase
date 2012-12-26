##
## is.DAG, is.UG, is.TUG implemented for graphNEL, matrix and Matrix
##

is.adjMAT <- function(x){
  res <- FALSE

  if (inherits(x, c("matrix","Matrix"))){
    d <- dim(x)
    if (d[1L]==d[2L]){
      v <- 1:d[1L]
      if( all(x[cbind(v, v)]==0) ){
        res <- TRUE
      }
    }
  }
  res
}

## ######################################

is.DAG <- function(object){UseMethod("is.DAG")}
		
is.DAG.graphNEL <- function(object){
  if(edgemode(object)=="undirected"){
    FALSE
  } else {
    length(topoSort(as.adjMAT(object, "Matrix")))>0
  }
}

is.DAGMAT <- function(object){
  if (!is.adjMAT(object))
    stop("Matrix is not adjacency matrix...\n")  
  length(topoSort(object))>0
}

is.DAG.matrix <- is.DAG.Matrix <- is.DAGMAT
  
## ######################################

is.UG <- function(object){
  UseMethod("is.UG")
}

is.UGMAT <- function(object){
##  is.adjMAT(object) && (max(abs(t(object)-object))==0) ##isSymmetric.matrix(object)
  is.adjMAT(object) && isSymmetric(object)
}

is.UG.graphNEL <- function(object){
  edgemode(object)=="undirected"
}

is.UG.matrix <- is.UG.Matrix <- is.UGMAT

## ######################################

is.TUG <- function(object){
  UseMethod("is.TUG")
}

is.TUGMAT <- function(object){
  is.adjMAT(object) && (length(mcsMAT(object))>0)
}

is.TUG.graphNEL <- function(object){
  if (edgemode(object)=="undirected")
    is.triangulated(object)
  else
    FALSE
}

is.TUG.matrix <- is.TUG.Matrix <- is.TUGMAT





