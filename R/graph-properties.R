## ##################################################################
##
## GRAPH PROPERTIES
##
## ##################################################################


## Is model defined by <glist> graphical and strongly decomposable
## if discrete=NULL, then the check is just if the graph is decomposable
## Issues: Fails on the "empty graph".

isGSD_glist <- function(glist, vn=unique(unlist(glist)), discrete=NULL)
{
  amat <- glist2adjMAT(glist,vn=vn)
  cliq <- maxCliqueMAT(amat)[[1]]
  isg  <- all(unlist(lapply(cliq, function(sss) isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcsmarkedMAT(amat,discrete=discrete)) > 0))
  }
}

properties_glist <- function(glist,
                             vn=unique(unlist(glist)),
                             amat=glist2adjMAT(glist,vn=vn),
                             cliq=maxCliqueMAT(amat)[[1]],discrete=NULL){

  isg <- all(unlist(lapply(cliq, function(sss) isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcsmarkedMAT(amat,discrete=discrete)) > 0))
  }
}

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
    is.DAGMAT( graphNEL2dgCMatrix(object) )
}

is.DAGMAT <- function(object){
    if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
    length(topoSort(object))>0
}

is.DAG.matrix <- is.DAG.Matrix <- is.DAGMAT

## ######################################

is.UG <- function(object){
    UseMethod("is.UG")
}

is.UG.graphNEL <- function(object){
    is.UGMAT( graphNEL2dgCMatrix(object) )
}

is.UGMAT <- function(object){
    if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
    isSymmetric(object)
}

is.UG.matrix <- is.UG.Matrix <- is.UGMAT

## ######################################

is.TUG <- function(object){
  UseMethod("is.TUG")
}

is.TUGMAT <- function(object){
    if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
    length(mcsMAT(object))>0
}

is.TUG.graphNEL <- function(object){
    z <- graphNEL2dgCMatrix(object)
    if (!is.UGMAT( z ))
        FALSE
    else
        length( ripMAT( z ) )>0
}

is.TUG.matrix <- is.TUG.Matrix <- is.TUGMAT

## ######################################

is.DG <- function(object){UseMethod("is.DG")}

is.DG.graphNEL <- function(object){
    is.DGMAT( graphNEL2dgCMatrix(object) )
}

is.DGMAT <- function(object){
    if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
    sum(object * t(object))==0
}

is.DG.matrix <- is.DG.Matrix <- is.DGMAT




