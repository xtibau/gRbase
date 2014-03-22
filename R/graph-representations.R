MAT2matrix <- function(amat){
  if( class(amat)=="dgCMatrix"){
    dgCMatrix2matrix(amat)
  } else if( class(amat)=="matrix"){
    amat
  } else { stop("Can not convert 'amat'")}
}

MAT2dgCMatrix <- function(amat){
  if( class(amat)=="matrix"){
    matrix2dgCMatrix(amat)
  } else if( class(amat)=="dgCMatrix"){
    amat
  } else { stop("Can not convert 'amat'")}
}





## Represent list of sets in a matrix...
##
glist2setMAT <- function(glist,vn=unique(unlist(glist))){
  amat <- matrix(0, nrow=length(glist), ncol = length(vn))
  colnames(amat) <- vn

  for (i in 1:length(glist)){
    amat[i, glist[[i]]] <- 1
  }
  amat
}



##################################################
##
## Convert list of generators to adjacency matrix
##
##################################################

## glist: A list of vectors of the form (v, pa1, pa2, ... pan)
vpaList2adjMAT <- function(glist, vn=unique(unlist(glist)), result="matrix"){
    result <- match.arg(result, c("matrix", "Matrix", "dgCMatrix"))
    switch(result,
           "Matrix"=,"dgCMatrix"={dagList2dgCMatrix(glist, vn)},
           "matrix"={dagList2matrix(glist, vn)}
           )
}

## glist: A list of vectors of the form (v1, v2, ... vn)
glist2adjMAT <- function(glist, vn=unique(unlist(glist)), result="matrix"){
    result <- match.arg(result, c("matrix","Matrix","dgCMatrix"))
    switch(result,
           "Matrix"=,"dgCMatrix"={ugList2dgCMatrix(glist, vn)},
           "matrix"={ugList2matrix(glist, vn)}
           )
}

## adjList : named list as returned by graph::edges( )
adjList2adjMAT <- function(adjList, result="matrix"){
    result <- match.arg(result, c("matrix", "Matrix", "dgCMatrix"))
    switch(result,
           "matrix"={adjList2matrix(adjList)},
           "Matrix"=,"dgCMatrix"={adjList2dgCMatrix( adjList )})

}

graphNEL2adjMAT  <- function(object, result="matrix"){
  if(!inherits(object, "graphNEL"))
      stop("'object' must be a graphNEL object...")
  adjList2adjMAT( graph::edges(object), result=result )
}

as.adjMAT <- graphNEL2adjMAT
graphNEL2matrix <- function(object){ as.adjMAT(object, result="matrix") }
graphNEL2dgCMatrix <- function(object){ as.adjMAT(object, result="Matrix") }

## vpaL2tfM: (v,pa(v))-list 2 to-from-matrix
## FIXME vpaL2tfM: rename to vpaList2ftM; used in topoSort
vpaL2tfM <- function(vpaL){
 eMat  <- lapply(vpaL, function(xx) names2pairs(xx[1], xx[-1],
                                                 sort = FALSE, result = "matrix"))
 do.call(rbind, eMat)
}

graphNEL2ftM <- function(object){
    adjList2ftM(graph::edges(object))
}

graphNEL2tfM <- function(object){
    adjList2tfM(graph::edges(object))
}







