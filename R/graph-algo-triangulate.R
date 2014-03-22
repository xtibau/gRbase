
triangulateMAT <- function(amat, nLevels=rep(2, ncol(amat)), ...){
    if (is.null(nLevels))
        nLevels=rep( 2, ncol(amat) )
    triangulateMAT_( amat, nLevels )
}

triangulate.default <- function(object, nLevels=NULL, result=NULL, ...)
{
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))
    if (is.null( result ))
        result <- cls

    switch(cls,
           "graphNEL" ={tt <- triangulateMAT( graphNEL2dgCMatrix(object), nLevels=nLevels )},
           "igraph"   ={tt <- triangulateMAT( igraph::get.adjacency(object), nLevels=nLevels)},
           "dgCMatrix"=,
           "matrix"   ={tt <- triangulateMAT( object, nLevels=nLevels )})
    as( tt, result )
}





## triangulate.graphNEL <- function(object, nLevels=NULL, result="graphNEL",...){
##     as(triangulateMAT( graphNEL2dgCMatrix(object), nLevels=nLevels ), result)
## }

## triangulate.igraph <- function(object, nLevels=NULL, result="igraph",...){
##   as(triangulateMAT( igraph::get.adjacency(object), nLevels=nLevels), result )
## }

## triangulate.matrix <- function(object, nLevels=NULL, result="matrix", ...){
##     as( triangulateMAT( object, nLevels=nLevels ), result)
## }

## triangulate.Matrix <- function(object, nLevels=NULL, result="Matrix", ...){
##     as(triangulateMAT( object, nLevels=nLevels ), result)
## }



## .matClass <- function(amat){
##   max( which(inherits(amat, c("matrix","Matrix","dgCMatrix"), which=TRUE) > 0 ))
## }

