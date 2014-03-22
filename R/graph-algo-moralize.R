moralize <- function(object,...){
  UseMethod("moralize")
}

moralize.default <- function(object, result=NULL, ...)
{
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))
    if (is.null( result ))
        result <- cls

    switch(cls,
           "graphNEL" ={
               if (graph::edgemode(object)=="undirected")
                   stop("Graph must be directed")
               as( moralizeMAT(graphNEL2dgCMatrix(object)), result)
           },
           "igraph"   ={
               if (!igraph::is.directed(object))
                   stop("Graph must be directed")
               as( moralizeMAT(igraph::get.adjacency(object)), result)
           },
           "dgCMatrix"=,
           "matrix"   ={
               as( moralizeMAT(object), result)
           })
}


## moralize.graphNEL <- function(object, result="graphNEL", ...){
##   if (graph::edgemode(object)=="undirected")
##     stop("Graph must be directed")
##   as( moralizeMAT(graphNEL2dgCMatrix(object)), result)
## }

## moralize.igraph <- function(object, result="igraph", ...){
##   if (!igraph::is.directed(object)){
##     stop("Graph must be directed")
##   }
##   as( moralizeMAT(igraph::get.adjacency(object)), result)
## }

## moralize.matrix <- function(object, result="matrix", ...){
##     as( moralizeMAT(object), result)
## }

## moralize.Matrix <- function(object, result="Matrix", ...){
##   as( moralizeMAT(object), result )
## }

