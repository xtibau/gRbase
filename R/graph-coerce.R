##############################################################
####
#### Coercion between graphNEL, igraph and matrix
####
##############################################################

## FIXME: coercion: Are all possibilities accounted for ???
## FIXME: coercion: dgCMatrix is missing.

coerceGraph <- function(object, result){
  UseMethod("coerceGraph")
}

coerceGraph.graphNEL <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix","dgCMatrix","Matrix"))
  switch(result,
         "graphNEL"={object},
         "igraph"  ={gg <- igraph::igraph.from.graphNEL(object)
                     igraph::V(gg)$label <- igraph::V(gg)$name
                     gg
                   },
         "matrix" =,
         "Matrix" =,
         "dgCMatrix"={
           graphNEL2adjMAT(object, result=result)
         }
         )
}

coerceGraph.igraph <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix","dgCMatrix","Matrix"))
  switch(result,
         "graphNEL"={igraph::igraph.to.graphNEL(object)},
         "igraph"  ={object},
         "matrix"  ={as(igraph::get.adjacency(object),"matrix")},
         "Matrix"  =,
         "dgCMatrix"={MAT2dgCMatrix(igraph::get.adjacency(object))}
         )
}

coerceGraph.matrix <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix","dgCMatrix","Matrix"))
  switch(result,
         "graphNEL"={as(object,"graphNEL")},
         "igraph"  ={
             if (isSymmetric(object)){
             gg <- igraph::graph.adjacency(object, mode="undirected")
           } else {
             gg <- igraph::graph.adjacency(object, mode="directed")
           }
           igraph::V(gg)$label <- igraph::V(gg)$name
           gg
         },
         "matrix"  ={object},
         "Matrix"=,
         "dgCMatrix"={
             matrix2dgCMatrix( object )
         }
         )
}

coerceGraph.dgCMatrix <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix","dgCMatrix","Matrix"))
  switch(result,
         "graphNEL"={as(object,"graphNEL")},
         "igraph"  ={
             object <- dgCMatrix2matrix( object )
             if (isSymmetric(object)){
             gg <- igraph::graph.adjacency(object, mode="undirected")
           } else {
             gg <- igraph::graph.adjacency(object, mode="directed")
           }
           igraph::V(gg)$label <- igraph::V(gg)$name
           gg
         },
         "matrix"  ={
             dgCMatrix2matrix( object )},
         "Matrix"=,
         "dgCMatrix"={
             object
         }
         )
}










setOldClass("igraph")

setAs("graphNEL", "igraph", # input,output
      function(from){
        gg <- igraph::igraph.from.graphNEL(from)
        igraph::V(gg)$label <- igraph::V(gg)$name
        gg
      })

setAs("matrix", "igraph",
      function(from){
        if (isSymmetric(from)){
          gg <- igraph::graph.adjacency(from, mode="undirected")
        } else {
          gg <- igraph::graph.adjacency(from, mode="directed")
        }
        igraph::V(gg)$label <- igraph::V(gg)$name <- colnames(from)
        gg
      })

setAs("Matrix", "igraph",
      function(from){
        from <- as.matrix(from)
        if (isSymmetric.matrix(from)){
          gg <- igraph::graph.adjacency(from, mode="undirected")
        } else {
          gg <- igraph::graph.adjacency(from, mode="directed")
        }
        igraph::V(gg)$label <- igraph::V(gg)$name <- colnames(from)
        gg
      })

setAs("igraph",   "matrix",      function(from) as(igraph::get.adjacency(from),"matrix"))
setAs("igraph",   "graphNEL",    function(from) igraph::igraph.to.graphNEL(from))
setAs("igraph",   "Matrix",      function(from) MAT2dgCMatrix(igraph::get.adjacency(from)))
setAs("graphNEL", "matrix",      function(from) graphNEL2adjMAT(from, result="matrix"))
setAs("graphNEL", "Matrix",      function(from) graphNEL2adjMAT(from, result="Matrix"))
setAs("graphNEL", "dgCMatrix",   function(from) graphNEL2adjMAT(from, result="Matrix"))
