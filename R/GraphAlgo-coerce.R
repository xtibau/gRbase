##############################################################
####
#### Coercion between graphNEL, igraph and matrix
####
##############################################################

## FIXME: coercion: Are all possibilities accounted for ???

## FIXME: Should all the coerceGraph methods be deleted???
## THere may be speed issues

coerceGraph <- function(object, result){
  UseMethod("coerceGraph")
}

coerceGraph.graphNEL <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix","Matrix"))
  switch(result,
         "graphNEL"={object},
         "igraph"  ={gg <- igraph.from.graphNEL(object)
                     V(gg)$label <- V(gg)$name
                     gg
                   },
         "matrix","Matrix"  ={
           as.adjMAT(object, result=result)
         }
         )
}

coerceGraph.igraph <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix","Matrix"))
  switch(result,
         "graphNEL"={igraph.to.graphNEL(object)},
         "igraph"  ={object},
         "matrix"  ={as(get.adjacency(object),"matrix")},
         #"Matrix"  ={Matrix(get.adjacency(object),sparse=TRUE)}
         "Matrix"  ={.asdgCMatrix(get.adjacency(object))}
         )
}

coerceGraph.matrix <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix"))
  switch(result,
         "graphNEL"={as(object,"graphNEL")},
         "igraph"  ={
           if (isSymmetric(object)){
             gg <- graph.adjacency(object, mode="undirected")
           } else {
             gg <- graph.adjacency(object, mode="directed")
           }
           V(gg)$label <- V(gg)$name
           gg
         },
         "matrix"  ={object}
         ) 
}



setOldClass("igraph")
setAs("graphNEL", "igraph", # input,output
      function(from){
        gg <- igraph.from.graphNEL(from)
        V(gg)$label <- V(gg)$name
        gg
      }
      )

setAs("matrix", "igraph",
      function(from){ 
        if (isSymmetric(from)){
          gg <- graph.adjacency(from, mode="undirected")
        } else {
          gg <- graph.adjacency(from, mode="directed")
        }
        V(gg)$label <- V(gg)$name <- colnames(from)
        gg
      })

setAs("Matrix", "igraph",
      function(from){
        from <- as.matrix(from)
        if (isSymmetric.matrix(from)){
          gg <- graph.adjacency(from, mode="undirected")
        } else {
          gg <- graph.adjacency(from, mode="directed")
        }
        V(gg)$label <- V(gg)$name <- colnames(from)
        gg
      })

setAs("igraph",   "matrix",   function(from) as(get.adjacency(from),"matrix"))
setAs("igraph",   "graphNEL", function(from) igraph.to.graphNEL(from))
setAs("igraph",   "Matrix",   function(from) .asdgCMatrix(get.adjacency(from)))
setAs("graphNEL", "matrix",   function(from) as.adjMAT(from, result="matrix"))
setAs("graphNEL", "Matrix",   function(from) as.adjMAT(from, result="Matrix"))
