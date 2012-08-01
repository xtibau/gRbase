##############################################################
####
#### Coercion between graphNEL, igraph and matrix
####
##############################################################


coerceGraph <- function(object, result){
  UseMethod("coerceGraph")
}

coerceGraph.graphNEL <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix"))
  switch(result,
         "graphNEL"={object},
         "igraph"  ={gg <- igraph.from.graphNEL(object)
                     V(gg)$label <- V(gg)$name
                     gg
                   },
         "matrix"  ={as.adjMAT(object)})
}

coerceGraph.igraph <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix"))
  switch(result,
         "graphNEL"={igraph.to.graphNEL(object)},
         "igraph"  ={object},
         "matrix"  ={get.adjacency(object)})
}

coerceGraph.matrix <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix"))
  switch(result,
         "graphNEL"={as(object,"graphNEL")},
         "igraph"  =
         {
           if (isSymmetric(object)){
             gg <- graph.adjacency(object, mode="undirected")
           } else {
             gg <- graph.adjacency(object, mode="directed")
           }
           V(gg)$label <- V(gg)$name
           gg
         },
         "matrix"  ={object}) 
}


setOldClass("igraph")
setAs("graphNEL", "igraph",
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

setAs("igraph", "matrix",   function(from) get.adjacency(from))
setAs("igraph", "graphNEL", function(from) igraph.to.graphNEL(from))
