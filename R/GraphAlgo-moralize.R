
moralize <- function(object,...){
  UseMethod("moralize")
}

moralize.graphNEL <- function(object, result="graphNEL", ...){
  if (edgemode(object)=="undirected"){
    stop("Graph must be directed")
  }
  moralizeMAT(as.adjMAT(object), result=result)
}

moralize.igraph <- function(object, result="igraph", ...){
  if (!is.directed(object)){
    stop("Graph must be directed")
  }
  moralizeMAT(get.adjacency(object), result=result)
}

moralize.matrix <- function(object, result="matrix", ...){
  moralizeMAT(object, result=result)
}

moralizeMAT <- function(amat, result="matrix"){

  result <- match.arg(result, c("matrix","graphNEL","igraph"))
  amat2 <- amat  
  for(kk in 1:ncol(amat)){
    idx <- which(amat[,kk]==1)
    lenidx <- length(idx)
    if (lenidx>1){ 
      for (ii in 1:(lenidx-1)){
        for (jj in (ii+1):lenidx) {
          amat2[idx[ii],idx[jj]] <- TRUE
        }
      }
    }
  }
  
  vn      <- colnames(amat2)
  amat2   <- amat2 + amat + t(amat2 + amat)
  ans     <- 1*(amat2 !=0)

  switch(result,
         "matrix"  ={return(ans)},
         "graphNEL"={return(as(ans, "graphNEL"))},
         "igraph"  ={return(graph.adjacency(ans, mode="undirected"))}
         )

}



## moralize <- function(object){
##   if ((class(object)=="graphNEL") && (edgemode(object)=="directed")){
##     as(moralizeMAT(as.adjMAT(object)), "graphNEL")
##   } else {
##     stop("'object' must be graphNEL object with directed edges")
##   }

## }
