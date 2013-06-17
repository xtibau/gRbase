jTree <- function(object, ...){
  UseMethod("jTree")
}

jTree.graphNEL <- function(object, method  = "mcwh",
                  nLevels = rep(2,length(nodes(object))), ...){

  method <- match.arg(tolower(method),c("mcwh","r"))
  tug        <- triangulate(object, method=method, nLevels=nLevels, result="Matrix")
  val        <- ripMAT(tug,nLevels=nLevels)
  return(val)
}

jTree.matrix <- function(object, method  = "mcwh",
                         nLevels = rep(2,ncol(object)), ...){
  
  method <- match.arg(tolower(method),c("mcwh","r"))
  tug        <- triangulateMAT(object, method=method, nLevels=nLevels, result="Matrix")
  val        <- ripMAT(tug,nLevels=nLevels)
  return(val)
}

jTree.Matrix <- function(object, method  = "mcwh",
                         nLevels = rep(2,ncol(object)), ...){
  
  method <- match.arg(tolower(method),c("mcwh","r"))
  tug        <- triangulateMAT(object, method=method, nLevels=nLevels, result="Matrix")
  val        <- ripMAT(tug,nLevels=nLevels)
  return(val)
}

jTree.igraph <- function(object,  method  = "mcwh",
                  nLevels = rep(2,length(V(object))), ...){
  
  method <- match.arg(tolower(method),c("mcwh","r"))
  tug        <- triangulate(object, method=method, nLevels=nLevels, result="matrix")
  val        <- ripMAT(tug,nLevels=nLevels)
  return(val)
}


junctionTree <- jTree
junctionTree.graphNEL <- jTree.graphNEL
junctionTree.igraph   <- jTree.igraph
junctionTree.matrix   <- jTree.matrix
junctionTree.Matrix   <- jTree.Matrix


