moralize <- function(object,...){
  UseMethod("moralize")
}

moralize.graphNEL <- function(object, result="graphNEL", ...){
  if (edgemode(object)=="undirected"){
    stop("Graph must be directed")
  }
  moralizeMAT(as.adjMAT(object, "Matrix"), result=result)
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

moralize.Matrix <- function(object, result="Matrix", ...){
  moralizeMAT(object, result=result)
}

moralizeMAT <- function(amat, result=NULL){

  cls <- class(amat)
  if (is.null(result))
    result <- cls
  else {
    result <- match.arg(result, c("matrix","graphNEL","igraph", "Matrix"))
  }
    
  if (cls =="dgCMatrix"){
    ans <- moralizeMAT_spC(amat)
  } else {
    ans <- moralizeMAT_stR(amat)
  }
  
  as(ans, result) 
}

## FIXME: moralizeMAT_stR: Calls for C-implementation
moralizeMAT_stR <- function(amat){
  amat2 <- amat  
  for(kk in 1:ncol(amat)){
    idx <- which(amat[,kk]==1)
    lenidx <- length(idx)
    if (lenidx>1){ 
      for (ii in 1:(lenidx-1)){
        for (jj in (ii+1):lenidx) {
          amat2[idx[ii],idx[jj]] <- 1L
        }
      }
    }
  }
  amat2 <- amat2+amat
  amat2 <- amat2+t(amat2)
  ans     <- 1*(amat2 !=0)    
  ans
}

moralizeMAT_spC <- function(amat){
  ans           <- sp_moralize(amat)
  dimnames(ans) <- dimnames(amat)
  ans
}










