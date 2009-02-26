
moralize <- function(object){
  if ((class(object)=="graphNEL") && (edgemode(object)=="directed")){
    as(moralizeMAT(as.adjMAT(object)), "graphNEL")
  } else {
    stop("'object' must be graphNEL object with directed edges")
  }

}


moralizeMAT <- function(amat){

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
  return(1*(amat2 !=0))
  
}
