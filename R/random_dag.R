random_dag <- function(V, maxpar=3, wgt=0.1){
  V <- as.character(V)
  vparList <- vector("list", length(V))
  names(vparList) <- V
  for (ii in 1:length(V)){
    rest <- V[-(1:ii)] 
    zz <- 0:(min(maxpar, length(rest))-1)
    if (min(zz)<0)
      zz <- 0
    pp <- wgt^zz
    npar <- sample(zz, 1, prob=pp)
    vparList[[ii]] <- c(V[ii], sample(rest, npar, replace=FALSE))
  }
  
  dg <- dagList(vparList)
  dg
}
