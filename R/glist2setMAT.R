## Represent list of sets in a matrix...
##
## FIXME glist2setMAT could go to gRbase with a matrix/Matrix argument
glist2setMAT <- function(glist,vn=unique(unlist(glist))){
  amat <- matrix(0, nrow=length(glist), ncol = length(vn))
  colnames(amat) <- vn
  
  for (i in 1:length(glist)){
    amat[i, glist[[i]]] <- 1
  }
  amat
}
