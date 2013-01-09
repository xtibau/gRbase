## Find max cliques from adjacency matrix
##
## FIXME: Some check of input in maxCliqueMAT is needed...
##
maxCliqueMAT <- function(amat)
{
  vn <- dimnames(amat)[[2L]]
  ##   nv <- ncol(amat)
  ##   ne   <- ncol(em)

  if(class(amat)=="dgCMatrix"){
    ##em <- t(which((amat!=0), arr.ind=TRUE))
    ##em <- t(sp_fromto(amat))
    em <- t.default(sp_fromto(amat)) ## FIXME: Should be possible to transpose on cpp side
    #em <- sp_fromto(amat)
    #em <- rbind(em[,1],em[,2])
  } else {
    #em <- which.arr.ind2(amat)
    em <- t.default(st_fromto(amat)) ## FIXME: Should be possible to transpose on cpp side
  }
  
  ans2 <- maxClique(nodes=vn, edgeMat=em)
  ans2
}


