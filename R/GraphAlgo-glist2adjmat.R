##################################################
##
## Convert list of generators to adjacency matrix
## (works for undirected graphs)
##
##################################################

## glist2adjMAT <- function(glist, vn=unique(c(glist, recursive=TRUE))){

##   if (length(glist)==0)
##     return(NULL)
  
##   amat <- matrix(0L, nc=length(vn), nr=length(vn))
##   dimnames(amat) <- list(vn,vn)
  
##   for (ii in 1:length(glist)){
##     gg <- glist[[ii]]
##     len_gg <- length(gg)
##     if (len_gg>1){
##       for (jj in 1:(len_gg-1)){
##         for (kk in (jj+1):len_gg){
##           amat[gg[jj],gg[kk]] <- amat[gg[kk],gg[jj]] <- 1L
##         }
##       }
##     }
##   }
##   amat
##  }  

glist2adjMAT <- function(glist, vn=uniquePrim(c(glist, recursive=TRUE))){

  nr <- length(vn)
  amat  <- matrix(0L, nc=nr, nr=nr, dimnames=list(vn,vn))
  
  if(length(glist)>0){
    for (ii in 1:length(glist)){
      gg <- glist[[ii]]
      amat[gg,gg] <- 1L
    }
  }

  iii <- 1+(nr+1)*((1:nr)-1)
  amat[iii] <- 0L ## faster than diag(sss)
  amat
}











