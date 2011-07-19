
## Is model defined by <glist> graphical and strongly decomposable
## if discrete=NULL, then the check is just if the graph is decomposable
## Issues: Fails on the "empty graph".
isGSD_glist <- function(glist, vn=unique(unlist(glist)), discrete=NULL) 
{
  amat <- glist2adjMAT(glist,vn=vn)
  cliq <- maxCliqueMAT(amat)[[1]]
  isg  <- all(unlist(lapply(cliq, function(sss) isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcsmarkedMAT(amat,discrete=discrete)) > 0)) 
  }
}

properties_glist <- function(glist,
                             vn=unique(unlist(glist)), 
                             amat=glist2adjMAT(glist,vn=vn),
                             cliq=maxCliqueMAT(amat)[[1]],discrete=NULL){
  
  isg <- all(unlist(lapply(cliq, function(sss) isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcsmarkedMAT(amat,discrete=discrete)) > 0)) 
  }
}
