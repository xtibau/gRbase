ugList <-  function(x){
  
  isForm <-sapply(x, inherits, "formula")
  
  flist <- x[isForm]
  
  ans <- lapply(flist, function(f){
    tt    <- terms(f)
    glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
    V     <- rownames(attr(tt,"factors"))
    list(glist=glist,V=V)
  }) 
  
  gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
  V     <- lapply(ans, "[[", "V")
  
  gset <- c(gset, x[!isForm])
  V    <- unique(unlist(c(V, x[!isForm])))
  
  ed   <- unlist(lapply(gset, names2pairs, sort=FALSE), recursive=FALSE)
  ed   <- unique(lapply(ed, sort))
  ed   <- ed[sapply(ed,length)==2]
  
  if (length(ed)==0){
    value <-   new("graphNEL", nodes=V, edgemode="undirected")
  } else {
    value <- ftM2graphNEL(do.call(rbind, ed), V=V, edgemode="undirected")
  } 
  value
}


ug <- function(...){
  ugList(list(...))
}


dagList  <- function(x){
  isForm <-sapply(x, inherits, "formula")
  flist <- x[isForm]

  ans <- lapply(flist, function(f){
    tt    <- terms(f)
    glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
    V     <- rownames(attr(tt,"factors"))
    list(glist=glist,V=V)
  }) 


  gset  <-lapply(lapply(ans, "[[", "glist"), unlist)
  V     <- lapply(ans, "[[", "V")
  
  gset <- c(gset, x[!isForm])
  V    <- unique(unlist(c(V, x[!isForm])))
  
  gset <- lapply(gset, function(xx) names2pairs(xx[1],xx[-1], sort=FALSE))
  gset <- unlist(gset,recursive=FALSE)
  gset <- unique(gset)

  ed   <- gset[sapply(gset,length)==2]

  if (length(ed)==0){
    value <-   new("graphNEL", nodes=V, edgemode="directed")
  } else {
    value <- ftM2graphNEL(do.call(rbind, lapply(ed,rev)), V=V, edgemode="directed")
  } 

  ## Check if acyclic:

  is.acyc <- TRUE
  amat <- as.adjMAT(value)
  elorder <- NULL
  
  repeat{
    idx <- which(rowSums(amat)==0)
    if (!length(idx)){
      return(NULL)
    }
    elorder <- c(elorder, idx)
    amat <- amat[-idx,-idx]
    
    if(all(c(0,0)==dim(amat))){
      break()
    }
  }
  zzz <- names(rev(elorder))
  
  if(!is.null(zzz))
    return(value)
  else
    return(NULL)
}


dag <- function(...){
  dagList(list(...))
}



as.adjMAT <- function(object){
  if(!is(object, "graphNEL"))
    stop("Must be a graphNEL object...")
  
  ed    <- edges(object)
  vn    <- nodes(object)
  amat  <- matrix(0, nc=length(vn), nr=length(vn), dimnames=list(vn,vn))
  
  if(length(ed)>0){
    for (ii in 1:length(ed)){
      v <- names(ed)[[ii]]
      w <- ed[[ii]]
      amat[v,w] <- 1
    }
  }
  return(amat)
}



