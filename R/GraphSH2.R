ugList <-  function(x){
  
  #isForm <-sapply(x, inherits, "formula")
  isForm <- unlistPrim(lapply(x, function(a) {"formula" %in% class(a)}))
  
  flist <- x[isForm]

  ans <- lapply(flist, function(f){
    tt    <- terms(f)
    ##glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
    glist <- strsplit(attr(tt,"term.labels"),":|\\*")
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



ugListMAT <-  function(x){
  n2p <- function(set){
    len <- length(set)
    if (len==2)
      return(set)
    idx1<-unlistPrim(lapply(1:(len-1), function(i) rep(i, len-i)))
    idx2<-unlistPrim(lapply(1:(len-1), function(i) seq(i+1, len)))
    ans<-cbind(set[idx1],set[idx2])
                                        #  ans <- split(ans, row(ans))
    ans
  }

  isForm <- unlistPrim(lapply(x, function(a) {"formula" %in% class(a)}))
  
  flist <- x[isForm]
  
  ans <- lapply(flist, function(f){
    tt    <- terms(f)
    ##glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
    glist <- strsplit(attr(tt,"term.labels"),":|\\*")
    V     <- rownames(attr(tt,"factors"))
    list(glist=glist,V=V)
  }) 
  
  
  gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
  gset <- c(gset, x[!isForm])
  
  V     <- lapply(ans, "[[", "V")
  V    <- unique(unlist(c(V, x[!isForm])))
  
  ed <- gset[lapply(gset,length)>1]
  
  if (length(ed)>0){    
    ed   <- do.call(rbind, lapply(ed, n2p))
    ed   <- split(ed, row(ed))
    ed   <- do.call(rbind, removeRedundant(ed))
    mm   <- matrix(matchPrim(ed,V),nc=2)
    amat <- matrix(0L, nr=length(V), nc=length(V))
    dimnames(amat) <- list(V,V)
    amat[rbind(mm,mm[,2:1])] <- 1L
  } else {
    amat <- matrix(0L, nr=length(V), nc=length(V))
    dimnames(amat) <- list(V,V)
  }
  amat    
}


ugMAT <- function(...){
  ugListMAT(list(...))
}


ug <- function(...){
  ugList(list(...))
}



dagListMAT <-  function(x){
  n2p <- function(set){
    len <- length(set)
    if (len==2)
      return(set)
    idx1<-unlistPrim(lapply(1:(len-1), function(i) rep(i, len-i)))
    idx2<-unlistPrim(lapply(1:(len-1), function(i) seq(i+1, len)))
    ans<-cbind(set[idx1],set[idx2])
                                        #  ans <- split(ans, row(ans))
    ans
  }

  isForm <- unlistPrim(lapply(x, function(a) {"formula" %in% class(a)}))
  
  flist <- x[isForm]
  
  ans <- lapply(flist, function(f){
    tt    <- terms(f)
    glist <- removeRedundant(strsplit(attr(tt,"term.labels"),":|\\*"))
    ##glist <- strsplit(attr(tt,"term.labels"),":|\\*")
    V     <- rownames(attr(tt,"factors"))
    list(glist=glist,V=V)
  }) 


  gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
  gset <- c(gset, x[!isForm])

  V    <- lapply(ans, "[[", "V")
  V    <- unique(unlist(c(V, x[!isForm])))

  gset<-gset[lapply(gset,length)>1]

  if (length(gset)>0){
    gset <- lapply(gset, function(xx) {
      if (length(xx)>2)
        cbind( xx[rep(1,length(xx)-1)], xx[2:length(xx)])
      else
        xx  
    })

    
    ed<- do.call(rbind, gset)
    mm   <- matrix(matchPrim(ed,V),nc=2)
    amat <- matrix(0L, nr=length(V), nc=length(V))
    dimnames(amat) <- list(V,V)
    amat[mm[,2:1]] <- 1L
  } else {
    amat <- matrix(0L, nr=length(V), nc=length(V))
    dimnames(amat) <- list(V,V)
  }


  ## Check if acyclic:
  ##
  is.acyc <- TRUE
  elorder <- NULL
  amat2 <- amat
  repeat{
    idx <- which(rowSums(amat2)==0)
    if (!length(idx)){
      return(NULL)
    }
    elorder <- c(elorder, idx)
    amat2 <- amat2[-idx,-idx]
    
    if(all(c(0,0)==dim(amat2))){
      break()
    }
  }
  zzz <- names(rev(elorder))
  
  if(!is.null(zzz))
    return(amat)
  else
    return(NULL)
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
  ##
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


dagMAT <- function(...){
  dagListMAT(list(...))
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



