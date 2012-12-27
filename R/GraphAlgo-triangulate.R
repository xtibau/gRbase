
triangulate.graphNEL <- function(object, method="mcwh",
                        nLevels=rep(2,length(nodes(object))), result="graphNEL",...){
  triangulateMAT(as.adjMAT(object),
                 method=method, nLevels=nLevels, result=result, ...) 
}


triangulate.igraph <- function(object, method="mcwh",
                               nLevels=rep(2,length(V(object))), result="igraph",...){
  triangulateMAT(get.adjacency(object),
                 method=method, nLevels=nLevels, result=result, ...) 
}

triangulate.matrix <- function(object, method="mcwh",
                           nLevels=rep(2,ncol(object)), result="matrix", ...){
  triangulateMAT(object,
                 method=method, nLevels=nLevels, result=result, ...)
}

triangulate.Matrix <- function(object, method="mcwh",
                           nLevels=rep(2,ncol(object)), result="Matrix", ...){
  triangulateMAT(object,
                 method=method, nLevels=nLevels, result=result, ...)
}


triangulateMAT <- function(amat, method="mcwh",
                           nLevels=rep(2,ncol(amat)), result=NULL, ...){

  trimethod <- c("mcwh","r")                      ## FIXME: Not used
  method <- match.arg(tolower(method),trimethod)  ## FIXME: Not used
  
  cls <- class(amat)
  if (is.null(result))
    result <- cls
  else {
    result <- match.arg(result, c("matrix","graphNEL","igraph", "Matrix"))
  }

  if (cls =="dgCMatrix"){
    ans <- triangulateMAT_spR(amat)
  } else {
    ans <- triangulateMAT_stC(amat)
  }
  as(ans, result)
}


triangulateMAT_stC<- function(amat, method="mcwh", nLevels=rep(2,ncol(amat))){
  nc  <-  ncol(amat)
  vn  <-  colnames(amat)
  i   <- .C("triangmcwh", Av=as.integer(amat), nc, vn,
            as.integer(nLevels), ans=integer(1), PACKAGE="gRbase")$Av
  ans           <- matrix(i, ncol=nc, nrow=nc)           
  dimnames(ans) <- dimnames(amat)
  ans           <- abs(ans)   ## FIXME: Do this in C
  diag(ans)     <- 0L         ## FIXME: Do this in C  
  ans 
}


triangulateMAT_spR<- function(amat, vn=colnames(amat), nLevels=rep(2, length(vn))){

  if (is.null(vn))
    vn <- 1:ncol(amat)
  
  nvar    <- length(vn)
  lognlev <- log(nLevels)
  active  <- rep.int(1L, nvar)
  goon    <- 0L

  #cat(sprintf("initializing...\n"))
  .cqsize <- function(X,ii,lognlev){sum(lognlev[c(ii,which(sp_getXi(X,ii)>0))])}
  cqsize <- unlist(lapply(1:nvar, function(ii) .cqsize(amat,ii,lognlev)))
  #cat(sprintf("done\n"))
        
  while(goon<nvar){
    goon <- goon + 1L
    vii  <- which.min(cqsize)
    ne   <- (active==1) * sp_getXi(amat, vii) > 0
    nne  <- sum(ne)
    if (goon %% 1000 == 0)
      cat(sprintf("triangulate: node number=%6i node=%7s nne=%5i\n",  goon, vn[vii], nne))
    if (nne > 1){
      neii    <- which(ne)
      tf      <- names2pairs(neii, sort=FALSE, result="matrix")        
      vv      <- sp_getXtf(amat, tf)
      nedges  <- sum(vv)
      nfillin <- ((nne-1)*nne / 2) - nedges
      if (nfillin>0){
        amat <- sp_setXM1(amat, rbind(tf, tf[,2:1,drop=FALSE]))
        zz   <- unlist(lapply(neii, function(ii) .cqsize(amat,ii,lognlev)))
        cqsize[neii] <- zz
      }
    }
    active[vii] <- 0L
    cqsize[vii] <- 10000000L
  }
  dimnames(amat) <- list(vn,vn)
  amat  
}

## FIXME: Fra kommentarer ovenfor ser det ud som om dette ikke virker
triangulate_stR <- function(object, vn=nodes(object), nLevels=rep(2, length(vn))){
  
  amat       <- amat2 <- as.adjMAT(object)

  anodes     <- vn     
  activeList <- gnodes <- rep(1, length(vn))
  wgt        <- rep(NA,length(vn))
  
  names(activeList) <- names(gnodes) <- names(nLevels) <- names(wgt) <- vn

  repeat{
    ##cat("RUN\n")
    ##cat("activeList:", paste(vn[activeList==1], collapse=' '),"\n")
    ##print(activeList)
    ##print(gnodes)
    for (ii in 1:length(anodes)){
      cn <- anodes[ii]
      if (activeList[cn]==1){
        ##nb <-  intersect(anodes, names(which(amat[cn,]==1)))
        ##print(as.numeric(amat[cn,]))
        ##print(as.numeric(amat[cn,])* gnodes)
        ##print((as.numeric(amat[cn,])* gnodes)==1)
        nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
        ##nb <-  names((as.numeric(amat[cn,]) * gnodes)==1)
        w  <-  prod(nLevels[c(cn,nb)])
        wgt[cn] <- w 
        ##   cat("cn:", cn, "nb:", paste(nb, collapse=' '), "wgt:", w, "\n")
        activeList[cn] <- 0
      }
    }
    ##    print(wgt)
    id    <- which.min(wgt)
    wgt[id] <- Inf
    
    ##    print(id)
    cn <- vn[id]
    nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
    ## nb <- intersect(anodes, names(which(amat[cn,]==1)))
    activeList[cn] <- -1
    activeList[nb] <-  1
    
    ##   cat("completing bd for node:", cn, "nb:", paste(nb, collapse=' '), "\n")
    
    if (length(nb)>1){
      for (i in 1:(length(nb)-1)){
        for (j in (i+1):length(nb)){
          amat2[nb[i],nb[j]] <- amat2[nb[j],nb[i]] <- TRUE
          amat [nb[i],nb[j]] <- amat [nb[j],nb[i]] <- TRUE
        }
      }
    }

    gnodes[id] <- 0
    #print(anodes)
    anodes <- setdiff(anodes,cn)
    if (length(anodes)==1) 
      break()
                                        #    amat   <- amat[anodes, anodes]
  }
  return(amat2)
  #as(amat2, "graphNEL")
}











































