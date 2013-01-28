##################################################################
####
#### Find RIP-ordering of cliques of chordal (triangulated)
#### undirected graph
####
#### Based on Algorithm 4.11 in Steffen et all (the yellow book)
####
#### Known issues: Should check that amat specifies TUG; 
#### possibly controlled by forceCheck argument
####
##################################################################

rip <- function(object, root=NULL, nLevels=NULL){
  UseMethod("rip")
}

rip.graphNEL <- function(object, root=NULL, nLevels=NULL){
  if (edgemode(object)=="directed"){
    stop("Graph must be undirected")
  }
  ripMAT(as.adjMAT(object), root=root, nLevels=nLevels)
}

rip.igraph <- function(object, root=NULL, nLevels=NULL){
  if (is.directed(object)){
    stop("Graph must be undirected")
  }
  ripMAT(get.adjacency(object), root=root, nLevels=nLevels)
}

rip.matrix <- function(object, root=NULL, nLevels=NULL){
  ripMAT(object, root=root, nLevels=nLevels)
}

rip.Matrix <- function(object, root=NULL, nLevels=NULL){
  ripMAT(object, root=root, nLevels=nLevels)
}

.ripMAT <- function(amat, root=NULL, nLevels=NULL){
  ## FIXME: ripMAT Check that it is an adjMAT.
  cq.vn   <- maxCliqueMAT(amat)[[1]]
  ncq     <- length(cq.vn)
  vn      <- colnames(amat)
  
  if (ncq>1){
    mcidx   <- mcsMAT(amat, root=root, index=TRUE)
    cq.idx  <- lapplyV2I(cq.vn, vn)
    max_no  <- sapply(cq.idx, function(cc) max(mcidx[cc]))
    cq2.idx <- cq.idx[order(max_no)]
    cq2.vn  <- lapplyI2V(cq2.idx, vn)

    sp.idx  <- vector("list", ncq) 
    sp.idx[[1]] <- integer(0)
    pa.idx  <- rep.int(0L, ncq)

    for (ii in 2:ncq){
      paset <- unlist(cq2.idx[1:(ii-1L)])
      isect <- intersectPrim(cq2.idx[[ii]], paset)
      sp.idx[[ii]] <- isect  
      if (length(isect)){
        for (kk in (ii-1):1){  
          if (subsetof(isect,cq2.idx[[kk]])){
            pa.idx[ii]   <- kk  
            break()    
          }
        }
      }
    }
    nodes <- vn[mcidx]               
    sp.vn <- lapplyI2V(sp.idx, vn)
    child <- match(seq_along(cq2.idx), pa.idx)
    host  <- rep.int(0L, length(nodes))
    ll    <- lapply(cq2.vn, fmatch, nodes)
    for (ii in seq_along(ll)){
      host[ll[[ii]]] <- ii
    }
  } else { ## The graph has only one clique!
    cq2.vn <- list(vn)
    nodes  <- vn
    pa.idx <- 0L
    sp.vn  <- list(character(0))
    child  <- NA
    host <- rep.int(1L, length(nodes))    
  }

  .getChildList <- function(parents){
    vv <- 1:length(parents)
    ft <-unname(cbind(parents, vv))
    ft <- ft[ft[, 1] != 0, , drop = FALSE]
    vch<-lapply(vv, function(ff) ft[ft[,1]==ff,2])
    names(vch) <- vv
    vch
  }
  ## FIXME: Create ftM2vch and ftM2vpar from the above
  
  rip3 <-
    structure(list(nodes       = nodes,
                   cliques     = cq2.vn, separators = sp.vn,
                   parents     = pa.idx, children = child,
                   host        = host,
                   nLevels     = nLevels,
                   createGraph = .createJTreeGraph,
                   childList   = .getChildList(pa.idx)
                   ),
              class="ripOrder")

  return(rip3)
}



print.ripOrder <- function(x, ...){
  idx <- 1:length(x$cliques)
  cat("cliques\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"), x$cliques, idx)
  
  cat("separators\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"), x$separators, idx)
  
  cat("parents\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"), x$pa, idx)
  
#  cat("Children\n")
#  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x$ch, idx)
}

.createJTreeGraph <- function(rip){
  if (length(rip$cliques)>1){
    ft <-cbind(rip$parents, 1:length(rip$parents))
    ft <- ft[ft[,1]!=0,, drop=FALSE]
    V <- seq_along(rip$parents)
    if (nrow(ft)==0){
      jt <- new("graphNEL", nodes = as.character(V), edgemode = "undirected")
    } else {
      jt <- ftM2graphNEL(ft, V=as.character(V), edgemode="undirected")
    }
  } else {
    jt <- new("graphNEL", nodes = "1", edgemode = "undirected")
  }
  return(jt)
}

plot.ripOrder <- function(x,...){
  plot(x$createGraph(x))
}

.rip2dag<-function (rip) {
  if (length(rip$cliques) > 1) {
    ft <- cbind(rip$parents, 1:length(rip$parents))
    ft <- ft[ft[, 1] != 0, , drop = FALSE]
    V  <- seq_along(rip$parents)
    if (nrow(ft) == 0) {
      jt <- new("graphNEL", nodes = as.character(V), edgemode = "directed")
    } else {
      jt <- ftM2graphNEL(ft, V = as.character(V), edgemode = "directed")
    }
  } else {
    jt <- new("graphNEL", nodes = "1", edgemode = "directed")
  }
  return(jt)
}


## Old version; replaced by newer and much faster in January 2013
ripMAT <- function(amat, root=NULL, nLevels=NULL){
  
  t0 <- proc.time()
  vn <- colnames(amat)
  mcidx <- mcsMAT(amat=amat, root=root, index=TRUE)
  if (length(mcidx)==0) ## Graph is not chordal !
    return(list())  
  len <- length(mcidx)
  
  if (len>1) {
    ladder <- is.ladder <- rep.int(0, len)
    is.ladder[len] <- 1
    
    cq      <- list()
    cqcount <- 1
    for (ii in len:1){
      nb   <- amat[mcidx[ii],]
      prev <- rep(0, len)
      ##cat(sprintf("length(nb)=%i length(prev)=%i\n", length(nb), length(prev)))
      if (ii > 1){          
        prev[mcidx[1:(ii-1)]] <- 1
        prevnb <- nb*prev
        ladder[ii] <- sum(prevnb)
      }
      if (ii == len){
        cq[[cqcount]] <- c(mcidx[ii],which(prevnb==1))
        cqcount <- cqcount + 1
      } else {
        xx <- (ladder[ii] + 1 > ladder[ii+1])    #print(xx)
        if (xx){ 
          cq[[cqcount]] <- c(mcidx[ii],which(prevnb==1))
          cqcount       <- cqcount + 1
        }
        is.ladder[ii] <- xx
      }
    }
      
    cq <- lapply(rev(cq), function(x) {names(x)<-NULL; x})
    
    ncq <- length(cq)
    sp  <- as.list(rep(NA, ncq))
    pa  <- rep(0, ncq)
      
    if (ncq>1){
      for (ii in 2:ncq){
        paset <- unlist(cq[1:(ii-1)])
        isect <- intersectPrim(cq[[ii]], paset)
        sp[[ii]] <- isect  
        if (length(isect)){
          for (kk in (ii-1):1){  #print("----");print(kk); print(cq[[kk]]); print(isect)
            if (subsetof(isect,cq[[kk]])){
              pa[ii]   <- kk  
              break()    
            }
          }
        }
      }
    }
    
    sp <- lapply(sp, function(x)if (any(is.na(x))) character(0) else vn[x])
    cq <- lapply(cq, function(a) vn[a])
    child <- match(seq_along(cq), pa)  
  }  else {   ## The graph has only one node!
    cq <- list(colnames(amat))
    sp <- list(character(0))
    child <- NA
    pa <- 0
  }

  rip2 <-
    structure(list(nodes      = vn[mcidx],               
                   cliques    = cq, separators = sp, parents = pa, children = child,
                   nLevels    = nLevels, createGraph= .createJTreeGraph,
                   childList = lapply(graph::edges(.rip2dag(list(cliques=cq, parents=pa))), as.integer)                   
                   ),
              class="ripOrder")

  return(rip2)
}







## FIXME: Check issues: A graph with one clique only; a graph with one node only!
## ripMAT2 <- function(amat, root=NULL, nLevels=NULL){
##   vn  <- colnames(amat)
##   nvar <- length(vn)
##   cq  <- maxCliqueMAT(amat)$maxCliques
##   ncq <- length(cq)
##   cqi <- lapplyMatch(cq, vn)
##   mmi <- mcsMAT(amat, root=root, index=T)
##   vni <- as.character(1:length(vn))
##   cqi <- cqi[order(unlist(lapply(cqi, max)))] 
##   cq  <- lapplyI2V(cqi, vn)
  
##   ## FIXME: M could be sparse here
##   if (class(amat) =="dgCMatrix"){
##     #M   <- as(Matrix(0L, nrow=ncq,ncol=nvar, sparse=TRUE),"dgCMatrix") ## Clique matrix
##     M   <- .dgCMatrix(0L, nrow=ncq, ncol=nvar)
##   } else {
##     M   <- matrix(0L, nrow=ncq,ncol=nvar) ## Clique matrix
##   }
##   for (ii in 1:ncq){
##     M[ii,cqi[[ii]]] <- 1L
##   }
  
##   ##.Ppn <- function(x) {x[x>0] <- 1; x[x<0] <- 0;x}
##   ##MU <- apply(M,2,cumsum)
##   MU <- M
##   for (jj in 1:ncol(M)){
##     MU[,jj] <- cumsum(M[,jj])
##   }
##   ##MU <- apply(MU,2, .Ppn)
  
##   pa <- rep.int(0, ncq)
##   ch <- rep.int(NA, ncq)
##   sp <- vector("list", ncq)
##   ## care here if ncq=1
##   if (ncq>1){
##     for (ii in 2:ncq){
##       S <- M[ii,] * MU[ii-1,]
##       for(kk in 1:(ii-1)){
##         if(all((S*M[kk,]-S)==0)){
##           pa[ii]   <- kk
##           ch[kk]   <- ii
##           sp[[ii]] <- which(S>0)
##           break
##         }
##       }
##     }	
##   }
##   sp <- lapplyI2V(sp, vn)

##   ans <- list(nodes=vn,
##               cliques=cq, separators=sp, parents=pa, children=ch,
##               nLevels    = nLevels, createGraph= .createJTreeGraph,
##               childList = lapply(graph::edges(.rip2dag(list(cliques=cq, parents=pa))), as.integer) 
##               )
##   class(ans) <- "ripOrder"
##   ans
## }
