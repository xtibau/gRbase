
##
## Works only on triangulated graph
##
## Based on Algorithm 4.11 in Steffen et all (the yellow book)
##

rip <- function(object, root=NULL, nLevels=NULL){
  amat <- as.adjMAT(object)
  ripMAT(amat, root=root, nLevels=nLevels)
}

ripMAT <- function(amat, root=NULL, nLevels=NULL){
  
  t0 <- proc.time()
  vn <- colnames(amat)
                                        #cat("finding amat", proc.time()-t0,"\n"); t0 <- proc.time()
                                        #mcidx <- mcs(ug,amat=amat, root=root, index=TRUE)
  mcidx <- mcsMAT(amat=amat, root=root, index=TRUE)
                                        #cat("finding mcs", proc.time()-t0,"\n"); t0 <- proc.time()

  #cat("mcs", proc.time()-t0,"\n"); t0 <- proc.time()

##   cat("root:", root, "\n")
##   cat("mcidx:", mcidx,"\n")

  if (length(mcidx)==0)
    return(list())
  
  len <- length(mcidx)
  ladder <- is.ladder <- rep.int(0, len)
  is.ladder[len] <- 1
  
  cq <- list()
  cqcount <- 1
  for (ii in len:1){
    nb   <- amat[mcidx[ii],]
    prev <- rep(0, len)
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
      if (xx){ #print (mcidx[ii]); print (which(prevnb==1))
        cq[[cqcount]] <- c(mcidx[ii],which(prevnb==1))
        cqcount <- cqcount + 1
      }
      is.ladder[ii] <- xx
    }
  }
  
  cq <- lapply(rev(cq), function(x) {names(x)<-NULL; x})
  
  #cat("finding cliques", proc.time()-t0,"\n"); t0 <- proc.time()
  
  ncq <- length(cq)
  sp  <- as.list(rep(NA, ncq))
  pa  <- rep(0, ncq)

  #pa[1]<-character(0)
  
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
  #cat("finding sep/pa", proc.time()-t0,"\n"); t0 <- proc.time()

  sp <- lapply(sp, function(x)if (any(is.na(x))) character(0) else vn[x])
  cq <- lapply(cq, function(a) vn[a])
  
  ##sp    <- lapply(sp, function(a) if(length(a)==1 && is.na(a)) NA else vn[a])
  ##  sp    <- lapply(sp, function(a) if(length(a)==0 ) character(0) else vn[a])

  child <- match(seq_along(cq), pa)
  
  rip2 <-
    structure(list(nodes      = vn[mcidx],               
                   cliques    = cq,
                   separators = sp,
                   parents    = pa,
                   children   = child,
                   nLevels    = nLevels
                   ),
              class="ripOrder")
  
  return(rip2)

  ##sp[sapply(sp, length)==0] <- NA
  ##sp[unlistPrim(lapply(sp, function(x)any(is.na(x))))] <- character(0)
  ##sp[sapply(sp, length)==0] <- character(0)

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


