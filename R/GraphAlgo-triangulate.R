
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
                 method=method, nLevels=nLevels, result="matrix", ...)
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
  ## FIXME: Not sure whether as() should be here
  ## FIXME: If 'igraph' is it then ensured that the result is undirected??  
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
  active  <- rep.int(1L,nvar)
  goon    <- 0L

  #cat(sprintf("initializing...\n"))
  .cqsize <- function(X,ii,lognlev){sum(lognlev[c(ii,which(sp_getXi(X,ii)>0))])}
  cqsize <- unlist(lapply(1:nvar, function(ii) .cqsize(amat,ii,lognlev)))
  #cat(sprintf("done\n"))
        
  while(goon<nvar){
    goon <- goon + 1L
    vii  <- which.min(cqsize)
    ne   <- (active==1) * sp_getXi(amat,vii)>0
    nne  <- sum(ne)
    if (goon %% 1000 == 0)
      cat(sprintf("goon=%6i node=%7s nne=%5i\n",  goon, vn[vii], nne))
    if (nne>1){
      neii <- which(ne)
      tf   <- names2pairs(neii, sort=FALSE, result="matrix")        
      vv   <- sp_getXtf(amat, tf)
      nedges <- sum(vv)
      nfillin    <- ((nne-1)*nne / 2) - nedges
      if (nfillin>0){
        amat <- sp_setXM1(amat, rbind(tf, tf[,2:1,drop=FALSE]))
        #clii <- c(vii,neii)
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











## triangulateMAT <- function(amat, method="mcwh",
##                            nLevels=rep(2,ncol(amat)), result="matrix", ...){
##   ## Based on an adjacency matrix
##   trimethod <- c("mcwh","r")
##   method <- match.arg(tolower(method),trimethod)

##   ##cat("triangulate - method:", method,"\n")
##   switch(method,
##          "mcwh"={
##            nc  <-  ncol(amat)
##            vn  <-  colnames(amat)
##            i   <- .C("triangmcwh", Av=as.integer(amat), nc, vn,
##                      as.integer(nLevels), ans=integer(1), PACKAGE="gRbase")$Av
##            ans           <- matrix(i, ncol=nc, nrow=nc)           
##            dimnames(ans) <- dimnames(amat)
##            ans           <- abs(ans)   ## FIXME: Do this in C
##            diag(ans)     <- 0L         ## FIXME: Do this in C  
##          },         
##          "r"={ ## Pure R implementation  ## FIXME: Dette virker ikke...
##            ans <- triangR(amat, nLevels=nLevels)
##          })

##   switch(result,
##          "matrix"  ={return(ans)},
##          "graphNEL"={return(as(ans, "graphNEL"))},
##          "igraph"  ={return(graph.adjacency(ans, mode="undirected"))}
##          )
## }
















































## towards sparse matrices - but obsolete now
## triang2R <- function(amat, vn=colnames(amat), nLevels=rep(2, length(vn))){
  
##   amat2       <- amat

##   anodes     <- vn     
##   activeList <- gnodes <- rep(1, length(vn))
##   wgt        <- rep(NA,length(vn))
    
##   names(activeList) <- names(gnodes) <- names(nLevels) <- names(wgt) <- vn
##   count <- 0L
##   repeat{
##     count <- 0L
##     cat("activeList:", paste(vn[activeList==1], collapse=' '),"\n")
##     for (ii in 1:length(anodes)){
##       cn <- anodes[ii]
##       cni <- ii
##       print(c(cn, cni))
##       if (activeList[cn]==1){
##         nbi <- which((sp_getXi(amat,cni)*gnodes)==1)
##         nb  <- vn[nbi]
##         w   <- prod(nLevels[c(cni,nbi)])
##         wgt[cni] <- w
##         activeList[cni] <- 0
##         count <- count + 1L
##       }
##     }
    
##     print(wgt)
##     id    <- which.min(wgt)
##     print(unname(id))
##     wgt[id] <- Inf
##     cn <- vn[id]
##     nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
##     ## nb <- intersect(anodes, names(which(amat[cn,]==1)))
##     activeList[cn] <- -1
##     activeList[nb] <-  1    
##     ##   cat("completing bd for node:", cn, "nb:", paste(nb, collapse=' '), "\n")    
##     if (length(nb)>1){
##       for (i in 1:(length(nb)-1)){
##         for (j in (i+1):length(nb)){
##           amat2[nb[i],nb[j]] <- amat2[nb[j],nb[i]] <- TRUE
##           amat [nb[i],nb[j]] <- amat [nb[j],nb[i]] <- TRUE
##         }
##       }
##     }
##     gnodes[id] <- 0

##     anodes <- setdiff(anodes,cn) ## Expensive
##     if (length(anodes)==1) 
##       break()
##   }
##   return(amat2)
## }



## sp_triang <- function(amat, vn=colnames(amat), nLevels=rep(2, length(vn))){

##   nvar    <- length(vn)
##   active  <- rep.int(1L, nvar)
##   lognlev <- rep(0, nvar)
##   statespace <- 0
##   vidx    <- 1:nvar
  
##   for (ii in vidx){
##     lognlev[ii] <- log(nLevels[ii])
##     statespace  <- statespace + lognlev[ii]
##   }

##   goon = 0L
##   while(goon<nvar){
##     goon <- goon + 1L
##     mincqsize <- statespace; 
##     ##cat(sprintf("active=%s\n", toString(active)))
##     for (ii in vidx){
##       if (active[ii]==1L){
##         ## Find neighbours of ii in active set (nb) and #of neighbours in active set (nne).
##         allnb <- sp_getXi(amat,ii)
##         nbi <- which((allnb*active)==1)
##         nne <- length(nbi)
##         #nb  <- vn[nbi]
##         #cat(sprintf("node=%s allnb=%s nbi=%s\n", vn[ii], toString(allnb), toString(nbi)))
##         if (ii %% 10==0)
##           cat(sprintf("goon=%6i ii=%6i node=%s nbi=%s\n", goon, ii, vn[ii], toString(nbi)))
##         ## Find #of edges between nb (nedges)
##         if (nne>1){
##           tf <- names2pairs(nbi, sort=FALSE, result="matrix")        
##           vv <- sp_getXtf(amat, tf)
##           nedges <- sum(vv)
##         } else {
##           nedges <- 0
##         }
        
##         cqsize <- lognlev[ii]
##         if (length(nbi)>0){
##           for (jj in 1:length(nbi)){
##             ##if (nb[jj]!=0)
##             cqsize = cqsize + lognlev[jj];
##           }
##         }
## 	totedges <-  (nne-1)*nne / 2;
## 	nfillin  <- totedges - nedges;
## 	if ( (nfillin==0) || (cqsize < mincqsize) ){	  
## 	  mincqsize = cqsize;
## 	  mincqidx  = ii;
## 	  minfillin = nfillin;
## 	}
## 	if (nfillin==0)
## 	  break;
##       }
##     }
##     ## cat(sprintf("Chosen var: %s nfillin %i \n----\n", vn[mincqidx], minfillin))
##     active[mincqidx] = 0L;
##     if (minfillin>0){
##       allnb <- sp_getXi(amat,mincqidx)
##       nbi   <- which((allnb*active)==1)
##       nne   <- length(nbi)
##       ##nb    <- vn[nbi]

##       ## Find neighbours of mincqidx in active set (nb) and #of neighbours
##       ## in active set (nne). Find #of edges between nb (nedges)
##       pair <- names2pairs(nbi, sort=FALSE, result="matrix")
##       #print(pair)
##       #print(nbi)

##       amat <- sp_setXM1(amat, rbind(pair, pair[,2:1,drop=FALSE]))
      
## ##       for (ii in 1:(length(nbi)-1)){
## ##         ##if (nbi[ii]!=0){
## ##         for (jj in (ii+1):length(nbi)){
## ##           ##if (nbi[jj]!=0){
## ##           ##cat(sprintf("ii=%2i jj=%2i nb[ii]=%2i nb[jj]=%2i\n", ii, jj, nbi[ii], nbi[jj]))
## ##           if (amat[nbi[ii],nbi[jj]]==0){
## ##             amat[nbi[ii],nbi[jj]] = 1;
## ##             amat[nbi[jj],nbi[ii]] = 1;
## ##           }
## ##           ##}
## ##         }
## ## 	##} 
## ##       } #for
      
##       ##print(amat)      
##     }
##   }
##   amat
## }





## sp_triang2 <- function(amat, vn=colnames(amat), nLevels=rep(2, length(vn))){

##   nvar    <- length(vn)
##   active  <- rep.int(1L, nvar)
##   lognlev <- rep(0, nvar)
##   statespace <- 0
##   vidx    <- 1:nvar
  
##   ##cat(sprintf("active=%s\n", toString(active)))
##   for (ii in vidx){
##     if (active[ii]==1L){
##       ## Find neighbours of ii in active set (nb) and #of neighbours in active set (nne).
##       allnb <- sp_getXi(amat,ii)
##       nbi <- which((allnb*active)==1)
##       nne <- length(nbi)
##       ##nb  <- vn[nbi]
##       ##cat(sprintf("node=%s allnb=%s nbi=%s\n", vn[ii], toString(allnb), toString(nbi)))
##       ##if (ii %% 1000==0)
##       cat(sprintf("ii=%6i node=%7s length(nbi)=%5i\n",  ii, vn[ii], length(nbi)))
##       ## Find #of edges between nb (nedges)
##       if (nne>1){
##         tf <- names2pairs(nbi, sort=FALSE, result="matrix")        
##         vv <- sp_getXtf(amat, tf)
##         nedges <- sum(vv)
##       } else {
##         nedges <- 0
##       }        
##       totedges   <- (nne-1)*nne / 2;
##       nfillin    <- totedges - nedges;
##       active[ii] <- 0L
##       if (nfillin>0){
##         amat <- sp_setXM1(amat, rbind(tf, tf[,2:1,drop=FALSE]))
##       }
##     }
##   }
##   amat
## }












































##   for (ii in vidx){
##     lognlev[ii] <- log(nLevels[ii])
##     statespace  <- statespace + lognlev[ii]
##   }

##     ## cat(sprintf("Chosen var: %s nfillin %i \n----\n", vn[mincqidx], minfillin))
##     active[mincqidx] = 0L;
##     if (minfillin>0){
##       allnb <- sp_getXi(amat,mincqidx)
##       nbi   <- which((allnb*active)==1)
##       nne   <- length(nbi)
##       ##nb    <- vn[nbi]

##       ## Find neighbours of mincqidx in active set (nb) and #of neighbours
##       ## in active set (nne). Find #of edges between nb (nedges)
##       pair <- names2pairs(nbi, sort=FALSE, result="matrix")
##       #print(pair)
##       #print(nbi)

##       amat <- sp_setXM1(amat, rbind(pair, pair[,2:1,drop=FALSE]))
##     }
##   }
##   amat



##         cqsize <- lognlev[ii]
##         if (length(nbi)>0){
##           for (jj in 1:length(nbi)){
##             ##if (nb[jj]!=0)
##             cqsize = cqsize + lognlev[jj];
##           }
##         }

      
##       for (ii in 1:(length(nbi)-1)){
##         ##if (nbi[ii]!=0){
##         for (jj in (ii+1):length(nbi)){
##           ##if (nbi[jj]!=0){
##           ##cat(sprintf("ii=%2i jj=%2i nb[ii]=%2i nb[jj]=%2i\n", ii, jj, nbi[ii], nbi[jj]))
##           if (amat[nbi[ii],nbi[jj]]==0){
##             amat[nbi[ii],nbi[jj]] = 1;
##             amat[nbi[jj],nbi[ii]] = 1;
##           }
##           ##}
##         }
## 	##} 
##       } #for
      
      ##print(amat)      
































## triangulateMAT <- function(object, method="mcwh",
##                            nLevels=rep(2,ncol(object)), result="matrix", ...){

##   cat("NOTICE: triangulateMAT will be deprecated...\n")
##   trimethod <- c("mcwh","r")
##   method <- match.arg(tolower(method),trimethod)
##   ## FIXME: there is an issue about the return format
##   ## FIXME: argument matrix should be 'result'
##   object.ans <- triangulate.matrix(object=object,
##                                  method=method, nLevels=nLevels, result=result, ...) 
##   return(object.ans)
## }






## triangulate.matrix <- function(object, method="mcwh",
##                                nLevels=rep(2,ncol(object)), matrix=FALSE,...){
  
##   amat.ans <- .triangulate_internal(amat=object,
##                                     method=method, nLevels=nLevels, ...) 
##   return(amat.ans)
## }












## triangulateMAT <- function(amat, method="mcwh",
##                            nLevels=rep(2,ncol(amat)),...){

##   trimethod <- c("mcwh","r")
##   method <- match.arg(tolower(method),trimethod)

##   ##cat("triangulate - method:", method,"\n")
##   switch(method,
##          "r"={ ## Pure R implementations
##            ans <- triangR(amat, nLevels=nLevels)
##            if (matrix)
##              ans <- as(ans,"matrix")
##          },
##          "mcwh"={
##            A  <- amat
##            Av <- as.numeric(A)
##            nc <- ncol(A)
##            vn <- colnames(A)
##            i  <-.C("triangmcwh", Av=as.integer(Av), nc, vn,
##                    as.integer(nLevels), ans=integer(1), PACKAGE="gRbase")$Av
##            ans  <-matrix(i, nc=nc,nr=nc)
           
##            dimnames(ans)<-dimnames(A)
##            ans <- abs(ans)
##            diag(ans) <- 0L           
##          }
##          )

##   return(ans)
## }


## triangulate.graphNEL <- function(object, method="mcwh",
##                         nLevels=rep(2,length(nodes(object))), matrix=FALSE,...){

##   trimethod <- c("mcwh","r")
##   method <- match.arg(tolower(method),trimethod)

##   ##cat("triangulate - method:", method,"\n")
##   switch(method,
##          "r"={ ## Pure R implementations
##            ans <- triangR(object, nLevels=nLevels)
##          },
##          "mcwh"={
##            A  <- as.adjMAT(object)
##            Av <- as.numeric(A)
##            nc <- ncol(A)
##            vn <- colnames(A)
##            i  <-.C("triangmcwh", Av=as.integer(Av), nc, vn,
##                    as.integer(nLevels), ans=integer(1), PACKAGE="gRbase")$Av
##            ans  <-matrix(i, nc=nc,nr=nc)

##            dimnames(ans)<-dimnames(A)
##            if (!matrix){
##              ans <- abs(ans)
##              diag(ans) <- 0
##              ans <- as(ans,"graphNEL")
##            }
##          }
##          )

##   return(ans)
## }


##
## Purely R-based triangulation
##
## Notice: input is graphNEL; output is amat.
## FIXME: Make input and output consistent...













### ##################################################
###
### OLD STUFF BELOW HERE
###  
### ##################################################



##
## Triangulation based on an adjacency matrix
##
## This function is not used in the package but can probably be used
## as a benchmark for the C-implementation.
##
## triangRMAT <- function(amat, vn=colnames(amat), nLevels=rep(2, ncol(amat))){
  
##   amat2       <- amat 

##   anodes     <- vn     
##   activeList <- gnodes <- rep(1, length(vn))
##   wgt        <- rep(NA,length(vn))
  
##   names(activeList) <- names(gnodes) <- names(nLevels) <- names(wgt) <- vn

##   repeat{
##     for (ii in 1:length(anodes)){
##       cn <- anodes[ii]
##       if (activeList[cn]==1){
##         nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
##         w  <-  prod(nLevels[c(cn,nb)])
##         wgt[cn] <- w 
##         ##   cat("cn:", cn, "nb:", paste(nb, collapse=' '), "wgt:", w, "\n")
##         activeList[cn] <- 0
##       }
##     }
##     ##    print(wgt)
##     id    <- which.min(wgt)
##     wgt[id] <- Inf    
##     ##    print(id)
##     cn <- vn[id]
##     nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
##     activeList[cn] <- -1
##     activeList[nb] <-  1
    
##     ##   cat("completing bd for node:", cn, "nb:", paste(nb, collapse=' '), "\n")
    
##     if (length(nb)>1){
##       for (i in 1:(length(nb)-1)){
##         for (j in (i+1):length(nb)){
##           amat2[nb[i],nb[j]] <- amat2[nb[j],nb[i]] <- TRUE
##           amat [nb[i],nb[j]] <- amat [nb[j],nb[i]] <- TRUE
##         }
##       }
##     }

##     gnodes[id] <- 0
##     #print(anodes)
##     anodes <- setdiff(anodes,cn)
##     if (length(anodes)==1) 
##       break()
##                                         #    amat   <- amat[anodes, anodes]
##   }
##   return(amat2)
## }


## Original with PG's code
##
## jTree <- function(object,
##                   method  = "standard",
##                   vn      = nodes(object),
##                   nLevels = rep(2,length(vn)),
##                   control = list()){
##   trimethod <- c("standard","mcwh","r")
##   method <- match.arg(tolower(method),trimethod)

##   switch(method,
##          "standard"=,
##          "r"={
##            ###tug        <- triangulate(ug, method=method,nLevels=nLevels)
##            tug        <- triang(object, nLevels=nLevels)
##            #tug <<- tug
##            #nLevels <<- nLevels
##            ##val        <- ripOrder(tug,nLevels=nLevels)
##            val        <- RIP(tug,nLevels=nLevels)
##            val$tug    <- tug
##            return(val)
##          },
##          "mcwh"={
##            val         <- ripOrderGreen(object,vn,nLevels,control)
##            val$nLevels <- nLevels
##            ##val$tug     <- newugsh(val$cliques)
##            val$tug     <- newUGlist(val$cliques)
##            return(val)           
##          }         
##          )
## }

## triangR <- function(ug, vn=nodes(ug), nLevels=rep(2, length(vn))){
  
##   amat    <- amat2 <- as.adjmat(ug)
##   anodes  <- vn 
##   names(nLevels) <- vn

##   activeList <- rep(1, length(vn))
##   gnodes     <- rep(1, length(vn))
##   names(activeList) <- names(gnodes) <- vn
##   wgt   <- rep(NA,length(vn))
##   names(wgt) <- vn
##   repeat{
##     ##cat("RUN\n")
##     ##cat("activeList:", paste(vn[activeList==1], collapse=' '),"\n")
##     ##print(activeList)
##     ##print(gnodes)
##     for (ii in 1:length(anodes)){
##       cn <- anodes[ii]
##       if (activeList[cn]==1){
##         ##nb <-  intersect(anodes, names(which(amat[cn,]==1)))
##         ##print(as.numeric(amat[cn,]))
##         ##print(as.numeric(amat[cn,])* gnodes)
##         ##print((as.numeric(amat[cn,])* gnodes)==1)
##         nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
##         ##nb <-  names((as.numeric(amat[cn,]) * gnodes)==1)
##         w  <-  prod(nLevels[c(cn,nb)])
##         wgt[cn] <- w 
##         ##   cat("cn:", cn, "nb:", paste(nb, collapse=' '), "wgt:", w, "\n")
##         activeList[cn] <- 0
##       }
##     }
##     ##    print(wgt)
##     id    <- which.min(wgt)
##     wgt[id] <- Inf
    
##     ##    print(id)
##     cn <- vn[id]
##     nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
##     ## nb <- intersect(anodes, names(which(amat[cn,]==1)))
##     activeList[cn] <- -1
##     activeList[nb] <-  1
    
##     ##   cat("completing bd for node:", cn, "nb:", paste(nb, collapse=' '), "\n")
    
##     if (length(nb)>1){
##       for (i in 1:(length(nb)-1)){
##         for (j in (i+1):length(nb)){
##           amat2[nb[i],nb[j]] <- amat2[nb[j],nb[i]] <- TRUE
##           amat [nb[i],nb[j]] <- amat [nb[j],nb[i]] <- TRUE
##         }
##       }
##     }

##     gnodes[id] <- 0
##     #print(anodes)
##     anodes <- setdiff(anodes,cn)
##     if (length(anodes)==1) 
##       break()
##                                         #    amat   <- amat[anodes, anodes]
##   }
  
##   amat2[lower.tri(amat2)] <- FALSE
##   edmat <- edmat2 <- which(amat2, arr.ind=TRUE)
##   storage.mode(edmat2) <- "character"
##   dimnames(edmat2) <- NULL
##   edmat2[,1] <- vn[edmat[,1]]
##   edmat2[,2] <- vn[edmat[,2]]
##   ed <- split(edmat2, row(edmat2))
##   tug <- new("ugsh", nodes=vn, edges=ed)
##   ##print("DONE")
##   return(tug)
## }


