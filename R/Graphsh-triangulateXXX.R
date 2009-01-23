
triangulateMAT <- function(amat, method="mcwh",
                           nLevels=rep(2,ncol(amat))){

  trimethod <- c("mcwh","r")
  method <- match.arg(tolower(method),trimethod)

  ##cat("triangulate - method:", method,"\n")
  switch(method,
         "r"={ ## Pure R implementations
           ans <- triangR(amat, nLevels=nLevels)
           if (matrix)
             ans <- as(ans,"matrix")
         },
         "mcwh"={
           A  <- amat
           Av <- as.numeric(A)
           nc <- ncol(A)
           vn <- colnames(A)
           i  <-.C("triangmcwh", Av=as.integer(Av), nc, vn,
                   as.integer(nLevels), ans=integer(1), PACKAGE="gRbase")$Av
           ans  <-matrix(i, nc=nc,nr=nc)

           dimnames(ans)<-dimnames(A)
           ans <- abs(ans)
           diag(ans) <- 0L           
         }
         )

  return(ans)
}



triangulate <- function(object, method="mcwh",
                        nLevels=rep(2,length(nodes(object))), matrix=FALSE){

  trimethod <- c("mcwh","r")
  method <- match.arg(tolower(method),trimethod)

  ##cat("triangulate - method:", method,"\n")
  switch(method,
         "r"={ ## Pure R implementations
           ans <- triangR(object, nLevels=nLevels)
         },
         "mcwh"={
           A  <- as.adjMAT(object)
           Av <- as.numeric(A)
           nc <- ncol(A)
           vn <- colnames(A)
           i  <-.C("triangmcwh", Av=as.integer(Av), nc, vn,
                   as.integer(nLevels), ans=integer(1), PACKAGE="gRbase")$Av
           ans  <-matrix(i, nc=nc,nr=nc)

           dimnames(ans)<-dimnames(A)
           if (!matrix){
             ans <- abs(ans)
             diag(ans) <- 0
             ans <- as(ans,"graphNEL")
           }
         }
         )

  return(ans)
}






## Sørens triangulation
##


triangR <- function(object, vn=nodes(object), nLevels=rep(2, length(vn))){
  
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

  as(amat2, "graphNEL")
}



## Triangulation based on an adjacency matrix
##
triangRMAT <- function(amat, vn=colnames(amat), nLevels=rep(2, ncol(amat))){
  
  amat2       <- amat 

  anodes     <- vn     
  activeList <- gnodes <- rep(1, length(vn))
  wgt        <- rep(NA,length(vn))
  
  names(activeList) <- names(gnodes) <- names(nLevels) <- names(wgt) <- vn

  repeat{
    for (ii in 1:length(anodes)){
      cn <- anodes[ii]
      if (activeList[cn]==1){
        nb <- (vn[(as.numeric(amat[cn,])* gnodes)==1])
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
}



jTree <- function(object,
                  method  = "mcwh",
                  nLevels = rep(2,length(nodes(object))),
                  control = list()){
  method <- match.arg(tolower(method),c("mcwh","r"))

  tug        <- triangulate(object, method=method, nLevels=nLevels)
  val        <- rip(tug,nLevels=nLevels)
  val$tug    <- tug
  return(val)
}


###
### JUNK BELOW HERE...
###  



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


