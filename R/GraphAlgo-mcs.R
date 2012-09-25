##
## Maximum Cardinality Search, February, 2009
##
## Returns perfect ordering if it exists and character(0) otherwise
##

mcs <- function(object, root=NULL, index=FALSE){
  UseMethod("mcs")
}

mcs.graphNEL <- function(object, root=NULL, index=FALSE){
  mcsMAT(as.adjMAT(object), root=root, index=index)
}

mcs.igraph <- function(object, root=NULL, index=FALSE){
  mcsMAT(get.adjacency(object),root=root, index=index)
}

mcs.matrix <- function(object, root=NULL, index=FALSE){
  mcsMAT(object, root=root, index=index)
}

mcsMAT <- function (amat, vn = colnames(amat), root = NULL, index = FALSE){
  cls <- class(amat)
  if (cls =="dgCMatrix"){
    ans <- mcsMAT_spR(amat, vn=vn, root=root, index=index)
  } else {
    ## FIXME : There is a bug in C code for mcs
    ##ans <- mcsMAT_stC(amat, vn=vn, root=root, index=index)
    ans <- mcsMAT_stR(amat, vn=vn, root=root, index=index)
  }
  ans
}

mcsMAT_stC <- function (amat, vn = colnames(amat), root = NULL, index = FALSE){
  if (is.null(root)){
    root    <- vn
    rootNUM <- 0:(length(vn)-1)
  } else {
    root    <- c(root, setdiffPrim(vn, root))
    rootNUM <- match(root, vn)-1L
  }  
  
  ans<-.C("C_mcs", A=as.integer(amat), nc=ncol(amat), root=rootNUM,
          ans=integer(ncol(amat))
          ,PACKAGE="gRbase"
          )$ans  
  ret <- if (ans[1]==-1){
    character(0)
  } else {
    if (index) {
      ans + 1
    } else {
      colnames(amat)[ans+1]
    }
  }
  return(ret)
}

###
### R version for sparse matrices 
###
mcsMAT_spR <- function(amat, vn=colnames(amat), root=NULL, index=FALSE){
  
  is.perfect <- TRUE
  if (is.null(vn))
    vn <- 1:ncol(amat)

  ## Make 'root' the variables to be searched first when finding elimination order.
  vn.orig <- vn
  if (!is.null(root)){ 
    vn2      <- c(root, setdiffPrim(vn, root))
    neworder <- match(vn2, vn)
    amat     <- amat[neworder,neworder]
    vn       <- vn2
  }

  nvar    <- length(vn)
  active  <- nnvec <- ans <- rep.int(0L, nvar)
  passive <- rep.int(1L, nvar)
  
  cnode   <- 1
  for (kk in 1:nvar){
    ans[kk]        <- cnode #;cat("cnode:", cnode, "\n")
    active[cnode]  <- 1L
    passive[cnode] <- 0L 
    nb             <- sp_getXi(amat, cnode)
    nbidx          <- which((nb*active)==1) 
    len            <- length(nbidx)
    if (kk %% 1000==0)
      cat(sprintf("kk=%5i cnode=%6s, length(nbidx)=%s\n", kk, cnode, length(nbidx)))
    if (len>1){
      tf <- names2pairs(nbidx, sort=FALSE, result="matrix")
      vv <- sp_getXtf(amat, tf)
      if (any(vv==0)){
        is.perfect <-  FALSE
        break()
      }    
    }
    
    nnvec <- nnvec + nb
    if (max(nnvec * passive)==0){
      cnode <- which(passive==1)[1]
    } else {
      cnode <- which.max(nnvec * passive)
    }
  }
  
  if (is.perfect){
    names(ans)<-vn    
    if (index)
      return(match(vn[ans],vn.orig))
    else
      return(vn.orig[match(vn[ans],vn.orig)])
  } else {
    return(character(0))
  }
  
}


### mcs for adjacency matrices
###
### September 2012; reimplementation
### There seems to be a bug in the C-code so for now this version is used.
###
mcsMAT_stR <- function(amat, vn=colnames(amat), root=NULL, index=FALSE){

  .whichPrim <- function(x){seq_along(x)[x]}

  is.perfect <- TRUE
  vn.orig <- vn
  if (!is.null(root)){
    vn <- c(root, setdiff(vn, root))
    rootNUM <- match(vn, colnames(amat))
    use.root <- TRUE
  } else {
    rootNUM <- seq_along(vn)
    use.root <- FALSE
  }
  
  res <- act <- rep(0L, dim(amat)[1L])
  nidx      <- rootNUM[1L] ## node index

  act[nidx] <- 1L ## first node marked as 'passive'
  res[1]    <- nidx ## number of first node
  nnodes    <- length(res)
  
  ## iterate
  if (nnodes>1){
    for (ii in 2:nnodes){
      
      ## Some saving can be made here as we do not need to consider 
      ## the rows of amat corresponding to nodes that are already numbered
      nn <- amat %*% act ## numbered neighbours
      nn[act>0] <- -1L
      
      if (use.root){
        nidx.cand <- .whichPrim(nn==max(nn)) ## candidates to be numbered
        if (any(rootNUM[ii]==nidx.cand)){
          nidx <- rootNUM[ii]
        } else {
          nidx <- nidx.cand[1]
          use.root <- FALSE
        }
      } else {
        nidx <- which.max(nn)
      }
      
      bd <- as.numeric(amat[nidx,] * act)
      
      bd.idx <- .whichPrim(bd>0)
      n.bd   <- length(bd.idx)
      ## check that subgraph defined by bd.idx is complete
      ## if not then exit      
      if(sum(amat[bd.idx,bd.idx]) != n.bd*(n.bd-1L)){
        is.perfect <- FALSE
        break
      }
      
      act[nidx] <- 1L
      res[ii] <- nidx
    }

  }
    if (is.perfect){
      if (index)
        res
      else
        vn.orig[res]
    } else {
      character(0)
    }
  
}





###
### R version for comparison with C-implementation
###
### A more recent (and faster) and faster version is used
###
## .mcsMAT_stR <- function(amat, vn=colnames(amat), root=NULL, index=FALSE){
  
##   is.perfect <- TRUE

##   ## Make 'root' the variables to be searched first when finding elimination order.

##   vn.orig <- vn
##   if (!is.null(root)){ 
##     vn2      <- c(root, setdiffPrim(vn, root))
##     neworder <- match(vn2, vn)
##     amat     <- amat[neworder,neworder]
##     vn       <- vn2
##   }

##   #print(vn)
##   cnode   <- 1
##   active  <- nnvec <- rep(0L,length(vn))
##   passive <- rep(1L,length(vn))
##   ans     <- NULL
  
##   amat <- amat*1
  
##   for (kk in 1:length(vn)){
##                                         #cat("cnode:", cnode, "\n")
##     ans            <- c(ans, cnode)
##     active[cnode]  <- 1
##     passive[cnode] <- 0 
##     nb             <- amat[cnode,]    
##     is.comp <- TRUE
##     nbidx   <- which((nb*active)==1) #print(nbidx)                                        
##     len   <- length(nbidx)
##     if (len>1){
##       for (ii in 1:(len-1)) { #cat ("ii", ii, "vnii:", vn[nbidx[ii]], "\n")                                       
##         for(jj in (ii+1):(len)) { #cat ("  jj", jj, "vnjj:", vn[nbidx[jj]], "\n")    
##           if (amat[nbidx[ii],nbidx[jj]]==0){
##             is.comp <- FALSE
##             break()
##           }
##         }
##       }
##     }
##     is.perfect <- is.comp
    
##     if (!is.perfect){
##       #cat("NOT perfect\n"); print(cnode)
##       break()
##     }
##     nnvec <- nnvec + nb
##     if (max(nnvec * passive)==0){
##       cnode <- which(passive==1)[1]
##     } else {
##       cnode <- which.max(nnvec * passive)
##     }
##     vn[ans]
##   }

##   if (is.perfect){
##     names(ans)<-vn

##     if (index)
##       return(match(vn[ans],vn.orig))
##     else
##       return(vn.orig[match(vn[ans],vn.orig)])
##   } else {
##     return(character(0))
##   }
## }


