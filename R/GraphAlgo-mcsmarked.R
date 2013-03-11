######################################################################
##
## Maximum cardinality search for marked graph
##
## FIXME: Create methods for igrahs, graphNEL, matrix
##
######################################################################

mcsmarked <- function (object, discrete=NULL, index = FALSE){
  UseMethod("mcsmarked")
}

mcsmarked.graphNEL <- function (object, discrete=NULL, index = FALSE){
  if (is.null(discrete))
    mcsMAT(graphNEL2dgCMatrix(object), index=index)
  else
    mcsmarkedMAT(graphNEL2dgCMatrix(object), discrete=discrete, index = index)
}

mcsmarked.igraph <- function (object, discrete=NULL, index = FALSE){
  if (is.null(discrete))
    mcsMAT(get.adjacency(object), index=index)
  else
    mcsmarkedMAT(get.adjacency(object), discrete=discrete, index = index)
}

mcsmarked.matrix <- function (object, discrete=NULL, index = FALSE){
  if (is.null(discrete))
    mcsMAT(object, index=index)
  else
    mcsmarkedMAT(object, discrete=discrete, index = index)
}


mcsmarkedMAT <- function(amat, vn = colnames(amat), discrete = NULL, index = FALSE) {

  if (is.null(discrete)){
    return(mcsMAT(amat, vn=vn, index=index))
  }

  nv   <- length(vn)
  vn.ext <- c(".", vn)

  idx <- match(discrete, vn.ext)

  ## amat.ext <- matrix(0L, nrow=nv+1L, ncol=nv+1L)

  amat.ext <- as(Matrix(0, nrow=nv+1L, ncol=nv+1L), "dgCMatrix")

  
  amat.ext[2:(nv+1),2:(nv+1)] <- amat
  
  amat.ext[idx, 1L] <- 1L
  amat.ext[1L, idx] <- 1L
	  
  ans <- mcsMAT(amat.ext, vn=vn.ext, index=index)
  if (length(ans)>0)
    ans <- ans[-1L]
  
  ans
}




.mcsmarkedMAT <- function(amat, vn = colnames(amat), discrete = NULL, index = FALSE) {

  if (is.null(discrete)){
    return(mcsMAT(amat, vn=vn, index=index))
  }

  nv   <- length(vn)
  vn.ext <- c(vn, ".")

  idx <- match(discrete, vn.ext)
  if (any(is.na(idx))){
    stop("Not all variables are in the graph\n")
  }

  amat.ext <- matrix(0L, nrow=nv+1L, ncol=nv+1L)
  dimnames(amat.ext)  <- list(vn.ext, vn.ext)

  amat.ext[1:nv,1:nv] <- amat
  
  amat.ext[idx, nv+1L] <- 1L
  amat.ext[nv+1L, idx] <- 1L
	  
  ans <- mcsMAT(amat.ext, vn=vn.ext, root=".", index=index)
  if (length(ans)>0)
    ans <- ans[-1L]
  
  ans
}



















## mcsmarkedMAT <- function(amat, vn=colnames(amat), discrete=NULL, index=FALSE){


##   if (is.null(discrete)){
##     return(mcsMAT(amat, vn=vn, index=index))
##   }
  
##   whichPrim <- function(x){
##     seq_along(x)[x & !is.na(x)]
##   }

##   len.vn  <- length(vn)

##   if (is.null(discrete)){
##     disc.idx <- rep(1, len.vn)
##   } else {
##     if (is.numeric(discrete)){
##       disc.idx <- rep(0, len.vn)
##       disc.idx[discrete] <- 1
##     } else {
##       zzz <- match(discrete, vn)
##       if (any(is.na(zzz))){
##         stop("Not all variables are in the graph\n")
##       }
##       disc.idx <- rep(0, len.vn)
##       disc.idx[zzz] <- 1
##     }
##   }
##   LL  <- rep(0,length(vn))       ## Labelled nodes, L
##   UU  <- rep(1,length(vn))       ## Unlabelled nodes, U 
  
##   ans.idx    <- rep(NA, len.vn)
##   ans.name   <- rep(NA, len.vn)

##   curr.node.idx  <- whichPrim(disc.idx==1)[1]
##   curr.node.name <- vn[curr.node.idx]

##   node.wgt <- disc.idx

##   ##cat("disc.idx:\n"); print(disc.idx)
  
##   kk <- 1

##   repeat{
## ##     cat("------\n")
## ##     cat("curr.node.name:",curr.node.name, "curr.node.idx: ", curr.node.idx, "\n")
## ##     cat("node.wgt:\n"); print(node.wgt)   
##     LL[curr.node.idx] <- 1
##     UU[curr.node.idx] <- 0 

##     curr.nb.v         <- amat[curr.node.idx,]             ## ne(v)  
##     curr.n.nb.v       <- curr.nb.v * LL                   ## ne(v) \cap L
##     curr.un.nb.v      <- curr.nb.v * UU                   ## ne(v) \cap U
##     curr.n.nb.v.idx   <- whichPrim(curr.n.nb.v==1)          

## ##     cat("curr.n.nb.v:\n");     print(curr.n.nb.v)
## ##     cat("curr.un.nb.v:\n");    print(curr.un.nb.v)
## ##     cat("curr.n.nb.v.idx:\n"); print(curr.n.nb.v.idx)

##     is.comp    <- TRUE
##     is.disc <- TRUE
    
## ##    cat("Check if ne(v) cap L is complete \n")
##     len     <- length(curr.n.nb.v.idx)
##     if (len>1){
##       for (ii in 1:(len-1)) {
##         for(jj in (ii+1):(len)) {
##           if (amat[curr.n.nb.v.idx[ii],curr.n.nb.v.idx[jj]]==0){
##             is.comp <- FALSE
##             break()
##           }
##         }
##       }      
##     }

##     if (!is.comp){
##       break()
##     }
    
##     if (disc.idx[curr.node.idx]==1){
## ##      cat("Check if ne(v) cap L subset Delta if v in Delta\n")
##       if (length(curr.n.nb.v.idx)>0){
##         if(any(disc.idx[curr.n.nb.v.idx]==0)){
##           is.disc <- FALSE
##           break()
##         }
##       }
## ##      print(is.disc)      
##     }

##     if (!is.disc){
##       break()
##     }

##     ans.idx[kk]  <- curr.node.idx
##     ans.name[kk] <- curr.node.name

##     node.wgt <- node.wgt + curr.un.nb.v
##     zzz <- node.wgt * UU
## ##    cat("node.wgt (updated):\n"); print(node.wgt)
##     curr.node.idx  <- which.max(zzz)
##     curr.node.name <- vn[curr.node.idx]

##     if (kk==len.vn)
##       break()
##     kk <- kk + 1    
##   }  ## repeat

##   is.perfect <- is.comp & is.disc 

##   if (!is.perfect){
##     ret <- character(0)
##   } else {
##     names(ans.idx)<-vn
##     if (index){
##       ret <- ans.idx
##     } else {      
##       ret <- ans.name
##     }
##   } 
##   return(ret)
## }


