## ###################################################################
##
## Maximum Cardinality Search (mcs)
## Maximum Cardinality Search for marked graphs (mcsmarked)
##
## Returns perfect ordering if it exists and character(0) otherwise
##
## ###################################################################

mcs <- function(object, root=NULL, index=FALSE){
  UseMethod("mcs")
}

mcs.default <- function(object, root=NULL, index=FALSE){
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))
    switch(cls,
           "graphNEL" ={
               if (is.UG(object)){
                   mcsMAT(graphNEL2dgCMatrix(object), root=root, index=index)
               } else {
                   character(0)
               }
           },
           "igraph"   ={
               if (!igraph::is.dag(object)){
                   mcsMAT(igraph::get.adjacency(object),root=root, index=index)
               } else {
                   character(0)
               }
           },
           "dgCMatrix"=,
           "matrix"   ={
               if (is.UG(object)){
                   mcsMAT(object, root=root, index=index)
               } else {
                   character(0)
               }
           })
}

mcsMAT <- function (amat, vn = colnames(amat), root = NULL, index = FALSE)
{
  vn.old <- vn
  if (!is.null(root)){
    vn   <- c(root, setdiffPrim(vn, root))
    root2 <- match(vn, vn.old)-1
  } else {
    root2 <- 0:(ncol(amat)-1)
  }
  #print(root2)

  a <- mcsMAT_( amat, root2 )
  if (index){
    if (a[1]<0){
      NA
    } else {
      a+1
    }
  } else {
    if (a[1]<0){
      character(0)
    } else {
      vn.old[a+1]
    }
  }
}



mcsmarked <- function (object, discrete=NULL, index = FALSE){
  UseMethod("mcsmarked")
}

mcsmarked.default <- function (object, discrete=NULL, index = FALSE){
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))
    switch(cls,
           "graphNEL" ={
               if (is.null(discrete))
                   mcsMAT(graphNEL2dgCMatrix(object), index=index)
               else
                   mcsmarkedMAT(graphNEL2dgCMatrix(object), discrete=discrete, index = index)
           },
           "igraph"   ={
               if (is.null(discrete))
                   mcsMAT(igraph::get.adjacency(object), index=index)
               else
                   mcsmarkedMAT(igraph::get.adjacency(object), discrete=discrete, index = index)
           },
           "dgCMatrix"=,
           "matrix"   ={
               if (is.null(discrete))
                   mcsMAT(object, index=index)
               else
                   mcsmarkedMAT(object, discrete=discrete, index = index)
           })
}



## FIXME: mcsmarkedMAT: candidate for C++ implementation.
mcsmarkedMAT <- function(amat, vn = colnames(amat), discrete = NULL, index = FALSE) {

  if (is.null(discrete)){
    return(mcsMAT(amat, vn=vn, index=index))
  }

  nv   <- length(vn)
  vn.ext <- c(".", vn)
  idx <- match(discrete, vn.ext)

  amat.ext <- as(Matrix(0, nrow=nv+1L, ncol=nv+1L), "dgCMatrix")
  amat.ext[2:(nv+1),2:(nv+1)] <- amat
  amat.ext[idx, 1L] <- 1L
  amat.ext[1L, idx] <- 1L

  ans <- mcsMAT(amat.ext, vn=vn.ext, index=index)
  if (length(ans)>0)
    ans <- ans[-1L]

  ans
}






## mcsmarked.graphNEL <- function (object, discrete=NULL, index = FALSE){
##   if (is.null(discrete))
##     mcsMAT(graphNEL2dgCMatrix(object), index=index)
##   else
##     mcsmarkedMAT(graphNEL2dgCMatrix(object), discrete=discrete, index = index)
## }

## mcsmarked.igraph <- function (object, discrete=NULL, index = FALSE){
##   if (is.null(discrete))
##     mcsMAT(igraph::get.adjacency(object), index=index)
##   else
##     mcsmarkedMAT(igraph::get.adjacency(object), discrete=discrete, index = index)
## }

## mcsmarked.matrix <- function (object, discrete=NULL, index = FALSE){
##   if (is.null(discrete))
##     mcsMAT(object, index=index)
##   else
##     mcsmarkedMAT(object, discrete=discrete, index = index)
## }


## mcs.graphNEL <- function(object, root=NULL, index=FALSE){
##   if (is.UG(object)){
##     mcsMAT(graphNEL2dgCMatrix(object), root=root, index=index)
##   } else {
##     character(0)
##   }
## }

## mcs.igraph <- function(object, root=NULL, index=FALSE){
##   if (!igraph::is.dag(object)){
##     mcsMAT(igraph::get.adjacency(object),root=root, index=index)
##   } else {
##     character(0)
##   }
## }

## mcs.matrix <- mcs.Matrix <- function(object, root=NULL, index=FALSE){
##   if (is.UG(object)){
##     mcsMAT(object, root=root, index=index)
##   } else {
##     character(0)
##   }
## }



## ############################################################
##
## OBSOLETE BELOW HERE
##
## ############################################################

## mcsMAT <- function (amat, vn = colnames(amat), root = NULL, index = FALSE)
## {
##   vn.old <- vn
##   if (!is.null(root)){
##     vn   <- c(root, setdiffPrim(vn, root))
##     root2 <- match(vn, vn.old)-1
##   } else {
##     root2 <- 0:(ncol(amat)-1)
##   }
##   #print(root2)

##   if (class(amat)=="matrix"){
##     a <- mcsMAT_stCpp(amat, root2)
##   } else {
##     a <- mcsMAT_spCpp(amat, root2)
##   }

##   #print(a)
##   if (index){
##     if (a[1]<0){
##       NA
##     } else {
##       a+1
##     }
##   } else {
##     if (a[1]<0){
##       character(0)
##     } else {
##       vn.old[a+1]
##     }
##   }
## }



## ############################################################
##
## OLDER IMPLEMENTATIONS
##
## ############################################################


## Old version, replaced by faster version in January 2013
## .mcsMAT <- function (amat, vn = colnames(amat), root = NULL, index = FALSE){
##   cls <- class(amat)
##   if (cls =="dgCMatrix"){
##     ans <- .mcsMAT_spR(amat, vn=vn, root=root, index=index)
##   } else {
##     ans <- .mcsMAT_stR(amat, vn=vn, root=root, index=index)
##   }
##   ans
## }

## .mcsMAT_stC <- function (amat, vn = colnames(amat), root = NULL, index = FALSE){
##   if (is.null(root)){
##     root    <- vn
##     rootNUM <- 0:(length(vn)-1)
##   } else {
##     root    <- c(root, setdiffPrim(vn, root))
##     rootNUM <- match(root, vn)-1L
##   }

##   ans<-.C("C_mcs", A=as.integer(amat), nc=ncol(amat), root=rootNUM,
##           ans=integer(ncol(amat))
##           ,PACKAGE="gRbase"
##           )$ans
##   ret <- if (ans[1]==-1){
##     character(0)
##   } else {
##     if (index) {
##       ans + 1
##     } else {
##       colnames(amat)[ans+1]
##     }
##   }
##   return(ret)
## }

## ###
## ### R version for sparse matrices
## ###
## .mcsMAT_spR <- function(amat, vn=colnames(amat), root=NULL, index=FALSE){

##   is.perfect <- TRUE
##   if (is.null(vn))
##     vn <- 1:ncol(amat)

##   ## Make 'root' the variables to be searched first when finding elimination order.
##   vn.orig <- vn
##   if (!is.null(root)){
##     vn2      <- c(root, setdiffPrim(vn, root))
##     neworder <- match(vn2, vn)
##     amat     <- amat[neworder,neworder]
##     vn       <- vn2
##   }

##   nvar    <- length(vn)
##   active  <- nnvec <- ans <- rep.int(0L, nvar)
##   passive <- rep.int(1L, nvar)

##   cnode   <- 1
##   for (kk in 1:nvar){
##     ans[kk]        <- cnode #;cat("cnode:", cnode, "\n")
##     active[cnode]  <- 1L
##     passive[cnode] <- 0L
##     nb             <- sp_getXi(amat, cnode)
##     nbidx          <- which((nb*active)==1)
##     len            <- length(nbidx)
##     if (kk %% 1000==0)
##       cat(sprintf("mcs: kk=%5i cnode=%6s, length(nbidx)=%s\n", kk, cnode, length(nbidx)))
##     if (len>1){
##       tf <- names2pairs(nbidx, sort=FALSE, result="matrix")
##       vv <- sp_getXtf(amat, tf)
##       if (any(vv==0)){
##         is.perfect <-  FALSE
##         break()
##       }
##     }

##     nnvec <- nnvec + nb
##     if (max(nnvec * passive)==0){
##       cnode <- which(passive==1)[1]
##     } else {
##       cnode <- which.max(nnvec * passive)
##     }
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


## ### mcs for adjacency matrices
## ###
## ### September 2012; reimplementation
## ### There seems to be a bug in the C-code so for now this version is used.
## ###
## .mcsMAT_stR <- function(amat, vn=colnames(amat), root=NULL, index=FALSE){

##   .whichPrim <- function(x){seq_along(x)[x]}

##   is.perfect <- TRUE
##   vn.orig <- vn
##   if (!is.null(root)){
##     vn <- c(root, setdiff(vn, root))
##     rootNUM <- match(vn, colnames(amat))
##     use.root <- TRUE
##   } else {
##     rootNUM <- seq_along(vn)
##     use.root <- FALSE
##   }

##   res <- act <- rep(0L, dim(amat)[1L])
##   nidx      <- rootNUM[1L] ## node index

##   act[nidx] <- 1L ## first node marked as 'passive'
##   res[1]    <- nidx ## number of first node
##   nnodes    <- length(res)

##   ## iterate
##   if (nnodes>1){
##     for (ii in 2:nnodes){

##       ## Some saving can be made here as we do not need to consider
##       ## the rows of amat corresponding to nodes that are already numbered
##       nn <- amat %*% act ## numbered neighbours
##       nn[act>0] <- -1L

##       if (use.root){
##         nidx.cand <- .whichPrim(nn==max(nn)) ## candidates to be numbered
##         if (any(rootNUM[ii]==nidx.cand)){
##           nidx <- rootNUM[ii]
##         } else {
##           nidx <- nidx.cand[1]
##           use.root <- FALSE
##         }
##       } else {
##         nidx <- which.max(nn)
##       }

##       bd <- as.numeric(amat[nidx,] * act)

##       bd.idx <- .whichPrim(bd>0)
##       n.bd   <- length(bd.idx)
##       ## check that subgraph defined by bd.idx is complete
##       ## if not then exit
##       if(sum(amat[bd.idx,bd.idx]) != n.bd*(n.bd-1L)){
##         is.perfect <- FALSE
##         break
##       }

##       act[nidx] <- 1L
##       res[ii] <- nidx
##     }

##   }
##     if (is.perfect){
##       if (index)
##         res
##       else
##         vn.orig[res]
##     } else {
##       character(0)
##     }

## }



## ##
## ## New implementation; somewhat more elegant but not faster
## ##
## .mcsMAT2 <- function (amat, vn = colnames(amat), root = NULL, index = FALSE){

##   .wmo <- function(x,o=seq_along(x)){o[which.max(x[o])]}
##   #.wmo <- function(x,o=seq_along(x)){which.max(x)}

##   NN  <- nrow(amat)

##   if (!is.null(root)){
##     zz <- c(root, setdiffPrim(vn, root))
##     oo <- match(zz, colnames(amat))
##   } else {
##     oo <- seq_along(vn)
##   }

##   pp  <- rep(0L,  NN)   ## passive nodes
##   aa  <- rep(1L,  NN)   ## active nodes
##   res <- rep(-1L, NN)   ## result
##   ok  <- TRUE
##   ss  <- rep.int(0L, NN)
##   idxvec <- 1:NN

##   ii     <- oo[1]
##   res[1] <- ii
##   pp[ii] <- 1L
##   aa[ii] <- 0L

##   if (NN>1){
##     for(kk in 2:NN){
##       ss <- as.numeric(amat %*% pp)
##       ii <- .wmo(ss*aa, oo)
##       rr <- amat[ii,] * pp ## indicator of passive neighbours
##       bb <- idxvec[rr!=0]
##       n.nb <- length(bb) ## number of passive neighbours

##       if (n.nb>1){
##         ## For sparse matrices:
##         ##       tf <- names2pairs(bb, sort=FALSE, result="matrix")
##         ##       vv <- amat[tf]
##         ##       if (any(vv==0)){
##         ##         ok <- FALSE
##         ##         break
##         ##       }

##         ## For standard matrices:
##         if(sum(amat[bb, bb]) != (n.nb-1)*n.nb){
##           ok <- FALSE
##           break
##         }
##       }
##       res[kk] <- ii
##       pp[ii] <- 1L
##       aa[ii] <- 0L
##     }
##   }

##   if (ok){
##     if (index)
##       res
##     else
##       vn[res]
##   } else {
##     character(0)
##   }
## }



































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




## .mcsmarkedMAT <- function(amat, vn = colnames(amat), discrete = NULL, index = FALSE) {

##   if (is.null(discrete)){
##     return(mcsMAT(amat, vn=vn, index=index))
##   }

##   nv   <- length(vn)
##   vn.ext <- c(vn, ".")

##   idx <- match(discrete, vn.ext)
##   if (any(is.na(idx))){
##     stop("Not all variables are in the graph\n")
##   }

##   amat.ext <- matrix(0L, nrow=nv+1L, ncol=nv+1L)
##   dimnames(amat.ext)  <- list(vn.ext, vn.ext)

##   amat.ext[1:nv,1:nv] <- amat

##   amat.ext[idx, nv+1L] <- 1L
##   amat.ext[nv+1L, idx] <- 1L

##   ans <- mcsMAT(amat.ext, vn=vn.ext, root=".", index=index)
##   if (length(ans)>0)
##     ans <- ans[-1L]

##   ans
## }



















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





