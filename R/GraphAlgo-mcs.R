##
## Maximum Cardinality Search, February, 2009
##
## Returns perfect ordering if it exists and character(0) otherwise
##
mcs <- function(object, root=NULL, index=FALSE){
  amat <- as.adjMAT(object)
  mcsMAT(amat, root=root, index=index)
}


mcsMAT <- function (amat, vn = colnames(amat), root = NULL, index = FALSE){

  #dyn.load("mcs")

  if (is.null(root)){
    root    <- vn
    rootNUM <- 0:(length(vn)-1)
  } else {
    root    <- c(root, setdiffPrim(vn, root))
    rootNUM <- charmatch(root, vn)-1L
  }
  ## cat("root    :"); print(root)
  ##   cat("rootNUM :"); print(rootNUM)
  ##   print(storage.mode(rootNUM))
  
  #ans<-.C("C_mcs", A=as.integer(amat), nc=ncol(amat), root=rootNUM, ans=integer(ncol(amat)))$ans
  ans<-.C("C_mcs", A=as.integer(amat), nc=ncol(amat), root=rootNUM, ans=integer(ncol(amat)),PACKAGE="gRbase")$ans


  ret <- if (ans[1]==-1){
    character(0)
  } else {
    if (index) {
      ans + 1
    } else {
      colnames(amat)[ans+1]
    }
  }
  
  #dyn.unload("mcs")
  return(ret)
} 


.mcsMAT <- function(amat, vn=colnames(amat), root=NULL, index=FALSE){
  
  is.perfect <- TRUE

  ## Make 'root' the variables to be searched first when finding elimination order.

  vn.orig <- vn
  if (!is.null(root)){ 
    vn2      <- c(root, setdiff(vn, root))
    neworder <- charmatch(vn2, vn)
    amat     <- amat[neworder,neworder]
    vn       <- vn2
  }

  #print(vn)
  cnode   <- 1
  active  <- nnvec <- rep(0,length(vn))
  passive <- rep(1,length(vn))
  ans     <- NULL
  
  amat <- amat*1
  
  for (kk in 1:length(vn)){
                                        #cat("cnode:", cnode, "\n")
    ans            <- c(ans, cnode)
    active[cnode]  <- 1
    passive[cnode] <- 0 
    nb             <- amat[cnode,]
    
    is.comp <- TRUE
    nbidx   <- which((nb*active)==1)
                                        #print(nbidx)
    len   <- length(nbidx)
    if (len>1){
      for (ii in 1:(len-1)) {
                                        #cat ("ii", ii, "vnii:", vn[nbidx[ii]], "\n")
        for(jj in (ii+1):(len)) {
                                        #cat ("  jj", jj, "vnjj:", vn[nbidx[jj]], "\n")
          if (amat[nbidx[ii],nbidx[jj]]==0){
            is.comp <- FALSE
            break()
          }
        }
      }
    }
    is.perfect <- is.comp
    
    if (!is.perfect){
      #cat("NOT perfect\n"); print(cnode)
      break()
    }
    nnvec <- nnvec + nb
    if (max(nnvec * passive)==0){
      cnode <- which(passive==1)[1]
    } else {
      cnode <- which.max(nnvec * passive)
    }
    vn[ans]
  }

  
  if (is.perfect){
    names(ans)<-vn

    if (index)
      return(charmatch(vn[ans],vn.orig))
    else
      return(vn.orig[charmatch(vn[ans],vn.orig)])
  } else {
    return(character(0))
  }
}

