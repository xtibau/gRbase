triangulateMAT <- function(amat, nLevels=rep(2, ncol(amat)), result=NULL, ...){

  if (is.null(nLevels))
    nLevels=rep(2, ncol(amat))
  cls1 <- .matClass( amat ) ##; cat(sprintf("cls1=%s\n", toString(cls1)))
  
  if (cls1==0){
    stop("Argumuent 'amat' must be a matrix\n")
  } else if (cls1==1){
    ans <- triangulateMAT_stCpp( amat, nLevels )
  } else if (cls1==2) {
    ans <- triangulateMAT_spCpp( as( amat, "dgCMatrix"), nLevels )
  } else {
    ans <- triangulateMAT_spCpp( amat, nLevels )
  }

  if ( !is.null(result) ){
    result <- match.arg(result, c("matrix","Matrix", "graphNEL","igraph"))
    if (result=="Matrix") result <- "dgCMatrix"
    as(ans, result)
  } else {
    ans
  }  
}

.matClass <- function(amat){
  max( which(inherits(amat, c("matrix","Matrix","dgCMatrix"), which=TRUE) > 0 ))
}

triangulate.graphNEL <- function(object, 
                                 nLevels=NULL, ##nLevels=rep(2,length(nodes(object))),
                                 result="graphNEL",...){
  triangulateMAT( graphNEL2dgCMatrix(object), nLevels=nLevels, result=result, ... ) 
}

triangulate.igraph <- function(object, 
                               nLevels=NULL, ##nLevels=rep(2,length(V(object))),
                               result="igraph",...){
  triangulateMAT( get.adjacency(object), nLevels=nLevels, result=result, ...) 
}

triangulate.matrix <- function(object, 
                               nLevels=NULL, ##nLevels=rep(2,ncol(object)),
                               result="matrix", ...){
  triangulateMAT( object, nLevels=nLevels, result=result, ... )
}
 
triangulate.Matrix <- function(object, 
                               nLevels=NULL, ##rep(2,ncol(object)),
                               result="Matrix", ...){
  triangulateMAT( object, nLevels=nLevels, result=result, ...)
}
















## .triangulateMAT_stC<- function(amat, method="mcwh", nLevels=rep(2,ncol(amat))){
##   nc  <-  ncol(amat)
##   vn  <-  colnames(amat)
##   i   <- .C("triangmcwh", Av=as.integer(amat), nc, vn,
##             as.integer(nLevels), ans=integer(1), PACKAGE="gRbase")$Av
##   ans           <- matrix(i, ncol=nc, nrow=nc)           
##   dimnames(ans) <- dimnames(amat)
##   ans           <- abs(ans)   ## FIXME: Do this in C
##   diag(ans)     <- 0L         ## FIXME: Do this in C  
##   ans 
## }


## .triangulateMAT_spR<- function(amat, vn=colnames(amat), nLevels=rep(2, length(vn))){

##   if (is.null(vn))
##     vn <- 1:ncol(amat)
  
##   nvar    <- length(vn)
##   lognlev <- log(nLevels)
##   active  <- rep.int(1L, nvar)
##   goon    <- 0L

##   #cat(sprintf("initializing...\n"))
##   .cqsize <- function(X,ii,lognlev){sum(lognlev[c(ii,which(sp_getXi(X,ii)>0))])}
##   cqsize <- unlist(lapply(1:nvar, function(ii) .cqsize(amat,ii,lognlev)))
##   #cat(sprintf("done\n"))
        
##   while(goon<nvar){
##     goon <- goon + 1L
##     vii  <- which.min(cqsize)
##     ne   <- (active==1) * sp_getXi(amat, vii) > 0
##     nne  <- sum(ne)
##     if (goon %% 1000 == 0)
##       cat(sprintf("triangulate: node number=%6i node=%7s nne=%5i\n",  goon, vn[vii], nne))
##     if (nne > 1){
##       neii    <- which(ne)
##       tf      <- names2pairs(neii, sort=FALSE, result="matrix")        
##       vv      <- sp_getXtf(amat, tf)
##       nedges  <- sum(vv)
##       nfillin <- ((nne-1)*nne / 2) - nedges
##       if (nfillin>0){
##         amat <- sp_setXM1(amat, rbind(tf, tf[,2:1,drop=FALSE]))
##         zz   <- unlist(lapply(neii, function(ii) .cqsize(amat,ii,lognlev)))
##         cqsize[neii] <- zz
##       }
##     }
##     active[vii] <- 0L
##     cqsize[vii] <- 10000000L
##   }
##   dimnames(amat) <- list(vn,vn)
##   amat  
## }




## triangulateMAT <- function(amat, method="mcwh",
##                            nLevels=rep(2,ncol(amat)), result=NULL, ...){

##   trimethod <- c("mcwh","r")                      ## FIXME: Not used
##   method <- match.arg(tolower(method),trimethod)  ## FIXME: Not used
  
##   cls <- class(amat)
##   if (is.null(result))
##     result <- cls
##   else {
##     result <- match.arg(result, c("matrix","graphNEL","igraph", "Matrix"))
##   }
  
##   if (cls=="dgCMatrix"){
##     ans <- triangulateMAT_spCpp(amat, nLevels)
##   } else {
##     ans <- triangulateMAT_stCpp(amat, nLevels)
##   }
##   as(ans, result)
## }
