##
## vpar implemented for graphNEL, matrix and Matrix
##

vpar <- function(object, getv=TRUE, forceCheck=TRUE){
  UseMethod("vpar")
}

vparMAT <- function(object, getv=TRUE, forceCheck=TRUE){
  if (forceCheck && !is.adjMAT(object))
    stop("Matrix is not adjacency matrix... \n")
  if(forceCheck && isSymmetric(object))
    stop("Graph is undirected; (v,pa(v)) does not exist...\n")

##   mywhich <- function(x, idx=seq_along(x)){
##     idx[x]
##   }

  vn <- rownames(object)
  ##idx <- seq_along(vn)
  ans <- vector("list", length(vn))
  if (getv){
    for (jj in seq_along(vn)) {
      ans[[jj]] <- vn[c(jj, which(object[, jj]!=0))]
    }
  } else {
    for (jj in seq_along(vn)) {
      ans[[jj]] <- vn[object[, jj] != 0]
    }
  }	
  names(ans) <- vn
  ans
}

vpar.graphNEL <- function(object, getv=TRUE, forceCheck=TRUE){
  if (forceCheck && edgemode(object)=="undirected")
    stop("Graph is undirected; (v,pa(v)) does not exist...\n")

  ee <- graph::edges(object)
  vn <- names(ee)
  tf <- do.call(rbind, # matrix in to-from form
                lapply(1:length(ee),
                       function(ii) names2pairs( ee[[ii]], vn[ii],
                                                sort=FALSE, result="matrix")))
  
  ans <- lapply(1:length(vn), function(ii) c(vn[ii], tf[tf[,1]==vn[ii],2]))
  names(ans) <- vn
  if (!getv)
    ans<-lapply(ans, function(x)x[-1])
  return(ans)
}

vpar.Matrix <- vpar.matrix <- vparMAT





























## .vpar <- function(object, forceCheck=TRUE){

##   type <- which(inherits(object, c("graphNEL","matrix"), which=TRUE)>0)
##   switch(type,
##          "1"={
##            ##cat("graphNEL\n")
##            if (forceCheck && edgemode(object)=="undirected")
##              stop("Graph is undirected; (v,pa(v)) does not exist...\n")
##            ee <- graph::edges(object)
##            vn <- names(ee)
##            tf <- do.call(rbind, # matrix in to-from form
##                          lapply(1:length(ee),
##                                 function(ii) names2pairs( ee[[ii]], vn[ii],
##                                                          sort=FALSE, result="matrix")))
           
##            ans <- lapply(1:length(vn), function(ii) c(vn[ii], tf[tf[,1]==vn[ii],2]))
##            ## FIXME: Probably faster to apply topoSort to ans rather than
##            ## checking if the graph is undirected...
##            names(ans) <- vn
##            return(ans)
##          },
##          "2"={
##            ##cat("matrix\n")
##            if(forceCheck && sum(abs(object-t(object))>100 * .Machine$double.eps)==0)
##              stop("Graph is undirected; (v,pa(v)) does not exist...\n")

##            vn   <- rownames(object) 
##            ans  <- vector("list", length(vn))
##            for (ii in seq_along(vn)){
##              ans[[ii]] <- c(vn[ii], vn[object[,ii]>0])
##            }
##            names(ans) <- vn
##            return(ans)
##          },
##          {stop("function 'vpar()' only implemented for graphNELs and adjacency matrices")
##         }
##        )
## }



















## vadj
## On undirected graph: same as adj
## On DAG: same as (v, pa(v))

## vadj <- function(object, getv=TRUE){
##   UseMethod("vadj")
## }

## vadj.matrix <- function(object, getv=TRUE){
##   vn <- rownames(object)
##   ans <- vector("list", length(vn))
##   if (getv){
##     for (jj in seq_along(vn)) {
##       ans[[jj]] <- c(vn[jj], vn[object[, jj] > 0])
##     }
##   } else {
##     for (jj in seq_along(vn)) {
##       ans[[jj]] <- vn[object[, jj] > 0]
##     }
##   }	
##   names(ans) <- vn
##   ans
## }

## vadj.graphNEL <- function(object, getv=TRUE){
##   if (getv){
##     ee<-graph::edges(object)
##     nn<-graph::nodes(object)
##     mapply(function(n,e)c(n,e), nn, ee)
##   } else {
##     graph::edges(object)
##   }
## }



