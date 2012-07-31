##################################################################
####
#### Create undirected graphs or DAGs from graph specification
####
##################################################################

####################
## Undirected graphs
####################

ug <- function(...,result="NEL"){
  result <- match.arg(result, c("NEL","matrix","Matrix", "igraph"))
  ugList(list(...), result=result)
}

ugList <- function(x, result="NEL"){
  result <- match.arg(result, c("NEL","matrix","Matrix","igraph"))
  if (result=="igraph"){
    gg <- igraph.from.graphNEL(.ugList_internal(x, result="NEL"))
    V(gg)$label <- V(gg)$name
    gg
  } else {
    .ugList_internal(x, result=result)
  }
}

.ugList_internal<- function(x, result="NEL"){
  isForm <-sapply(x, inherits, "formula")
  flist  <- x[isForm]
  gset   <- lapply(flist, function(f) rhsf2list(f))
  gset   <- unlist(gset, recursive=FALSE)
	
  gset   <- c(gset, x[!isForm])
  V      <- uniquePrim(unlist(gset))

  if (result=="NEL"){
    zzz    <- lapply(gset, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
    uuu    <- do.call(rbind, zzz)           
    if (nrow(uuu)>0)
      tofrom <- unique(rowmat2list(uuu))
    else
      tofrom <- NULL           
    if (length(tofrom)==0){
      value <-   new("graphNEL", nodes=as.character(V), edgemode="undirected")
    } else {
      fff <- do.call(rbind,tofrom)
      value <- ftM2graphNEL(fff, V=as.character(V), edgemode="undirected")
    }
  } else {
    value <- glist2adjMAT(gset, vn=V, result=result)
  }

  value
}

###########################
## Directed acyclic graphs
###########################

dag <- function(...,result="NEL", forceCheck=FALSE){
  result <- match.arg(result, c("NEL","matrix","igraph"))
  dagList(list(...), result=result)
}
  
dagList <- function(x, result="NEL", forceCheck=FALSE){
  result <- match.arg(result, c("NEL","matrix","Matrix","igraph"))
  if (result=="igraph"){
    zz <- .dagList_internal(x, result="NEL", forceCheck=forceCheck)
    if (!is.null(zz)){
      gg <- igraph.from.graphNEL(zz)
      V(gg)$label <- V(gg)$name
      gg
    } else {
      NULL
    }
  } else {
    .dagList_internal(x, result=result, forceCheck=forceCheck)
  }
}
         
.dagList_internal<- function(x, result="NEL", forceCheck=FALSE){

  #cat(sprintf("result=%s\n", result))
  isForm <- sapply(x, inherits, "formula")
  flist  <- x[isForm]
  gset   <- lapply(flist, function(f) rhsf2list(f))
  gset   <- unlist(gset, recursive=FALSE)
  ## vpaL: (v,pa(v)) list; not necessarily minimal or with unique elements;
  vpaL   <- c(gset, x[!isForm]) 
  V      <- uniquePrim(unlist(vpaL))
  
  if (forceCheck){
    if (length(topoSort(vpaL))==0){
      stop("Vertices can not be sorted topologically; not a DAG")
      return(NULL)
    }
  }
  
  if (result=="NEL"){
    uuu    <- lapply(vpaL, function(xx) names2pairs(xx[1],xx[-1],
                                                    sort=FALSE, result="matrix"))
    uuu    <- do.call(rbind, uuu)
    if (nrow(uuu)>0){
      tfL    <- unique(rowmat2list(uuu)) # to-from-list
      ftM    <- do.call(rbind,tfL)[,2:1,drop=FALSE]
      value  <- ftM2graphNEL(ftM, V=as.character(V), edgemode="directed")
    } else {
      value  <-   new("graphNEL", nodes=as.character(V), edgemode="directed")
    }
  } else {
    value <- vpaL2adjMAT(vpaL, vn=V, result=result)
  }
  value
}
  
  
isAcyclicMAT <- function(amat){
  is.acyc <- TRUE
  elorder <- NULL
  active  <- rep(TRUE, nrow(amat))
  
  for (ii in 1:nrow(amat)){
    idx <- rowSums(amat[,active,drop=FALSE])==0
    if (sum(idx)==0){
      is.acyc <- FALSE
      break()
    }
    active[idx] <- FALSE
  }
  is.acyc
}


isUndirectedMAT <- function(amat){
  isSymmetric.matrix(amat)
}



