##################################################################
####
#### Create undirected graphs or DAGs from graph specification
####
##################################################################

####################
## Undirected graphs
####################

ug <- function(...,result="NEL"){
  ugList(list(...), result=result)
}

ugList <- function(x, result="NEL"){
    result <- match.arg(result, c("matrix","Matrix","dgCMatrix","igraph","NEL","graphNEL"))
    x   <- unlist(lapply(x, function(g) rhsf2list(g)), recursive=FALSE)
    vn  <- unique(unlist(x))

    switch(result,
           "NEL"=,"graphNEL"={.ugList_NEL(x,vn)},
           "Matrix"=,"dgCMatrix"={ugList2dgCMatrix(x, vn)},
           "matrix"={ugList2matrix(x, vn)},
           "igraph"={
               gg <- igraph::igraph.from.graphNEL(.ugList_NEL(x, vn))
               igraph::V(gg)$label <- igraph::V(gg)$name
               gg
           })
}

.ugList_NEL<- function(gset, vn){
    zzz    <- lapply(gset, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
    ftM    <- do.call(rbind, zzz)
    if (nrow(ftM)>0){
        tofrom <- unique(rowmat2list(ftM))
        fff    <- do.call(rbind, tofrom)
        graph::ftM2graphNEL(fff, V=as.character(vn),
                            edgemode="undirected")
    } else {
        new("graphNEL", nodes=as.character(vn),  edgemode="undirected")
    }
}

###########################
## Directed acyclic graphs
###########################

dag <- function(...,result="NEL", forceCheck=FALSE){
  dagList(list(...), result=result, forceCheck=forceCheck)
}

## dagList: forceCheck not implemented
dagList <- function(x, result="NEL", forceCheck=FALSE){
    result <- match.arg(result, c("matrix","Matrix","dgCMatrix","igraph","NEL","graphNEL"))
    x   <- unlist(lapply(x, function(g) rhsf2list(g)), recursive=FALSE)
    vn  <- unique(unlist(x))

    out <- switch(result,
                  "NEL"=,"graphNEL"={.dagList_NEL(x, vn)},
                  "Matrix"=,"dgCMatrix"={dagList2dgCMatrix(x, vn)},
                  "matrix"={dagList2matrix(x, vn)},
                  "igraph"={
                      gg <- igraph::igraph.from.graphNEL(.dagList_NEL(x, vn))
                      igraph::V(gg)$label <- igraph::V(gg)$name
                      gg
                  })
    if (forceCheck){
        if( length( topoSort( out )) == 0){
            stop("In dag/dagList: Graph is not a DAG", call.=FALSE)
        }
    }
    out
}

.dagList_NEL<- function(gset, vn){
    zzz <- lapply(gset, function(xx) names2pairs(xx[1],xx[-1],
                                                    sort=FALSE, result="matrix"))
    ftM <- do.call(rbind, zzz)
    if (nrow(ftM)>0){
        tfL <- unique(rowmat2list(ftM))
        ftM <- do.call(rbind,tfL)[,2:1,drop=FALSE]
        graph::ftM2graphNEL(ftM, V=as.character(vn),
                            edgemode="directed")
    } else {
        new("graphNEL", nodes=as.character(vn), edgemode="directed")
    }

}



## dag <- function(...,result="NEL", forceCheck=FALSE){
##   result <- match.arg(result, c("NEL","matrix","Matrix","igraph"))
##   dagList(list(...), result=result)
## }

## dagList <- function(x, result="NEL", forceCheck=FALSE){
##   result <- match.arg(result, c("NEL","matrix","Matrix","igraph"))
##   if (result=="igraph"){
##     zz <- .dagList_internal(x, result="NEL", forceCheck=forceCheck)
##     if (!is.null(zz)){
##       gg <- igraph::igraph.from.graphNEL(zz)
##       igraph::V(gg)$label <- igraph::V(gg)$name
##       gg
##     } else {
##       NULL
##     }
##   } else {
##     .dagList_internal(x, result=result, forceCheck=forceCheck)
##   }
## }


## .dagList_internal<- function(x, result="NEL", forceCheck=FALSE){

##   #cat(sprintf("result=%s\n", result))
##   isForm <- sapply(x, inherits, "formula")
##   flist  <- x[isForm]
##   gset   <- lapply(flist, function(f) rhsf2list(f))
##   gset   <- unlist(gset, recursive=FALSE)
##   ## vpaL: (v,pa(v)) list; not necessarily minimal or with unique elements;
##   vpaL   <- c(gset, x[!isForm])
##   V      <- uniquePrim(unlist(vpaL))

##   if (forceCheck){
##     if (length(topoSort(vpaL))==0){
##       stop("Vertices can not be sorted topologically; not a DAG")
##       return(NULL)
##     }
##   }

##   if (result=="NEL"){
##     uuu    <- lapply(vpaL, function(xx) names2pairs(xx[1],xx[-1],
##                                                     sort=FALSE, result="matrix"))
##     uuu    <- do.call(rbind, uuu)
##     if (nrow(uuu)>0){
##       tfL    <- unique(rowmat2list(uuu)) # to-from-list
##       ftM    <- do.call(rbind,tfL)[,2:1,drop=FALSE]
##       value  <- graph::ftM2graphNEL(ftM, V=as.character(V), edgemode="directed")
##     } else {
##       value  <-   new("graphNEL", nodes=as.character(V), edgemode="directed")
##     }
##   } else {
##     value <- vpaList2adjMAT(vpaL, vn=V, result=result)
##   }
##   value
## }


## NEW




## .ugList_NEL<- function(gset, vn){
##     zzz    <- lapply(gset, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
##     uuu    <- do.call(rbind, zzz)
##     if (nrow(uuu)>0)
##         tofrom <- unique(rowmat2list(uuu))
##     else
##         tofrom <- NULL
##     if (length(tofrom)==0){
##         value <-   new("graphNEL", nodes=as.character(vn), edgemode="undirected")
##     } else {
##         fff <- do.call(rbind,tofrom)
##         value <- graph::ftM2graphNEL(fff, V=as.character(vn), edgemode="undirected")
##     }
##     value
## }

## .ugList <- function(x, result="NEL"){
##   result <- match.arg(result, c("NEL","matrix","Matrix","igraph"))
##   if (result=="igraph"){
##     gg <- igraph.from.graphNEL(.ugList_internal(x, result="NEL"))
##     V(gg)$label <- V(gg)$name
##     gg
##   } else {
##     .ugList_internal(x, result=result)
##   }
## }

## .ugList_internal<- function(x, result="NEL"){
##   isForm <-sapply(x, inherits, "formula")
##   flist  <- x[isForm]
##   gset   <- lapply(flist, function(f) rhsf2list(f))
##   gset   <- unlist(gset, recursive=FALSE)

##   gset   <- c(gset, x[!isForm])
##   V      <- uniquePrim(unlist(gset))

##   if (result=="NEL"){
##     zzz    <- lapply(gset, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
##     uuu    <- do.call(rbind, zzz)
##     if (nrow(uuu)>0)
##       tofrom <- unique(rowmat2list(uuu))
##     else
##       tofrom <- NULL
##     if (length(tofrom)==0){
##       value <-   new("graphNEL", nodes=as.character(V), edgemode="undirected")
##     } else {
##       fff <- do.call(rbind,tofrom)
##       value <- ftM2graphNEL(fff, V=as.character(V), edgemode="undirected")
##     }
##   } else {
##     value <- glist2adjMAT(gset, vn=V, result=result)
##   }

##   value
## }
