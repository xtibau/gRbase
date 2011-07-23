##################################################################
####
#### Create undirected graphs or DAGs from graph specification
####
##################################################################

####################
## Undirected graphs
####################

ug <- function(...,result="NEL"){
  result <- match.arg(result, c("NEL","matrix","igraph"))
  ugList(list(...), result=result)
}

ugList <- function(x, result="NEL"){
  result <- match.arg(result, c("NEL","matrix","igraph"))
  switch(result,
         "NEL"   ={.ugList_internal(x, result="NEL")},
         "matrix"={.ugList_internal(x, result="matrix")},
         "igraph"={gg <- igraph.from.graphNEL(.ugList_internal(x, result="NEL"))
                   V(gg)$label <- V(gg)$name
                   gg
                 })
}

.ugList_internal<- function(x, result="NEL"){
  isForm <-sapply(x, inherits, "formula")
  flist  <- x[isForm]
  gset   <- lapply(flist, function(f) rhsf2list(f))
  gset   <- unlist(gset, recursive=FALSE)
	
  gset   <- c(gset, x[!isForm])
  V      <- uniquePrim(unlist(gset))


  switch(result,
         "NEL"={
           zzz    <- lapply(gset, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
           uuu    <- do.call(rbind, zzz)           
           if (nrow(uuu)>0)
             tofrom <- unique(rowmat2list(uuu))
           else
             tofrom <- NULL           
           if (length(tofrom)==0){
             value <-   new("graphNEL", nodes=V, edgemode="undirected")
           } else {
             fff <- do.call(rbind,tofrom)
             value <- ftM2graphNEL(fff, V=V, edgemode="undirected")
           }},
         "matrix"={
           value <- glist2adjMAT(gset, vn=V)
         })

  value

         
##   if (result=="matrix")
##     as.adjMAT(value)
##   else
##     value
}


###########################
## Directed acyclic graphs
###########################

dag <- function(...,result="NEL"){
  result <- match.arg(result, c("NEL","matrix","igraph"))
  dagList(list(...), result=result)
}

dagList <- function(x, result="NEL"){
  result <- match.arg(result, c("NEL","matrix","igraph"))
  switch(result,
         "NEL"   ={.dagList_internal(x, "NEL")},
         "matrix"={.dagList_internal(x, "matrix")},
         "igraph"={
           zz <- .dagList_internal(x)
           if (!is.null(zz)){
             gg <- igraph.from.graphNEL(zz)
             V(gg)$label <- V(gg)$name
             gg
           } else {
             NULL
           }
         })
}
       
.dagList_internal<- function(x, result="NEL"){

  isForm <- sapply(x, inherits, "formula")
  flist  <- x[isForm]
  gset   <- lapply(flist, function(f) rhsf2list(f))
  gset   <- unlist(gset, recursive=FALSE)
  
  gset   <- c(gset, x[!isForm])
  V      <- unique(unlist(gset))
  
  zzz    <- lapply(gset, function(xx) names2pairs(xx[1],xx[-1],
                                                  sort=FALSE, result="matrix"))
  uuu    <- do.call(rbind, zzz)
  if (nrow(uuu)>0)
    tofrom <- unique(rowmat2list(uuu))
  else
    tofrom <- NULL
  
  if (length(tofrom)==0){
    value <-   new("graphNEL", nodes=V, edgemode="directed")
  } else {
    fff <- do.call(rbind,tofrom)[,2:1,drop=FALSE]
    value <- ftM2graphNEL(fff, V=V, edgemode="directed")
  } 

  amat    <- as.adjMAT(value)
  is.acyc <- isAcyclicMAT(amat)

  if (result=="matrix")
    value <- amat
  
  if (is.acyc){
    value
  } else {
    NULL
  }
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



as.adjMAT <- function(object){
  if(!is(object, "graphNEL"))
    stop("Must be a graphNEL object...")
  
  ed    <- edges(object)
  vn    <- nodes(object)
  amat  <- matrix(0L, nc=length(vn), nr=length(vn), dimnames=list(vn,vn))
  
  if(length(ed)>0){
    for (ii in 1:length(ed)){
      vv <- names(ed)[[ii]]
      ww <- ed[[ii]]
      amat[vv,ww] <- 1L
    }
  }
  return(amat)
}



## ug <- function(...){
##   ugList(list(...))
## }

## ugMAT <- function(...){
##   ugListMAT(list(...))
## }

## ugListMAT <- function(x){
##   as.adjMAT(ugList(x))
## }

## ### ugList:
## ### -------
## ## Create graphNEL from a list of model generators. 
## ## Each generator can be an RHS-formula or a vector.
## ## January 2011.
## ugList <- function(x){
  
##   isForm <-sapply(x, inherits, "formula")
##   flist  <- x[isForm]
##   gset   <- lapply(flist, function(f) rhsf2list(f))
##   gset   <- unlist(gset, recursive=FALSE)
	
##   gset   <- c(gset, x[!isForm])
##   V      <- unique(unlist(gset))
  
##   zzz    <- lapply(gset, function(xx) names2pairs(xx, sort=FALSE, result="matrix"))
##   uuu    <- do.call(rbind, zzz)
##   if (nrow(uuu)>0)
##     tofrom <- unique(rowmat2list(uuu))
##   else
##     tofrom <- NULL
  
##   if (length(tofrom)==0){
##     value <-   new("graphNEL", nodes=V, edgemode="undirected")
##   } else {
##     fff <- do.call(rbind,tofrom)
##     value <- ftM2graphNEL(fff, V=V, edgemode="undirected")
##   } 
##   value
## }
## # x <- list(~a:b:c+c:d:e+d:e:f+q)
## # x <- list(~a:b:c+c:d:e+d:e:f+q, c(1,2,3))
## # g2 <- ugList2(x)
## # g  <- ugList(x)
## # system.time({for(ii in 1:1000) ugList2(x)})
## # system.time({for(ii in 1:1000) ugList(x)})
## # # About the same speed



## dag <- function(...){
##   dagList(list(...))
## }

## dagMAT <- function(...){
##   dagListMAT(list(...))
## }

## dagListMAT <- function(x){
##   as.adjMAT(dagList(x))
## }

## ### dagList:
## ### --------
## ## Create graphNEL from a list of model generators. 
## ## Each generator can be an RHS-formula or a vector.
## ## It is checked that there are no cycles in the graph.
## dagList <- function(x){
##   isForm <-sapply(x, inherits, "formula")
##   flist  <- x[isForm]
##   gset   <- lapply(flist, function(f) rhsf2list(f))
##   gset   <- unlist(gset, recursive=FALSE)
	
##   gset   <- c(gset, x[!isForm])
##   V      <- unique(unlist(gset))
  
##   zzz    <- lapply(gset, function(xx) names2pairs(xx[1],xx[-1], sort=FALSE, result="matrix"))
##   uuu    <- do.call(rbind, zzz)
##   if (nrow(uuu)>0)
##     tofrom <- unique(rowmat2list(uuu))
##   else
##     tofrom <- NULL
  
##   if (length(tofrom)==0){
##     value <-   new("graphNEL", nodes=V, edgemode="directed")
##   } else {
##     fff <- do.call(rbind,tofrom)[,2:1,drop=FALSE]
##     value <- ftM2graphNEL(fff, V=V, edgemode="directed")
##   } 

##   is.acyc <- .is.acyclic(as.adjMAT(value))
  
##   if (is.acyc){
## 	value
##   } else {
## 	NULL
##   }
## }
# x <- list(~a, ~b*a*c+c*f, c("k","l"), "u")
# dagList2(x)
# dagList2(list(~a:b, ~b:c, ~c:d, ~d:a))
# dagList2(list(~b:a, ~c:b, ~d:c:b))


## ugList <- function(x){
##   isForm <-sapply(x, inherits, "formula")
##   flist  <- x[isForm]
##   gset   <- lapply(flist, function(f) rhsf2list(f))
##   gset   <- unlist(gset, recursive=FALSE)
	
##   gset   <- c(gset, x[!isForm])
##   V      <- unique(unlist(gset))
  
##   zzz    <- lapply(gset, function(xx) names2pairs(xx, sort=FALSE, result="matrix"))
##   uuu    <- do.call(rbind, zzz)
##   if (nrow(uuu)>0)
##     tofrom <- unique(rowmat2list(uuu))
##   else
##     tofrom <- NULL
  
##   if (length(tofrom)==0){
##     value <-   new("graphNEL", nodes=V, edgemode="undirected")
##   } else {
##     fff <- do.call(rbind,tofrom)
##     value <- ftM2graphNEL(fff, V=V, edgemode="undirected")
##   } 
##   value
## }

# x <- list(~a:b:c+c:d:e+d:e:f+q)
# x <- list(~a:b:c+c:d:e+d:e:f+q, c(1,2,3))
# g2 <- ugList2(x)
# g  <- ugList(x)
# system.time({for(ii in 1:1000) ugList2(x)})
# system.time({for(ii in 1:1000) ugList(x)})
# # About the same speed



##################################
###### OLD STUFF BELOW HERE ######
##################################

## ugList <-  function(x){
  
##   #isForm <-sapply(x, inherits, "formula")
##   isForm <- unlistPrim(lapply(x, function(a) {"formula" %in% class(a)}))
  
##   flist <- x[isForm]

##   ans <- lapply(flist, function(f){
##     tt    <- terms(f)
##     ##glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
##     glist <- strsplit(attr(tt,"term.labels"),":|\\*")
##     V     <- rownames(attr(tt,"factors"))
##     list(glist=glist,V=V)
##   }) 

  
##   gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
##   V     <- lapply(ans, "[[", "V")
  
##   gset <- c(gset, x[!isForm])
##   V    <- unique(unlist(c(V, x[!isForm])))
  
##   ed   <- unlist(lapply(gset, names2pairs, sort=FALSE), recursive=FALSE)
##   ed   <- unique(lapply(ed, sort))
##   ed   <- ed[sapply(ed,length)==2]
  
##   if (length(ed)==0){
##     value <-   new("graphNEL", nodes=V, edgemode="undirected")
##   } else {
##     value <- ftM2graphNEL(do.call(rbind, ed), V=V, edgemode="undirected")
##   } 
##   value
## }




## ugListMAT <-  function(x){
##   n2p <- function(set){
##     len <- length(set)
##     if (len==2)
##       return(set)
##     idx1<-unlistPrim(lapply(1:(len-1), function(i) rep(i, len-i)))
##     idx2<-unlistPrim(lapply(1:(len-1), function(i) seq(i+1, len)))
##     ans<-cbind(set[idx1],set[idx2])
##                                         #  ans <- split(ans, row(ans))
##     ans
##   }

##   isForm <- unlistPrim(lapply(x, function(a) {"formula" %in% class(a)}))
  
##   flist <- x[isForm]
  
##   ans <- lapply(flist, function(f){
##     tt    <- terms(f)
##     ##glist <- remove.redundant(strsplit(attr(tt,"term.labels"),":|\\*"))
##     glist <- strsplit(attr(tt,"term.labels"),":|\\*")
##     V     <- rownames(attr(tt,"factors"))
##     list(glist=glist,V=V)
##   }) 
  
  
##   gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
##   gset <- c(gset, x[!isForm])
  
##   V     <- lapply(ans, "[[", "V")
##   V    <- unique(unlist(c(V, x[!isForm])))
  
##   ed <- gset[lapply(gset,length)>1]
  
##   if (length(ed)>0){    
##     ed   <- do.call(rbind, lapply(ed, n2p))
##     ed   <- split(ed, row(ed))
##     ed   <- do.call(rbind, removeRedundant(ed))
##     mm   <- matrix(charmatch(ed,V),nc=2)
##     amat <- matrix(0L, nr=length(V), nc=length(V))
##     dimnames(amat) <- list(V,V)
##     amat[rbind(mm,mm[,2:1])] <- 1L
##   } else {
##     amat <- matrix(0L, nr=length(V), nc=length(V))
##     dimnames(amat) <- list(V,V)
##   }
##   amat    
## }




## dagList  <- function(x){
##   isForm <-sapply(x, inherits, "formula")
##   flist <- x[isForm]

##   ans <- lapply(flist, function(f){
##     tt    <- terms(f)
##     glist <- removeRedundant(strsplit(attr(tt,"term.labels"),":|\\*"))
##     V     <- rownames(attr(tt,"factors"))
##     list(glist=glist,V=V)
##   }) 

##   gset  <-lapply(lapply(ans, "[[", "glist"), unlist)
##   V     <- lapply(ans, "[[", "V")
  
##   gset <- c(gset, x[!isForm])
##   V    <- unique(unlist(c(V, x[!isForm])))
  
##   gset <- lapply(gset, function(xx) names2pairs(xx[1],xx[-1], sort=FALSE))
##   gset <- unlist(gset,recursive=FALSE)
##   gset <- unique(gset)

##   ed   <- gset[sapply(gset,length)==2]

##   if (length(ed)==0){
##     value <-   new("graphNEL", nodes=V, edgemode="directed")
##   } else {
##     value <- ftM2graphNEL(do.call(rbind, lapply(ed,rev)), V=V, edgemode="directed")
##   } 

##   ## Check if acyclic:
##   ##
##   is.acyc <- TRUE
##   amat <- as.adjMAT(value)
##   elorder <- NULL
  
##   repeat{
##     idx <- which(rowSums(amat)==0)
##     if (!length(idx)){
##       return(NULL)
##     }
##     elorder <- c(elorder, idx)
##     amat <- amat[-idx,-idx]
    
##     if(all(c(0,0)==dim(amat))){
##       break()
##     }
##   }
##   zzz <- names(rev(elorder))
  
##   if(!is.null(zzz))
##     return(value)
##   else
##     return(NULL)
## }












## dagListMAT <-  function(x){
##   n2p <- function(set){
##     len <- length(set)
##     if (len==2)
##       return(set)
##     idx1<-unlistPrim(lapply(1:(len-1), function(i) rep(i, len-i)))
##     idx2<-unlistPrim(lapply(1:(len-1), function(i) seq(i+1, len)))
##     ans<-cbind(set[idx1],set[idx2])
##                                         #  ans <- split(ans, row(ans))
##     ans
##   }

##   isForm <- unlistPrim(lapply(x, function(a) {"formula" %in% class(a)}))
  
##   flist <- x[isForm]
  
##   ans <- lapply(flist, function(f){
##     tt    <- terms(f)
##     glist <- removeRedundant(strsplit(attr(tt,"term.labels"),":|\\*"))
##     ##glist <- strsplit(attr(tt,"term.labels"),":|\\*")
##     V     <- rownames(attr(tt,"factors"))
##     list(glist=glist,V=V)
##   }) 


##   gset  <- unlist(lapply(ans, "[[", "glist"), recursive=FALSE)
##   gset <- c(gset, x[!isForm])

##   V    <- lapply(ans, "[[", "V")
##   V    <- unique(unlist(c(V, x[!isForm])))

##   gset<-gset[lapply(gset,length)>1]

##   if (length(gset)>0){
##     gset <- lapply(gset, function(xx) {
##       if (length(xx)>2)
##         cbind( xx[rep(1,length(xx)-1)], xx[2:length(xx)])
##       else
##         xx  
##     })

    
##     ed<- do.call(rbind, gset)
##     mm   <- matrix(charmatch(ed,V),nc=2)
##     amat <- matrix(0L, nr=length(V), nc=length(V))
##     dimnames(amat) <- list(V,V)
##     amat[mm[,2:1]] <- 1L
##   } else {
##     amat <- matrix(0L, nr=length(V), nc=length(V))
##     dimnames(amat) <- list(V,V)
##   }


##   ## Check if acyclic:
##   ##
##   is.acyc <- TRUE
##   elorder <- NULL
##   amat2 <- amat
##   repeat{
##     idx <- which(rowSums(amat2)==0)
##     if (!length(idx)){
##       return(NULL)
##     }
##     elorder <- c(elorder, idx)
##     amat2 <- amat2[-idx,-idx]
    
##     if(all(c(0,0)==dim(amat2))){
##       break()
##     }
##   }
##   zzz <- names(rev(elorder))
  
##   if(!is.null(zzz))
##     return(amat)
##   else
##     return(NULL)
## }

