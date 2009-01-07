
## Works on graphNEL objects
##
querygraph <-function(object, type, set=NULL, set2=NULL, set3=NULL){

  if (!is(object,"graphNEL"))
    stop("queryg2 needs a graphNEL object\n")
  
  type=match.arg(type,
    choices=c( 
      ## From graph / RBGL packages
      ##
      "maxClique", 
      "connectedComp",
      "separates",
      "adj", 
      "is.triangulated", 
      "subgraph",
      "nodes",
      "edges",                          
      
      ## SHD functions
      ##
      "ancestors", 
      "ancestralGraph",
      "ancestralSet",
      "children",  
      "closure",  
      "edgeList", 
      "is.decomposition",
      "is.complete",
      "is.simplicial",
      "parents", 
      "simplicialNodes",
      "vpav"
      ))
  
##  nelobject <- .grash2nel(object)

  switch(type,
         ## Functions from graph/RBGL package here.
         ##

         "maxClique"={
           maxClique(object)$maxCliques
         },

         "connectedComp"={
           connectedComp(object)
         },

         "separates"={
           separates(set, set2, set3, object)
         },

         "adj"={
           adj(object, set)
         },
                  
         "is.triangulated"={
           is.triangulated(object)
         },
         
         "subgraph"={
           subGraph(set, object)
         },         

         "nodes"={
           nodes(object)
         },
         "edges"={
           edges(object)
         },

         ## !!

         
         ## SHD graph functions here
         "ancestors"=,"an"={
           ancestors(set, object)
         },

         "ancestralGraph"={
           ancestralGraph(set, object)
           ##subGraph(.ancestral_set(object, set), object)
         },         
         "ancestralSet"={
           ancestralSet(set, object)
         },
                  
         "children"={
           children(set, object)
           ##ch <- structure(unlist(edges(object)[set]), names=NULL)
           ##if (length(ch)) ch else NULL
         },

         
         "closure"={
           closure(set, object)
           ##unique(c(set, unlist(adj(object, set))))
         },

         "edgeList"={
           edgeList(object)
         },
         
         "is.decomposition"={
           is.decomposition(set, set2, set3, object)
           ## vn <- uniquePrim(c(set, set2, set3))
           ##            sg <- subGraph(vn, object)
           ##            separates(set, set2, set3, sg) & .is_complete(sg, set3)
         },

         "is.complete"={
           is.complete(object, set)
         },
         
         "is.simplicial"={
           is.simplicial(set, object)
         },
         
         "parents"={
           parents(set, object)
         },

         "simplicialNodes"={
           simplicialNodes(object)
         },

         "vpav"={
           vpav(object)
         }
         )  
}



ancestors <- function(set, object){
  An <- setorig <- set
  x <- as.adjMAT(object)
  x <- x[-match(set, rownames(x)),]
  
  repeat{
    set2 <- rowSums(x[,set, drop=FALSE])
    set <- names(which(set2>0))
    if (!length(set))
      break()
    An <- c(An, set)
    x <- x[set2 == 0,,drop=FALSE]
  }
  setdiff(An, setorig)
}

ancestralGraph <- function(set, object){
  subGraph(ancestralSet(set, object), object)
}

ancestralSet <- function(set, object){
  if (missing(set))
    stop("'set' must be given..\n")
  amat <- as.adjMAT(object)
  vn   <- colnames(amat)
  an   <- rep(0, length(vn))
  names(an) <- vn
  an[set]   <- 1
  
  A0 <- set
  repeat{
    x <- amat[,A0,drop=FALSE]
    B <- rownames(x)[apply(x,1,sum)>0]
    if (!length(B))
      break()
    an[B] <- 1
    idx  <- match(A0, colnames(amat))
    amat <- amat[-idx,-idx,drop=FALSE]
    vn   <- colnames(amat)
    A0   <- intersect(B,vn)
    if (!length(A0))
      break()
  }
  names(an[an>0])
}


children <- function(set, object){
  ch <- structure(unlist(edges(object)[set]), names=NULL)
  if (length(ch)) ch else NULL
}


closure <- function(set, object){
  uniquePrim(c(set, unlist(adj(object, set))))
}

## Should be declared as a method for graphNEL's
##
## edgePairs <- function(object){
##   if(!is(object, "graphNEL"))
##     stop("Must be a graphNEL object...")
  
##   ed  <- edges(object)
##   ed  <- ed[lapply(ed,length)>0]
##   ed2 <- mapply(function(a,b)names2pairs(a,b,sort=FALSE), ed,names(ed),SIMPLIFY=FALSE)
##   ed2 <- structure(unlist(ed2, recursive=FALSE), names=NULL)
##   ed2 <- remove.redundant(ed2)
##   if(length(ed2)==0)
##     return(NULL)
##   ed2
## }



## Returns edges (pairs of vertices) of graph object.
## Based on lower triangular part of adjacency matrix; 
## hence for directed graphs it has the form (from, to)
## FIXME: Should check for graphNEL
edgeList <- function(object,matrix=FALSE){
  m <- as.adjMAT(object)
  if (edgemode(object)=="undirected")
    m[upper.tri(m)] <- 0
  epp <- which.arr.ind(m)
  ans <- matrix(colnames(m)[epp],nc=2)
  if (!matrix)
    ans<- split(ans,row(ans))
  ans
}



is.complete <- function(object, set=NULL){  
  if (is.null(set))
    submat <- as.adjMAT(object)
  else
    submat <- as.adjMAT(object)[set,set]
  all(submat[upper.tri(submat)]>0)
}

is.decomposition <- function(set, set2, set3, object){
  vn <- uniquePrim(c(set, set2, set3))
  if (setequal(vn, nodes(object))){
    #sg <- subGraph(vn, object)
    #separates(set, set2, set3, sg) & is.complete(sg, set3)
    separates(set, set2, set3, object) & is.complete(object, set3)
  } else {
    FALSE
  }
}


is.simplicial <- function(set, object){
  x <- unlist(adj(object,set))
  is.complete(subGraph(x, object), x)
}

parents <- function(set, object){
  amat <- as.adjMAT(object)
  pa <- names(which(amat[,set]>0))
  pa <- setdiff(pa,set)
  if (length(pa)) pa else NULL
}



simplicialNodes <- function(object){
  nodes <- nodes(object)
  b     <- unlistPrim(lapply(nodes, function(s) is.simplicial(s, object)))
  sim   <- nodes[b]
  return(sim)
}


vpav <- function(object){
  if (edgemode(object)=="undirected")
    stop("Graph is undirected; (v,pa(v)) does not exist...\n")
  amat <- as.adjMAT(object)
  vn <- rownames(amat) 
  ans <- vector("list", length(vn))
  for (ii in seq_along(vn)){
    ans[[ii]] <- c(vn[ii], vn[amat[,ii]>0])
  }
  names(ans) <- vn
  return(ans)
}




