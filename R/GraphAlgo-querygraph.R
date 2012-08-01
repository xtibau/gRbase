#######################################################################
####
#### querygraph provides unified interface to graph operations.
####
#### Works on graphNEL objects, igraph objects, and adjacency matrices
####
#### Notice: when a graph is returned it is always a graphNEL object
####
#######################################################################

querygraph <-function(object, op, set=NULL, set2=NULL, set3=NULL){
  
  ## From graph / RBGL packages
  graph.RBGL <-
    c("maxClique", 
      "connectedComp",
      "separates",
      "adj", 
      "is.triangulated", 
      "subgraph",
      "nodes",
      "edges")
  ## From gRbase
  gRbase <-
    c("ancestors", 
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
      "vpar")
  
  op=match.arg(op, choices=c(graph.RBGL, gRbase))

  object <- coerceGraph(object, "graphNEL")
  
  switch(op,
         ## Functions from graph/RBGL package here.
         "maxClique"=        { maxClique(object)$maxCliques              },
         "connectedComp"=    { connectedComp(object)                     },
         "separates"=        { separates(set, set2, set3, object)        },
         "adj"=              { adj(object, set)                          },    
         "is.triangulated"=  { is.triangulated(object)                   },         
         "subgraph"=         { subGraph(set, object)                     },         
         "nodes"=            { nodes(object)                             },
         "edges"=            { edges(object)                             },         
         ## gRbase functions
         "ancestors"=,"an"=  { ancestors(set, object)		         },
         "ancestralGraph"=   { ancestralGraph(set, object)	         },
         "ancestralSet"=     { ancestralSet(set, object)                 },
         "children"=         { children(set, object)         	         },
         "closure"=          { closure(set, object)          	         },
         "edgeList"=         { edgeList(object)	       		         },
         "is.decomposition"= { is.decomposition(set, set2, set3, object) },
         "is.complete"=      { is.complete(object, set)         	 },
         "is.simplicial"=    { is.simplicial(set, object)         	 },
         "parents"=          { parents(set, object)         		 },
         "simplicialNodes"=  { simplicialNodes(object)         	         },
         "vpar"=             { vpar(object)         			 }
         )  
}


########################################################################
###
### Functions which return vectors
###
########################################################################

## adjmat based
ancestors <- function(set, object){
  amat  <- as.adjMAT(object)
  if (isUndirectedMAT(amat))
    return(NULL)
  
  An <- setorig <- set
  amat  <- amat[-match(set, rownames(amat)),]
  
  repeat{
    set2 <- rowSums(amat[,set, drop=FALSE])
    set  <- names(which(set2>0))
    if (!length(set))
      break()
    An <- c(An, set)
    amat  <- amat[set2 == 0,,drop=FALSE]
  }
  setdiff(An, setorig)
}

## adjmat based
ancestralSet <- function(set, object){

  amat  <- as.adjMAT(object)
  if (isUndirectedMAT(amat))
    return(NULL)

  if (missing(set))
    stop("'set' must be given..\n")
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
    idx   <- match(A0, colnames(amat))
    amat  <- amat[-idx,-idx,drop=FALSE]
    vn    <- colnames(amat)
    A0    <- intersect(B,vn)
    if (!length(A0))
      break()
  }
  names(an[an>0])
}

## adjmat based
parents <- function(set, object){
  amat  <- as.adjMAT(object)
  if (isUndirectedMAT(amat))
    return(NULL)

  pa   <- names(which(amat[,set]>0))
  pa   <- setdiff(pa,set)
  if (length(pa)) pa else NULL
}

## adjmat based
vpar <- function(object){
  if (edgemode(object)=="undirected")
    stop("Graph is undirected; (v,pa(v)) does not exist...\n")
  amat <- as.adjMAT(object)
  vn   <- rownames(amat) 
  ans  <- vector("list", length(vn))
  for (ii in seq_along(vn)){
    ans[[ii]] <- c(vn[ii], vn[amat[,ii]>0])
  }
  names(ans) <- vn
  return(ans)
}

## graphNEL based
children <- function(set, object){
  if (edgemode(object)=="undirected")
    return(NULL)
  ch <- structure(unlist(edges(object)[set]), names=NULL)
  if (length(ch)) ch else NULL
}

## graphNEL based
closure <- function(set, object){
  uniquePrim(c(set, unlist(adj(object, set))))
}

## graphNEL based
simplicialNodes <- function(object){
  nodes <- nodes(object)
  b     <- unlistPrim(lapply(nodes, function(s) is.simplicial(s, object)))
  sim   <- nodes[b]
  return(sim)
}



########################################################################
###
### Functions which return graphs
###
########################################################################

ancestralGraph <- function(set, object){
  subGraph(ancestralSet(set, object), object)
}

########################################################################
###
### Boolan graph funcions (is.something)
###
########################################################################

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
    separates(set, set2, set3, object) & is.complete(object, set3)
  } else {
    FALSE
  }
}

is.simplicial <- function(set, object){
  x <- unlist(adj(object,set))
  is.complete(subGraph(x, object), x)
}
