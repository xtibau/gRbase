
## Works on graphNEL objects
##
querygraph <-function(object, type, set=NULL, set2=NULL, set3=NULL){

  if (!is(object,"graphNEL"))
    stop("queryg2 needs a graphNEL object\n")
  
  type=match.arg(type,
    choices=c( 
      ## From graph / RBGL packages
      ##
      "maxClique","cliques", 
      "connectedComp","concomp",
      "separates",
      "adj",
      "closure",  "cl",
      "ne",
      "is.triangulated", "is.chordal",
      "subgraph",
      "nodes",
      "edges",                          
      
      ## SHD functions
      ##
      "ancestors", "an",
      "ancestralGraph",
      "ancestralSet",
      "children",  "ch",
      "edgePairs", 
      "is.decomposition",
      "is.complete",
      "is.simplicial",
      "parents",   "pa",      
      "simplicialNodes"
      ))
  
##  nelobject <- .grash2nel(object)

  switch(type,
         ## Functions from graph/RBGL package here.
         ##

         "maxClique"=,"cliques"={
           maxClique(object)$maxCliques
         },

         "connectedComp"=,"concomp"={
           connectedComp(object)
         },

         "separates"={
           separates(set, set2, set3, object)
         },

         "adj"={
           adj(object, set)
         },

         "closure"=, "cl"={
           unique(c(set, unlist(adj(object, set))))
         },
                  
         "is.triangulated"=,"is.chordal"={
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
           .ancestors(object, set)
         },

         "ancestralGraph"={
           subGraph(.ancestral_set(object, set), object)
         },         
         "ancestralSet"={
           .ancestral_set(object, set)
         },
                  
         "children"=,"ch"={
           ch <- structure(unlist(edges(object)[set]), names=NULL)
           if (length(ch)) ch else NULL
         },

         "edgePairs"={
           edgePairs(object)
         },
         
         "is.decomposition"={
           vn <- uniquePrim(c(set, set2, set3))
           sg <- subGraph(vn, object)
           separates(set, set2, set3, sg) & .is_complete(sg, set3)
         },

         "is.complete"={
           .is_complete(object, set)
         },
         
         "is.simplicial"={
           .is_simplicial(object, set)
         },
         
         "parents"=,"pa"={
           .parents(object,set)
         },

         "simplicialNodes"={
           .simplicialNodes(object)
         }
         
         )  
}


.is_simplicial <- function(object, set){
  x <- unlist(adj(object,set))
  .is_complete(subGraph(x, object), x)
}


.simplicialNodes <- function(object){
  nodes <- nodes(object)
  b     <- unlistPrim(lapply(nodes, function(s) .is_simplicial(object,s)))
  sim   <- nodes[b]
  return(sim)
}



.ancestors <- function(object, set){
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



## Does not check for graphNEL and directed
##
.parents <- function(object,set){
  amat <- as.adjMAT(object)
  pa <- names(which(amat[,set]>0))
  pa <- setdiff(pa,set)
  if (length(pa)) pa else NULL
}

## Does not check for graphNEL and undirected
##
.is_complete <- function(object, set=NULL){

  if (is.null(set))
    submat <- as.adjMAT(object)
  else
    submat <- as.adjMAT(object)[set,set]
  all(submat[upper.tri(submat)]>0)
}


.ancestral_set <- function(object, set){
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
