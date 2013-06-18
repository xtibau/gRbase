# In graph theory, a topological sort or topological ordering of a 
# directed acyclic graph (DAG) is a linear ordering of its nodes in 
# which each node comes before all nodes to which it has outbound edges. 
# Every DAG has one or more topological sorts. If such ordering can 
# not be found then the graph has cycles
#
# Input:  list of vectors of the form (v,pa(v))
# Output: vector with ordering
#
# should perhaps be called dagTopoSort


topoSort <- function(object, index=FALSE){
  UseMethod("topoSort")
}

topoSort.graphNEL <- function(object, index=FALSE){
  topoSortMAT(as(object,"Matrix"), index=index)
}

topoSort.matrix <- topoSort.Matrix <- function(object, index=FALSE){
  topoSortMAT(object, index=index)
}

topoSortMAT <- function(XX_, index=FALSE){
  if (inherits(XX_, "Matrix")){
    ans <- .Call("C_topoSortMAT_sp", XX_ ,package="gRbase")
  } else {
    if (inherits(XX_, "matrix")){
      ans <- .Call("C_topoSortMAT_st", XX_ ,package="gRbase")
    } else {
      stop("'XX_' must be a matrix or a sparse matrix (a 'dgCMatrix')")
    }
  }
  if (index){
    if (ans[1]!=-1){
      ans
    } else {
      -1L
    }
  } else {
    if (ans[1]!=-1){
      colnames(XX_)[ans]
    } else {
      character(0)
    }
  }
}



topoSort_vparList<- function(vpaL){

  ## sdp: same as setdiff(A[x],A[unique(y)]); x: logical; y integers
  ## faster than setdiff
  sdp <- function(A, x, y){ 
    x[uniquePrim(y)] <- FALSE
    A[x]
  }
  
  Vset <- unique(unlist(vpaL))
  if (length(Vset)==1)
    return(Vset)

  Iset  <- 1:length(Vset)
  vpaI  <- lapplyMatch(vpaL, Vset)
  eMat  <- vpaL2tfM(vpaL)
  iMat  <- vpaL2tfM(vpaI)
    
  activeEdges <- rep(TRUE, nrow(eMat))
  activeNodes <- rep(TRUE, length(Vset))
  LL          <- rep(NA, length(Vset)) #Topo ordering of vertices
  ## names(activeNodes) <- Vset

  cnt 	  <- 1
  is.acyc <- TRUE
  
  repeat{
    ## if (cnt %% 1000 == 0) cat(sprintf("topoSort of graph; node number=%5i\n", cnt))
    ## vvv <- setdiffPrim(Vset[activeNodes],eMat[activeEdges,1])
    zzz <- sdp(Iset, activeNodes, iMat[activeEdges,1])
    vvv <- Vset[zzz]
    
    if (length(vvv)==0){  # The test!
      is.acyc <- FALSE
      break
    }
    ## FIXME: topoSort: We can speed up by removing more vertices at the time
    orph  <- vvv[1]  
    orphI <- zzz[1]
    LL[cnt] <- orph
    activeEdges[eMat[,2]==orph] <- FALSE
    ## FIXME: topoSort: above can be  activeEdges[iMat[,2]==orphI] <- FALSE
    activeNodes[orphI] <- FALSE
    if (sum(activeEdges)==0)
      break
    cnt <- cnt + 1
  }
  is.acyc
    
  if (is.acyc){
    ## Need the last one
    LL[is.na(LL)] <- setdiffPrim(Vset,LL)
    return(LL)
  } else {
    return(character(0))
  }
  ## DONE!
}


