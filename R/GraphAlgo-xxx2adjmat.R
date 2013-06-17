##################################################
##
## Convert list of generators to adjacency matrix
##
##################################################

## FIXME: Perhaps first arguments should be the same...
## For undirected graphs
glist2adjMAT <- function(glist, vn=uniquePrim(c(glist, recursive=TRUE)),
                         result="matrix"){
  result <- match.arg(result, c("matrix","Matrix"))
  nr     <- length(vn)
  if (result=="matrix"){
    amat  <- matrix(0L, ncol=nr, nrow=nr, dimnames=list(vn,vn))
  } else {
    #amat  <- Matrix(0L, ncol=nr, nrow=nr, dimnames=list(vn,vn), sparse=TRUE)
    amat  <- .dgCMatrix(0L, ncol=nr, nrow=nr, dimnames=list(vn,vn))
  }
  ob.idx<-lapply(glist, match, vn)
  if(length(glist)>0){
    for (ii in 1:length(glist)){
      gg <- ob.idx[[ii]]
      amat[gg,gg] <- 1L
    }
  }
  
  iii <- 1+(nr+1)*((1:nr)-1)
  amat[iii] <- 0L ## faster than diag(sss)
  amat
}

## NEL 2 adjacency matrix
graphNEL2adjMAT <- as.adjMAT <- function(object, result="matrix"){
  if(!inherits(object, "graphNEL"))
    stop("'object' must be a graphNEL object...")

  result <- match.arg(result, c("matrix","Matrix","dgCMatrix"))  
  vn     <- graph::nodes(object)
  nr     <- length(vn)

  switch(result,
         "matrix"={
           amat  <- matrix(0L, ncol=nr, nrow=nr, dimnames=list(vn,vn))
         },
         "Matrix"=,"dgCMatrix"={
           amat  <- .dgCMatrix(0L, ncol=nr, nrow=nr, dimnames=list(vn,vn))
         })
           
  ftM  <- graphNEL2ftM(object)
  ftMi <- matrix(match(ftM, vn), ncol=2)
  amat[ftMi] <- 1L
  amat
}

graphNEL2matrix <- function(object){ as.adjMAT(object, result="matrix") }
graphNEL2dgCMatrix <- function(object){ as.adjMAT(object, result="Matrix") }
  





## graphNEL2ftM will return a matrix with an edge from ii to jj AND from jj to ii.
graphNEL2ftM <- function(object){
  if(!is(object, "graphNEL"))
    stop("Must be a graphNEL object...")
  ed  <- graph::edges(object)
  nn  <- names(ed)
  ans <- do.call(rbind,
                 mapply(function(ff,tt){
                   names2pairs(ff,tt, sort=FALSE, result="matrix")}, as.list(nn), ed, SIMPLIFY=FALSE))
  ans
}


## For DAGs
vpaL2adjMAT <- function(vpaL, vn=uniquePrim(c(vpaL, recursive=TRUE)),
                            result="matrix"){
  result <- match.arg(result, c("matrix","Matrix"))
  nr     <- length(vn)
  vpaI   <- lapplyMatch(vpaL, vn)
  TF     <- vpaL2tfM(vpaI)

  if (result=="matrix"){
    amat  <- matrix(0L, ncol=nr, nrow=nr, dimnames=list(vn,vn))
    amat[TF[,2:1]] <- 1L
    amat
  } else {
    #amat  <- Matrix(0L, ncol=nr, nrow=nr, dimnames=list(vn,vn), sparse=TRUE)
    #amat  <- setXtf(amat,TF)
    #amat <- sp_setXtf1(Matrix(0L, ncol=nr, nrow=nr, sparse=TRUE),TF)
    amat <- sp_setXtf1(.dgCMatrix(0L, ncol=nr, nrow=nr), TF)
    dimnames(amat) <- list(vn,vn)
    amat
  }
}

# (v,pa(v))-list 2 to-from-matrix
vpaL2tfM <- function(vpaL){
 eMat  <- lapply(vpaL, function(xx) names2pairs(xx[1], xx[-1], 
                                                 sort = FALSE, result = "matrix"))
 do.call(rbind, eMat)
}










##   ed    <- graph::edges(object)  
##   if(length(ed)>0){
##     for (ii in 1:length(ed)){
##       vv <- names(ed)[[ii]]
##       ww <- ed[[ii]]
##       if (length(ww)>0)
##         amat[vv,ww] <- 1L
##     }
##   }
##   return(amat)

