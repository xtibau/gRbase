uniquePrim <- function(x){
  #.Internal(unique(x, FALSE, FALSE))
  unique(x)
}

setdiffPrim <- function (x, y) 
{
  setdiff(x,y)
##   x <- as.vector(x)
##     y <- as.vector(y)
##     uniquePrim(
##                if (length(x) || length(y)) { 
##                  x[.Internal(match( x, y, 0, NULL)) == 0]
##                } else
##                {x}
##                )
    
}

intersectPrim <- function (x, y) 
{
##   y <- as.vector(y)
##   .Internal(unique( y[  .Internal(match( as.vector(x), y, 0, NULL))    ], FALSE, FALSE))
  intersect(x,y)
}


unlistPrim <- function(l, recursive=TRUE, use.names=TRUE){
  #.Internal(unlist(l, recursive, use.names))
  unlist(l, recursive, use.names)
}
  

## colSumsPrim <- function(x){  
##   ##   n <- dim(x)[1]
##   ##   dn <- dim(x)[2]
##   ##   .Internal(colSums(x, n, prod(dn), FALSE))
##   colSums(x)
## }

## rowSumsPrim <- function(x){
## ##   dn <- dim(x)[1]
## ##   p  <- dim(x)[2]
## ##   .Internal(rowSums(x, prod(dn), p, FALSE))
##   rowSums(x)
## }

outerPrim <- function(X,Y){
  nX <- length(X)
  nY <- length(Y)
  Y <- rep(Y, rep.int(length(X), length(Y)))
  X <- rep(X, times = ceiling(length(Y)/length(X)))
  ans<-X*Y
  dim(ans)<-c(nX,nY)
  ans
}

matchPrim<-function(x,table){
  #.Internal(match(x,table, NA_integer_, NULL))
  match(x,table)
}
