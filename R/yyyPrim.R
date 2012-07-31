uniquePrim <- function(x){
    ##.Internal(unique(x, FALSE, FALSE))
    x[!duplicated.default(x)]
}

setdiffPrim <- function (x, y)
{
    x <- as.vector(x)
    y <- as.vector(y)
    uniquePrim(if (length(x) || length(y))
           x[match(x, y, 0L) == 0L]
    else x)
}

intersectPrim <- function (x, y)
{
  ##   y <- as.vector(y)
  ##  .Internal(unique( y[  .Internal(match( as.vector(x), y, 0, NULL)) ], FALSE, FALSE))
  #intersect(x,y)
  y <- as.vector(y)
  uniquePrim(y[match(as.vector(x), y, 0L)])

}


unlistPrim <- function(l){
  ##.Internal(unlist(l, recursive, use.names))
  ##unlist(l, recursive, use.names)
  c(l, recursive=TRUE)
}


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
