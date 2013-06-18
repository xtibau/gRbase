uniquePrim <- function(x){
  unique.default(x)
}

setdiffPrim <- function (x, y){
  ##     x <- as.vector(x)
  ##     y <- as.vector(y)
  ##     uniquePrim(if (length(x) || length(y))
  ##            x[match(x, y, 0L) == 0L]
  ##     else x)  
  unique.default(if (length(x) || length(y))
                 x[match(x, y, 0L) == 0L]
  else x)
}

intersectPrim <- function (x, y){
  ##y <- as.vector(y)
  ##uniquePrim(y[match(as.vector(x), y, 0L)])
  unique.default(y[match(x, y, 0L)])
}

unlistPrim <- function(l){
  c(l, recursive=TRUE)
}

matchPrim<-function(x,table){
  match(x,table)
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
