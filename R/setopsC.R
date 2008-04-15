
## A function to remove redundant generators.  If maximal=T, returns
## the maximal generators, if =F, the minimal generators.

## Can be speeded up if the as.character part can be avoided...

remove.redundant <- maximalSet <- function(x, maximal=TRUE, index=FALSE){
  if (length(x)<=1){
    if (index)
      return(1)
    else
      return(x)
  }
  lenx  <- unlistPrim(lapply(x,length))
  if (maximal){
    o     <- order(lenx, decreasing=TRUE)
    x2    <- x[o]
    x2    <- lapply(x2, as.character)
    ll    <- cumsum(unlistPrim(lapply(x2,length)))
    i<-.C("maxset", unlistPrim(x2), ll, length(x2),
          ans=integer(length(x2))
          , PACKAGE="gRbase"
          )$ans
    i <- i[order(o)]
  } else {
    o     <- order(lenx, decreasing=FALSE)
    x2    <- x[o]
    x2    <- lapply(x2, as.character)
    ll    <- cumsum(unlistPrim(lapply(x2,length)))
    i<-.C("minset", unlistPrim(x2), ll, length(x2),
          ans=integer(length(x2))
          ,PACKAGE="gRbase"
          )$ans
    i <- i[order(o)]  
  }  
  if (index){
    i
  } else {
    x[i==1]
  }
}

## Is e contained in any vector in x; 
## NOTE: x (the list) is the first argument
## Exceptions....
isin <- function(x, e, index=FALSE){
  if (length(x)==0){
    if (index)
      return(0)
    else
      return(FALSE)
  }    
  if (length(x)< 1){
    if (index)
      return(rep(1,length(e)))
    else
      return(TRUE)
  }

  ll <- cumsum(unlistPrim(lapply(x,length)))
  i<-.C("isin", e, length(e), unlist(x), ll , length(x),
        ans=integer(length(x)),PACKAGE="gRbase")$ans
  if (index) {
    return(i)
  } else {
    return(any(i))
  }
}





