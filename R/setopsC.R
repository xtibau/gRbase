
## A function to remove redundant generators.  If maximal=T, returns
## the maximal generators, if =F, the minimal generators.

## Can be speeded up if the as.character part can be avoided...

removeRedundant <- remove.redundant <- maximalSet <- function(l, maximal=TRUE, index=FALSE){
  if (length(l)<=1){
    if (index)
      return(1)
    else
      return(l)
  }
  lenx  <- unlistPrim(lapply(l,length))
  if (maximal){
    o     <- order(lenx, decreasing=TRUE)
    x2    <- l[o]
    x2    <- lapply(x2, as.character)
    ll    <- cumsum(unlistPrim(lapply(x2,length)))
    i<-.C("maxset", unlistPrim(x2), ll, length(x2),
          ans=integer(length(x2))
          , PACKAGE="gRbase"
          )$ans
    i <- i[order(o)]
  } else {
    o     <- order(lenx, decreasing=FALSE)
    x2    <- l[o]
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
    l[i==1]
  }
}

## Is e contained in any vector in x; 
## NOTE: x (the list) is the first argument
## Exceptions....

isin <- function(l, x, index=FALSE){
  if (length(l)==0){
    if (index)
      return(0)
    else
      return(FALSE)
  }    
  if (length(l)< 1){
    if (index)
      return(rep(1,length(x)))
    else
      return(TRUE)
  }

  l <- lapply(l, "as.character")
  x <- as.character(x)
  
  ll <- cumsum(unlistPrim(lapply(l,length)))
  i<-.C("isin", x, length(x), unlist(l), ll , length(l),
        ans=integer(length(l)),PACKAGE="gRbase")$ans
  if (index) {
    return(i)
  } else {
    return(any(i))
  }
}





