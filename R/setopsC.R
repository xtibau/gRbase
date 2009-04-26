
## A function to remove redundant generators.  If maximal=T, returns
## the maximal generators, if =F, the minimal generators.

## Can be speeded up if the as.character part can be avoided...

removeRedundant <- function(setlist, maximal=TRUE, index=FALSE){
  if (length(setlist)<=1){
    if (index)
      return(1)
    else
      return(setlist)
  }
  lenx  <- c(lapply(setlist,length), recursive=TRUE)
  if (maximal){
    o     <- order(lenx, decreasing=TRUE)
    x2    <- setlist[o]

##     x2    <- lapply(x2, as.character)
##     ll    <- cumsum(unlistPrim(lapply(x2,length)))
##     i<-.C("maxset", unlistPrim(x2), ll, length(x2),
##           ans=integer(length(x2))
##           , PACKAGE="gRbase"
##           )$ans

    ll    <- cumsum(c( lapply(x2,length), recursive=TRUE ))
    iii<-.C("maxset", as.character(c(x2, recursive=TRUE)), ll, length(x2), ans=integer(length(x2))
          , PACKAGE="gRbase")$ans
    
    iii <- iii[order(o)]
  } else {
    o     <- order(lenx, decreasing=FALSE)
    x2    <- setlist[o]


##     x2    <- lapply(x2, as.character)
##     ll    <- cumsum(unlistPrim(lapply(x2,length)))
##     i<-.C("minset", unlistPrim(x2), ll, length(x2), ans=integer(length(x2))
##           ,PACKAGE="gRbase")$ans

    ll    <- cumsum(c( lapply(x2,length), recursive=TRUE ))
    iii<-.C("minset", as.character(c(x2, recursive=TRUE)), ll, length(x2), ans=integer(length(x2))
          ,PACKAGE="gRbase")$ans
    
    iii <- iii[order(o)]  
  }  
  if (index){
    iii
  } else {
    setlist[iii==1]
  }
}

## Is x contained in any vector in setlist; 
is.insetlist <- function(x, setlist, index=FALSE){
  isin(setlist, x, index)
}

isin <- function(setlist, x, index=FALSE){

  len.setlist <- length(setlist)
  if (len.setlist==0){
    if (index)
      return(0)
    else
      return(FALSE)
  } 
  
  if (len.setlist < 1){
    if (index)
      return(rep(1,length(x)))
    else
      return(TRUE)
  }
  
##   setlist <- lapply(setlist, "as.character")
##   x <- as.character(x)
##   ll <- cumsum(unlistPrim(lapply(setlist,length)))
##   iii<-.C("isin", x, length(x), unlist(setlist), ll , len.setlist,
##         ans=integer(len.setlist),PACKAGE="gRbase")$ans


  ll    <- cumsum(c( lapply(setlist,length), recursive=TRUE ))
  iii<-.C("isin", as.character(x), length(x), as.character(c(setlist,recursive=TRUE)), ll, len.setlist,
        ans=integer(len.setlist),PACKAGE="gRbase")$ans
  
  

  if (index) {
    return(iii)
  } else {
    return(any(iii))
  }
}


## Faster versions of 'standard R functions'
##


is.subsetof <- function(x, set){
  all(.Internal(match(x,  set,  NA_integer_,  NULL))>0)
}

subsetof <- function(x, y){
  all(.Internal(match( x, y, 0, NULL))>0)
}


## subsetofList <- function(x,l){ 
##   any(unlistPrim(lapply(l, function(y) subsetof(x,y))))
## }
