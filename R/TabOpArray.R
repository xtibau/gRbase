####
#### Implementation of potentials based on arrays
#### Foulum, September 2008
####

## Create ptable object (really an array)
##
ptable <- function(varNames, nLevels, values=1, normalize=c("none","first","all"),
                 smooth=0 
                 ){

  if (is.list(nLevels)){
    dimnames = nLevels
    names(dimnames) <- varNames
    nLevels  = sapply(dimnames, length)
  } else {
    dimnames=makeDimNames(varNames, nLevels)
  }

  normalize <- match.arg(normalize, choices=c("none","first","all"))

  if (smooth>0){
    values <- values + smooth
  }

  ans <- array(values, dim=nLevels, dimnames=dimnames)

  switch(normalize,
    "first"={
      if (length(dim(ans))>1){
        marg  <- 2:length(dim(ans))
        ma    <- apply(ans, marg, sum)
        ans   <- sweep(ans, marg, ma, "/")
      } else {
        ans <- ans / sum(ans)
      }
    },
    "all"={ans <- ans / sum(ans)
    },
    "none"={}
    )

  class(ans) <- "ptable"
  return(ans)
}



## Accessors
##
varNames.array    <- function(x) names(attr(x,"dimnames"))
nLevels.array     <- function(x) dim(x)
valueLabels.array <- function(x) attr(x,"dimnames")

varNames.ptable    <- function(x) names(attr(x,"dimnames"))
nLevels.ptable     <- function(x) dim(x)
valueLabels.ptable <- function(x) attr(x,"dimnames")

print.ptable  <- function(x,...){
  class(x)<-NULL
  print(x)
  invisible(x)
}




## Create list with dimension names
##
makeDimNames <- function(varNames, nLevels, sep=''){
  if (missing(varNames) || is.null(varNames))
    return(lapply(nLevels, seq))
  lev <- lapply(nLevels, function(a) c(1:a))
  mapply(function(n,v) paste(n,v,sep=sep), varNames, lev, SIMPLIFY=FALSE)
}

## Coercion
##
as.ptable  <- function(x, ...){
  values <- x
  if (!inherits(values, c("array","matrix","integer","double"))){
    stop("arg must be array, matrix, integer or double\n")
  }
  if (is.null(dimnames(values))){
    if (!is.null(dim(values)))
      nLevels <- dim(values)
    else 
      nLevels <- length(values)
    varNames <- paste("V", 1:length(nLevels),sep='')
    dimnames <- makeDimNames(varNames, nLevels)
    ans <- array(values, dim = nLevels, dimnames = dimnames)
    class(ans) <- "ptable"
  } else {
    ans <- values
    class(ans) <- "ptable"
  }
  return(ans)
}  
















## Marginalize array onto margin
##
tableMarginPrim <- function(t1, margin, normalize=FALSE){
  if (missing(margin) || (length(margin)==1 && is.na(margin))){
    return(sum(as.numeric(t1)))
  }
  vn    <- names(dimnames(t1))
  idx   <- match(margin,vn)
  x     <- apply(t1, idx, sum)
  if (normalize)
    x <- x/sum(x)
  att           <- attributes(t1)
  attributes(x) <- list(dim=att$dim[idx], dimnames=att$dimnames[idx], class="ptable")
  x
}


