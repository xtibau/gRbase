####
#### Implementation of potentials based on arrays
#### Foulum, September 2008
####

## Create ptable object (really an array)
##
ptable <- function(varNames, levels, values=1, normalize=c("none","first","all"), smooth=0){

  varNames <- rhsFormula2list(varNames)[[1]]

##   print(varNames)
##   print(levels)
  
  if (is.list(levels)){
    #print("hhhhh")
    dimnames = levels
    names(dimnames) <- varNames
    levels  = sapply(dimnames, length)

    
  } else {

    dimnames=makeDimNames(varNames, levels)
  }

  normalize <- match.arg(normalize, choices=c("none","first","all"))

  if (smooth>0){
    values <- values + smooth
  }

  ans <- array(values, dim=levels, dimnames=dimnames)
  switch(normalize,
         "first"={
           if (length(dim(ans))>1){
             marg  <- 2:length(dim(ans))
                                        #ma    <- apply(ans, marg, sum)
                                        #ans   <- sweep(ans, marg, ma, "/")        

             ma2 <- tableMargin(ans, marg)
             ans <- tablePerm(.tableOp2(ans, ma2, op=`/`), names(dimnames(ans)))        
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
makeDimNames <- function(varNames, levels, sep=''){
  if (missing(varNames) || is.null(varNames))
    return(lapply(levels, seq))
  lev <- lapply(levels, function(a) c(1:a))
  mapply(function(n,v) paste(n,v,sep=sep), varNames, lev, SIMPLIFY=FALSE)
}



as.ptable  <- function(values, normalize=c("none","first","all"), smooth=0){
  ##values <- x
  normalize <- match.arg(normalize, choices=c("none","first","all"))

  if (!inherits(values, c("array","matrix","integer","double","table"))){
    stop("arg must be array, matrix, table, integer or double\n")
  }
  
  if (smooth>0){
    values <- values + smooth
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

  return(ans)
}  















