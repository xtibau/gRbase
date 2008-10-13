
## Multiply two arrays
##
tableOp <- function(t1, t2, op = "*"){

  ## Find permutation of variables in set2 such that those in set1 are either
  ## to the far right or far left
  ##
  permidx <- function(set1, set2, direction="right"){
    idx <- 1:length(set2)
    i   <- match(set1, set2)
    switch(direction,
           "right"={c(idx[-i],i)},
           "left" ={c(i,idx[-i])}
           )
  }
  
  if (!is.array(t1)) 
    stop("'t1' is not an array")
  if (!is.array(t2)) 
    stop("'t2' is not an array")

  levels1 <- dimnames(t1)
  levels2 <- dimnames(t2)

  if (is.null(levels1))
    stop("'t1' does not have dimnames")
  if (is.null(levels2))
    stop("'t2' does not have dimnames")
  
  vn1    <- names(levels1)
  vn2    <- names(levels2)
  
  lev1 <- sapply(levels1,length)
  lev2 <- sapply(levels2,length)
  
  idx       <- match(vn2, vn1)
  if (any(is.na(idx))){
    augnames  <- vn2[is.na(idx)]
    auglevn   <- lev2[is.na(idx)]
    auglevels <- levels2[is.na(idx)]
    pot1      <- rep(as.numeric(t1), prod(auglevn))
    vn1       <- c(vn1, augnames)
    lev1      <- c(lev1, auglevn)
    levels1   <- c(levels1, auglevels)
    dim(pot1) <- lev1
  } else {
    pot1 <- t1
  }
  
  perm  <- permidx(set1=vn2, set2=vn1,"left")
  if (op=="*"){
    pot1   <- as.numeric(aperm(pot1, perm)) * as.numeric(t2)
  } else {
    pot1   <- as.numeric(aperm(pot1, perm)) / as.numeric(t2)
    pot1[!is.finite(pot1)] <- 0
  }
  attributes(pot1) <- list(dim=lev1[perm], dimnames=levels1[perm], class="ptable")

  pot1 <- tablePerm(pot1, vn1)
  #print(vn1); print(vn2)
  
  pot1
}


..tableMargin <-  function (x, margin) 
{
  if (!is.array(x)) 
    stop("'x' is not an array")
  if (length(margin)) {
    varnames <- names(dimnames(x))
    margin2 <- rep(NA, length(margin))
    for (kk in seq_along(margin)){
      mtmp2 <- mtmp <- margin[kk]
      if (is.character(mtmp)) 
        mtmp2 <- match(mtmp, varnames)
      
      if (is.na(mtmp2))
        stop("Variable ", mtmp, " does not exist in table")
      margin2[kk] <- mtmp2
    }

    z <- apply(x, margin2, sum)
    dim(z) <- dim(x)[margin2]
    dimnames(z) <- dimnames(x)[margin2]
  }
  else
    return(sum(x))
  class(z) <- oldClass(x)
  z
}


.tableMargin <-  function (x, margin) 
{
  if (!is.array(x)) 
    stop("'x' is not an array")

  if (length(margin)) {
    if (is.character(margin)){
      varnames <- names(dimnames(x))
      margin2 <- match(margin, varnames)
      if (any(is.na(margin2)))
        stop("Variable not in table...\n")
    } else {
      margin2 <- margin
    }
    
    ##     z <- apply(x, margin2, sum)
    ##     dim(z) <- dim(x)[margin2]
    ##     dimnames(z) <- dimnames(x)[margin2]
    z <- structure(apply(x, margin2, sum),
                   dim=dim(x)[margin2], dimnames=dimnames(x)[margin2])

  }
  else
    return(sum(x))
  class(z) <- oldClass(x)
  z
}


tableMargin <-  function (x, margin) 
{
  if (!is.array(x)) 
    stop("'x' is not an array")

  if (length(margin)) {
    if (is.character(margin)){
      varnames <- names(dimnames(x))
      margin2 <- match(margin, varnames)
      if (any(is.na(margin2)))
        stop("Variable not in table...\n")
    } else {
      margin2 <- margin
    }

    di <- dim(x)
    dn <- dimnames(x)
    vn <- names(dn)

    ##print(margin); print(margin2)
    ##print(vn); print(length(vn))
    ##m.idx <- match(margin2,vn)
    m.idx <- margin2
    ##cat("m.idx:"); print(m.idx)
    c.idx <- (1:length(vn))[-m.idx]
    ##cat("c.idx:"); print(c.idx);

    x <- aperm(x, c(c.idx, m.idx))

    nr <- prod(unlistPrim(lapply(dn[margin2],length)))
    
    nc <- ceiling(length(x)/nr)
    ##print(nr); print(nc)
    xmat  <- matrix(x,nr=nr, nc=nc, byrow=TRUE)
    ##xmat <- structure(x, dim=c(nr,nc))
    z <- rowSumsPrim(xmat)
    ##z <- structure(z, dim=di[m.idx], dimnames=dn[m.idx])
    z <- array(z, dim=di[m.idx], dimnames=dn[m.idx])
    
    ##     z <- apply(x, margin2, sum)
    ##     dim(z) <- dim(x)[margin2]
    ##     dimnames(z) <- dimnames(x)[margin2]
    ##z <- structure(apply(x, margin2, sum),
    ##               dim=dim(x)[margin2], dimnames=dimnames(x)[margin2])
  }
  else
    return(sum(x))
  class(z) <- oldClass(x)
  z
}








.tableSlice <-  function (x, margin, level, impose) 
{

  if(is.null(margin)) return(x)

  dn <- dimnames(x)
  varnames <- names(dn)

  margin2 <- rep(NA, length(margin))
  level2  <- rep(NA, length(level))
  for (kk in seq_along(margin)){
    mtmp2 <- mtmp <- margin[kk]
    ltmp2 <- ltmp <- level[kk]
    if (is.character(mtmp)) 
      mtmp2 <- match(mtmp, varnames)
    if (is.na(mtmp2))
      stop("Variable ", mtmp, " does not exist in table")
    if (is.character(ltmp)) 
      ltmp2 <- match(ltmp, dn[[mtmp2]])
    if (is.na(ltmp2))
      stop("Level ", ltmp, " does not exist in table")
    margin2[kk] <- mtmp2
    level2[kk]  <- ltmp2  
  }

  margin <- margin2
  level  <- level2

  
  d  <- dim(x)
  ld <- length(d)
  z  <- rep(TRUE,length(x))
  a  <- c(1,cumprod(d))

  for(i in 1:length(margin)) 
    {
      si  <-margin[i]; #print(si)
      idx2 <- rep(1:d[si],each=a[si],times=length(x)/(d[si]*a[si]))
      z <- z & level[i]==idx2
    }

  dr<-d[(1:ld)[-margin]]
  
  if (!missing(impose) && is.numeric(impose)){
    x[!z] <- impose
    return(x)
  } else {
    newdn <- dimnames(x)[-margin]
    return(array(as.vector(x)[z],dr, dimnames=newdn))
  }
}

tableSlice <-  function (x, margin, level, impose) 
{

  if(is.null(margin)) return(x)

  dn <- dimnames(x)
  varnames <- names(dn)

  if (is.character(margin)){
    margin2 <- match(margin, varnames)
    if (any(is.na(margin2)))
      stop("Variables: ", margin[is.na(margin2)], " do not exist in table...")
  } else {
    margin2 <- margin
  }

  if (is.character(level)){
    level2  <- rep(NA, length(level))
    for (kk in seq_along(margin)){
      level2[kk] <- match(level[kk],dn[[margin2[kk]]])
    }
    if (any(is.na(level2)))
      stop("Level: ", level[is.na(level2)], " do not exist in table...")
  } else {
    level2 <- level
  }

##   print(margin2)
##   print(level2)
  
  if (!missing(impose) && is.numeric(impose)){  
    d  <- dim(x)
    ld <- length(d)
    z  <- rep(TRUE,length(x))
    a  <- c(1,cumprod(d))
    
    for(i in 1:length(margin)) 
      {
        si  <-margin2[i]; ##print(si); print(d[si])
        idx2 <- rep(1:d[si],each=a[si],times=length(x)/(d[si]*a[si]))
        z <- z & level2[i]==idx2
      }
    
    dr<-d[(1:ld)[-margin2]]
  
    x[!z] <- impose
    return(x)
  } else {
    idx <- vector("list", length(dim(x)))
    idx[]<-TRUE
    idx[margin2] <- level2
    .Internal(do.call("[", c(list(x), idx), parent.frame()))
  }
}


## tableSlicePrim: Works only with margin and level being indices
tableSlicePrim <- function(x, margin, level){
  idx <- vector("list", length(dim(x)))
  idx[]<-TRUE
  idx[margin] <- level
  .Internal(do.call("[", c(list(x), idx), parent.frame()))
}




tablePerm <- function(a, perm, resize=TRUE){
  # Like aperm() but perm can be dimnames 
  if (missing(perm)){
    return(aperm(a,resize=resize))
  }
  
  if (is.character(perm)){
    perm <- match(perm,names(dimnames(a)))
    if (any(is.na(perm)))
      stop("Invalid permutation...")
  }
  ans<-aperm(a,perm, resize=resize)
  class(ans) <- oldClass(a)
  ans
}
