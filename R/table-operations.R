tablePerm <- function(a, perm, resize=TRUE, keep.class=FALSE){
  # Like aperm() but perm can be dimnames 
  if (missing(perm)){
    perm <- integer(0)
    return(.Internal(aperm(a, perm, resize)))
  }
  
  if (is.character(perm)){
    perm <- charmatch(perm,names(dimnames(a)))
    if (any(is.na(perm)))
      stop("Invalid permutation...")
  }
  ans <- .Internal(aperm(a, perm, resize))
  if (keep.class){
      class(ans) <- oldClass(a)
  }
  ans
}

## Alternative to tableOp
##
tableOp <- function(t1,t2,op="*"){

  if (!is.array(t1)) 
    stop("'t1' is not an array")
  if (!is.array(t2)) 
    stop("'t2' is not an array")

  di1 <- dim(t1)
  di2 <- dim(t2)
  dn1 <- dimnames(t1)
  dn2 <- dimnames(t2)
  vn1 <- names(dn1)
  vn2 <- names(dn2)  

  ## indices of those variables in vn2 which exist in vn1:
  ##idx <- charmatch(vn2,vn1) ## OLD
  idx <- .Internal(charmatch(vn2, vn1, NA_integer_))
  
                                        #print(vn1); print(vn2); print(idx)
  ## indices of those variables in vn2 which do not exist in vn1:
  idx.na <- is.na(idx) 
  
  if (any(idx.na)){
    ## If there are variables in vn2 which are not in vn1
    aug.vn <- vn2[idx.na] # Find those variables  
    aug.di <- di2[idx.na] # - and their levels
    aug.dn <- dn2[idx.na] # - and their dimnames

    ## Create new "augmented" table defined over (vn1, vn2\vn1)
    pot1      <- rep.int(as.numeric(t1), prod(aug.di))
    vn.new    <- c(vn1, aug.vn)
    di.new    <- c(di1, aug.di)
    dn.new    <- c(dn1, aug.dn)
    dim(pot1) <- di.new
    dimnames(pot1) <- dn.new
  } else {
    pot1   <- t1
    vn.new <- vn1
    di.new <- di1
    dn.new <- dn1
  }

  ## Find indices of vn2 in the new "augmented" table
                                        #ii    <-  charmatch(vn2, vn.new)
  ii    <- .Internal(charmatch(vn2, vn.new, NA_integer_))

  ## Create perumation indices; first variables in vn2; then the rest
  perm  <-  c(ii, (1:length(vn.new))[-ii])

  if (op == "*") {
    pot1 <- as.numeric(.Internal(aperm(pot1, perm, TRUE))) * as.numeric(t2)
  }
  else {
    pot1 <- as.numeric(.Internal(aperm(pot1, perm, TRUE))) / as.numeric(t2)
    pot1[!is.finite(pot1)] <- 0
  }
  dim(pot1)      <- di.new[perm]
  dimnames(pot1) <- dn.new[perm]
  return(pot1)
}



tableOp2 <- .tableOp2 <- function (t1, t2, op = `*`, restore = FALSE) 
{
  if (!is.array(t1)) 
    stop("'t1' is not an array")
  if (!is.array(t2)) 
    stop("'t2' is not an array")

  vn1  <- names(dimnames(t1))
  vn2  <- names(dimnames(t2))
  
  ## indices of vn2 in vn1:
  ii   <- charmatch(vn2, vn1)
  ## indices of vn2 in vn1 followed by indicies of remaining variables in vn1,
  ## so that vn2 varies fastest.
  perm <- c(ii, (1:length(vn1))[-ii]) 
  
  pot1 <-
    if (restore) {
      zz    <- op(.Internal(aperm(t1, perm, TRUE)), as.numeric(t2))
                                        # newvn <- c(vn2, setdiffPrim(vn1, vn2)) ## OLD
      newvn <- c(vn2, vn1[-ii]) 
                                        # perm2 <- charmatch(vn1, newvn) ## OLD
      perm2 <- .Internal(charmatch(vn1, newvn, NA_integer_))
      .Internal(aperm(zz, perm2, TRUE))
    }
    else {
      op(.Internal(aperm(t1, perm, TRUE)), as.numeric(t2))
    }
  if (identical(op, `/`)) 
    pot1[!is.finite(pot1)] <- 0
  pot1
}




tableMargin <-  function (x, margin, keep.class=FALSE) 
{
  if (!is.array(x)) 
    stop("'x' is not an array")
  
  di <- dim(x)
  dn <- dimnames(x)
  vn <- names(dn)
  oc <- oldClass(x)
  if (length(margin)) {
    if (is.character(margin)){
      marg.idx <- .Internal(charmatch(margin, vn, NA_integer_))
                                        #marg.idx <- charmatch(margin, vn)
      if (any(is.na(marg.idx)))
        stop("Variable not in table...\n")
    } else {
      marg.idx <- margin
    }
    
                                        #rest.idx <- (1:length(vn))[-marg.idx]
    rest.idx <- (seq_along(vn))[-marg.idx]

    nr <- prod(di[marg.idx])
    nc <- prod(di[rest.idx])

    z <- .Internal(rowSums(.Internal(matrix(.Internal(aperm(x, c(rest.idx, marg.idx), TRUE)), nr, nc, TRUE, NULL)), nr, nc, FALSE))
    ## This call is just short for
    ##     z  <- .Internal(aperm(x, c(rest.idx, marg.idx), TRUE))        
    ##     z  <- .Internal(matrix(z, nr, nc, TRUE, NULL))    
    ##     z  <- .Internal(rowSums(z, nr, nc, FALSE))

    dim(z)      <- di[marg.idx]
    dimnames(z) <- dn[marg.idx]
  }
  else
    return(sum(x))
  
  if (keep.class)
    class(z) <- oc
  return(z)
}



tableSlice <-  function (x, margin, level, impose) 
{
  if(is.null(margin)) return(x)

  dn <- dimnames(x)
  varnames <- names(dn)

  if (is.character(margin)){
    margin2 <- charmatch(margin, varnames)
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






## ## Multiply two arrays
## ##
## tableOp <- function(t1, t2, op = "*"){

##   ## Find permutation of variables in set2 such that those in set1 are either
##   ## to the far right or far left
##   ##
##   permidx <- function(set1, set2, direction="right"){
##     idx <- 1:length(set2)
##     i   <- matchPrim(set1, set2)
##     if (direction=="left") {
##       c(i,idx[-i])
##     } else {
##       c(idx[-i],i)
##     }    
##   }
  
##   if (!is.array(t1)) 
##     stop("'t1' is not an array")
##   if (!is.array(t2)) 
##     stop("'t2' is not an array")

##   levels1 <- dimnames(t1)
##   levels2 <- dimnames(t2)

##   if (is.null(levels1))
##     stop("'t1' does not have dimnames")
##   if (is.null(levels2))
##     stop("'t2' does not have dimnames")
  
##   vn1    <- names(levels1)
##   vn2    <- names(levels2)

##   lev1 <- unlistPrim(lapply(levels1,length))
##   lev2 <- unlistPrim(lapply(levels2,length))

##   idx       <- matchPrim(vn2, vn1)
##   if (any(is.na(idx))){
##     augnames  <- vn2[is.na(idx)]
##     auglevn   <- lev2[is.na(idx)]
##     auglevels <- levels2[is.na(idx)]
##     pot1      <- rep(as.numeric(t1), prod(auglevn))
##     vn1       <- c(vn1, augnames)
##     lev1      <- c(lev1, auglevn)
##     levels1   <- c(levels1, auglevels)
##     dim(pot1) <- lev1
##   } else {
##     pot1 <- t1
##   }
  
##   perm  <- permidx(set1=vn2, set2=vn1,"left")
##   if (op=="*"){
##     ##pot1   <- as.numeric(aperm(pot1, perm)) * as.numeric(t2)
##     pot1   <- as.numeric(.Internal(aperm(pot1, perm, TRUE))) * as.numeric(t2)
##   } else {
##     ##pot1   <- as.numeric(aperm(pot1, perm)) / as.numeric(t2)
##     pot1   <- as.numeric(.Internal(aperm(pot1, perm, TRUE))) / as.numeric(t2)
##     pot1[!is.finite(pot1)] <- 0
##   }
##   #.z <<- pot1
##   dim(pot1) <- lev1[perm]
##   dimnames(pot1) <- levels1[perm]
##   #attributes(pot1) <- list(dim=lev1[perm], dimnames=levels1[perm], class="ptable")
##   #.z2 <<- pot1

##   ## FIXME: Bruges det nuget sted, at der er permuteret rigigt? tror det ikke...
##   ##pot1 <- tablePerm(pot1, vn1)
##   #print(vn1); print(vn2)
  
##   pot1
## }



## .tableOp2 <- function(t1, t2, op=`*`, restore=FALSE){
##   vn1 <- names(dimnames(t1))
##   vn2 <- names(dimnames(t2))
##   newvn <- c(vn2, setdiffPrim(vn1,vn2))
##   perm  <- matchPrim(newvn, vn1)
##   pot1 <-
##     if (restore){
##       zz<- op(.Internal(aperm(t1, perm, TRUE)), as.numeric(t2))
##       perm2 <- matchPrim(vn1, newvn)
##       .Internal(aperm(zz, perm2, TRUE))      
##     } else { 
##       op(.Internal(aperm(t1, perm, TRUE)), as.numeric(t2))
##     }

##   if (identical(op, `/`))
##     pot1[!is.finite(pot1)] <- 0

##   pot1
## }




## Marginalize array onto margin
## FIXME: Remove this...
## tableMarginPrim <- function(t1, margin, normalize=FALSE){
##   if (missing(margin) || (length(margin)==1 && is.na(margin))){
##     return(sum(as.numeric(t1)))
##   }
##   vn    <- names(dimnames(t1))
##   idx   <- match(margin,vn)
##   x     <- apply(t1, idx, sum)
##   if (normalize)
##     x <- x/sum(x)
##   att           <- attributes(t1)
##   attributes(x) <- list(dim=att$dim[idx], dimnames=att$dimnames[idx], class="ptable")
##   x
## }

## 
##
## .tableSlice <-  function (x, margin, level, impose) 
## {

##   if(is.null(margin)) return(x)

##   dn <- dimnames(x)
##   varnames <- names(dn)

##   margin2 <- rep(NA, length(margin))
##   level2  <- rep(NA, length(level))
##   for (kk in seq_along(margin)){
##     mtmp2 <- mtmp <- margin[kk]
##     ltmp2 <- ltmp <- level[kk]
##     if (is.character(mtmp)) 
##       mtmp2 <- match(mtmp, varnames)
##     if (is.na(mtmp2))
##       stop("Variable ", mtmp, " does not exist in table")
##     if (is.character(ltmp)) 
##       ltmp2 <- match(ltmp, dn[[mtmp2]])
##     if (is.na(ltmp2))
##       stop("Level ", ltmp, " does not exist in table")
##     margin2[kk] <- mtmp2
##     level2[kk]  <- ltmp2  
##   }

##   margin <- margin2
##   level  <- level2

  
##   d  <- dim(x)
##   ld <- length(d)
##   z  <- rep(TRUE,length(x))
##   a  <- c(1,cumprod(d))

##   for(i in 1:length(margin)) 
##     {
##       si  <-margin[i]; #print(si)
##       idx2 <- rep(1:d[si],each=a[si],times=length(x)/(d[si]*a[si]))
##       z <- z & level[i]==idx2
##     }

##   dr<-d[(1:ld)[-margin]]
  
##   if (!missing(impose) && is.numeric(impose)){
##     x[!z] <- impose
##     return(x)
##   } else {
##     newdn <- dimnames(x)[-margin]
##     return(array(as.vector(x)[z],dr, dimnames=newdn))
##   }
## }


## ..tableMargin <-  function (x, margin) 
## {
##   if (!is.array(x)) 
##     stop("'x' is not an array")
##   if (length(margin)) {
##     varnames <- names(dimnames(x))
##     margin2 <- rep(NA, length(margin))
##     for (kk in seq_along(margin)){
##       mtmp2 <- mtmp <- margin[kk]
##       if (is.character(mtmp)) 
##         mtmp2 <- match(mtmp, varnames)
      
##       if (is.na(mtmp2))
##         stop("Variable ", mtmp, " does not exist in table")
##       margin2[kk] <- mtmp2
##     }

##     z <- apply(x, margin2, sum)
##     dim(z) <- dim(x)[margin2]
##     dimnames(z) <- dimnames(x)[margin2]
##   }
##   else
##     return(sum(x))
##   class(z) <- oldClass(x)
##   z
## }


## .tableMargin <-  function (x, margin) 
## {
##   if (!is.array(x)) 
##     stop("'x' is not an array")

##   if (length(margin)) {
##     if (is.character(margin)){
##       varnames <- names(dimnames(x))
##       margin2 <- match(margin, varnames)
##       if (any(is.na(margin2)))
##         stop("Variable not in table...\n")
##     } else {
##       margin2 <- margin
##     }
    
##     ##     z <- apply(x, margin2, sum)
##     ##     dim(z) <- dim(x)[margin2]
##     ##     dimnames(z) <- dimnames(x)[margin2]
##     z <- structure(apply(x, margin2, sum),
##                    dim=dim(x)[margin2], dimnames=dimnames(x)[margin2])

##   }
##   else
##     return(sum(x))
##   class(z) <- oldClass(x)
##   z
## }


## tablePerm <- function(a, perm, resize=TRUE){
##   # Like aperm() but perm can be dimnames 
##   if (missing(perm)){
##     return(aperm(a,resize=resize))
##   }
  
##   if (is.character(perm)){
##     perm <- match(perm,names(dimnames(a)))
##     if (any(is.na(perm)))
##       stop("Invalid permutation...")
##   }
##   ans<-aperm(a,perm, resize=resize)
##   class(ans) <- oldClass(a)
##   ans
## }


## FIXME: This one is way faster than the original tablePerm;
## especially when keep.class=FALSE
##
