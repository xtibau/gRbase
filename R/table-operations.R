tablePerm <- function(a, perm, resize=TRUE, keep.class=FALSE){
  # Like aperm() but perm can be dimnames
  if (missing(perm)){
    perm <- integer(0)
    #return(.Internal(aperm(a, perm, resize)))
    return(aperm.default(a, perm, resize))
  }

  if (is.character(perm)){
    perm <- match(perm,names(dimnames(a)))
    if (any(is.na(perm)))
      stop("Invalid permutation...")
  }
  #ans <- .Internal(aperm(a, perm, resize))
  ans <- aperm.default(a, perm, resize)
  if (keep.class){
      class(ans) <- oldClass(a)
  }
  ans
}

tableMult <- function(t1,t2){
  tableOp(t1,t2, op="*")
}

tableDiv <- function(t1,t2){
  tableOp(t1,t2, op="/")
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
  idx <- fmatch(vn2, vn1)

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
  ii    <- fmatch(vn2, vn.new)

  ## Create perumation indices; first variables in vn2; then the rest
  perm  <-  c(ii, (1:length(vn.new))[-ii])

  if (op == "*") {
    pot1 <- as.numeric(aperm.default(pot1, perm, TRUE)) * as.numeric(t2)
  }
  else {
    pot1 <- as.numeric(aperm.default(pot1, perm, TRUE)) / as.numeric(t2)
    pot1[!is.finite(pot1)] <- 0
  }
  dim(pot1)      <- di.new[perm]
  dimnames(pot1) <- dn.new[perm]
  class(pot1) <- "array"
  class(pot1) <- c("parray","array")
  pot1
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
  ii   <- fmatch(vn2, vn1)
  ## indices of vn2 in vn1 followed by indicies of remaining variables in vn1,
  ## so that vn2 varies fastest.
  perm <- c(ii, (1:length(vn1))[-ii])

  pot1 <-
    if (restore) {
      zz    <- op(aperm.default(t1, perm, TRUE), as.numeric(t2))
      newvn <- c(vn2, vn1[-ii])
      perm2 <- fmatch(vn1, newvn)
      aperm.default(zz, perm2, TRUE)
    } else {
      op(aperm.default(t1, perm, TRUE), as.numeric(t2))
    }
  if (identical(op, `/`))
    pot1[!is.finite(pot1)] <- 0
  pot1
}

tableSlice <-  function (x, margin, level, impose)
{
    if (is.null(margin))
        return(x)

    if (is.null(dimnames(x)))
        stop("tableSlice requires a structure with a dimnames attribute (e.g. array or a table) ")

    dn    <- dimnames(x)
    vn    <- names(dn)

    if (is.character(margin)){
        margin2 <- fmatch(margin, vn)
        if (any(is.na(margin2)))
            stop("Variables: ", margin[is.na(margin2)], " do not exist in table...")
    } else {
        margin2 <- margin
    }

    if (is.character(level)){
        level2  <- rep(NA, length(level))
        for (kk in seq_along(margin)){
            level2[kk] <- fmatch(level[kk],dn[[margin2[kk]]])
        }
        if (any(is.na(level2)))
            stop("Level: ", level[is.na(level2)], " do not exist in table...")
    } else {
        level2 <- level
    }

    if (!missing(impose) && is.numeric(impose)){
        di     <- dim(x)
        ld     <- length(di)
        zz     <- rep(TRUE,length(x))
        aprod  <- c(1,cumprod(di))

        for(ii in 1:length(margin))
        {
            si   <- margin2[ii];
            idx2 <- rep(1:di[si], each=aprod[si], times=length(x)/(di[si]*aprod[si]))
            zz   <- zz & level2[ii] == idx2
        }

        dr<-di[(1:ld)[-margin2]]

        x[!zz] <- impose
        ans <- x
    } else {
        idx          <- vector("list", length(dim(x)))
        idx[]        <- TRUE
        idx[margin2] <- level2
        ans <-do.call("[", c(list(x), idx))
    }

    ans <- array(ans, dim=sapply(dn[-margin2], length), dimnames=dn[-margin2])
    class(ans) <- c("parray","array")
    ans
}

## tableSlicePrim: Works only with margin and level being indices
tableSlicePrim <- function(x, margin, level){
  idx         <- vector("list", length(dim(x)))
  idx[]       <-TRUE
  idx[margin] <- level
  do.call("[", c(list(x), idx), parent.frame())
}

tableMargin <- function (x, margin, keep.class = FALSE)
{
##   cat("===== tableMargin =====\n")
##   print(as.data.frame.table(x))
##   print(margin)

    if (!is.array(x))
        stop("'x' is not an array")

    at <- attributes(x)
    di <- at[['dim']]
    dn <- at[['dimnames']]

    vn <- names(dn)
    oc <- oldClass(x)
    if (length(margin)) {
        if (is.character(margin)) {
          marg.idx <- fmatch(margin, vn)
          if (any(is.na(marg.idx)))
            stop("Variable not in table...\n")
        }
        else {
          marg.idx <- margin
        }
        rest.idx <- (seq_along(vn))[-marg.idx]
        nr <- prod(di[marg.idx])
        nc <- prod(di[rest.idx])
        
        z <- rowSumsPrim(
                         matrix(
                                aperm.default(x, c(rest.idx, marg.idx), TRUE),
                                nrow=nr, ncol=nc, byrow=TRUE))
        attributes(z) <- list(dim=di[marg.idx], dimnames=dn[marg.idx])

    } else {
      z <- sum(x)
      #dim(z) <- 1
    }
    if (keep.class)
        class(z) <- oc

    return(z)
}





