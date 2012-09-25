.dgCMatrix <- function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL,
                      sparse = TRUE, doDiag = TRUE, forceCheck = FALSE){
  as(Matrix(data=data, nrow=nrow, ncol=ncol, dimnames=dimnames, sparse=TRUE),"dgCMatrix")
}

.asdgCMatrix <- function(x){
  as(Matrix(x, sparse=TRUE),"dgCMatrix")
}


### rowmat2list and colmat2list:
### ----------------------------
## Turns a matrix into a list, either by row or by column.
## Notice: finding unique rows in a matrix can be speeded up
## this way.
rowmat2list <- function(ans){
  if (nrow(ans)==0)
    return(list())
  res <- vector("list", nrow(ans))
  for (ii in 1:nrow(ans)){
    res[[ii]] <- ans[ii,]
  }
  res
}

colmat2list <- function(ans){
  if (ncol(ans)==0)
    return(list())
  res <- vector("list", ncol(ans))
  for (ii in 1:ncol(ans)){
    res[[ii]] <- ans[,ii]
  }
  res
}

matrix2list <- function(x, byrow=TRUE){
  if (byrow)
    rowmat2list(x)
  else
    colmat2list(x)
}


# mm <- matrix(c(1,1,2,2,2,2,4,4,4),nc=3)
# rowmat2list(unique(mm))
# unique(rowmat2list(mm))
# system.time({for(ii in 1:1000) rowmat2list(unique(mm))})
# system.time({for(ii in 1:1000) unique(rowmat2list(mm))})
## The latter is much faster...


## lapplyMatch: same as but much faster than
## lapply(xlist, function(gg) match(gg, set))
## 
lapplyV2I <- lapplyMatch <- function(xlist, set){lapply(xlist, function(gg) match(gg, set))}
## lapplyV2I <- function(xlist, set){
## mm <- match(unlist(xlist), set)
## ll <- unlist(lapply(xlist, length))  
## bb <- rep.int(0L, length(xlist))  
## ee <- cumsum(ll)
## gtz <- ll>0
## #ee[ll==0L] <- 0 # Not necessary
## zz <- ll[gtz]
## bb[gtz] <- cumsum(c(1, zz[-length(zz)]))
## ans <- vector("list", length(xlist))
## for (ii in 1:length(xlist)){
## if(gtz[ii]>0L){
## 	ans[[ii]] <- mm[bb[ii]:ee[ii]]
## } else {
## 	ans[[ii]] <- integer(0)
## }
## }
## ans
## }


## lapplyI2C: same as but faster than
## lapply(xlist, function(x) set[x])
lapplyI2V <- function (xlist, set) {lapply(xlist, function(xx) set[xx])}

## mm <- unlist(xlist)
## ll <- unlist(lapply(xlist, length))  
## bb <- rep.int(0L, length(xlist))  
## ee <- cumsum(ll)
## gtz <- ll>0
## #ee[ll==0L] <- 0 # Not necessary
## zz <- ll[gtz]
## bb[gtz] <- cumsum(c(1, zz[-length(zz)]))
## ans <- vector("list", length(xlist))
## for (ii in 1:length(xlist)){
## if(gtz[ii]>0L){
## 	ans[[ii]] <- set[mm[bb[ii]:ee[ii]]]
## } else {
## 	ans[[ii]] <- character(0)
## }
## }
## ans
## }








##
## Calculate logL for N(0,\Sigma) model.
##
## Sigma = Covariance matrix parameter
## K     = Sigma inverse
## S     = sample covariance matrix
## n     = sample size
##
ell <- function(Sigma, S, n){

  shdet <- function(Sigma){
    prod(eigen(Sigma)[[1]])
  }
  p <- dim(S)[1]
  const <- -n*p/2*log(2*pi)
  return(const-n/2*log(shdet(Sigma))
         -n/2*sum(diag( solve(Sigma)%*%S )) )
}

ellK <- function (K, S, n)
{
    value <- (n/2) * (log(det(K)) - sum(rowSums(K * S)))
    return(value)
}

cov2pcor <- function(V){
  ans <- -cov2cor(solve(V))
  diag(ans) <- -diag(ans)
  ans
  }

conc2pcor <- function(K){
  ans <- -cov2cor(K)
  diag(ans)<-1
  ans
}

## Returns matrix n x 2 matrix with indices of non-zero
## entries in matrix m
## FIXME: which.arr.ind: Fails on sparse matrices!!
which.arr.ind<-function(m){
  nr  <- nrow(m)
  nc  <- ncol(m)
  rr <- rep.int(1:nr, nc)
  cc <- rep(1:nc, each=nr)
  cbind(rr[m!=0L], cc[m!=0L])
}


## Codes a p x 2 matrix of characters or a list with pairs
## of characters into a vector of numbers.

pairs2num <- function(x, vn, sort=TRUE){
  if (class(x)!="matrix"){
    if (is.null(x))
      return(NULL)

    if (inherits(x,"list"))
      x <- do.call(rbind,x)
    else {
      if (inherits(x,"character"))
        x <- matrix(x,nrow=1)
    }
  }
  # From here x should be a p x 2 matrix

  dd <- dim(x)
  if (dd[1L]==0){
      return(numeric(0))
  } else {
      if (sort){
          i     <- x[,2L]< x[,1L]
          c1    <- i+1L
          c2    <- -1L*(i-1L) + 1L
          x  <- cbind(
                      x[cbind(seq_along(c1),c1)],
                      x[cbind(seq_along(c2),c2)])          
        }
      ans       <- match(x,vn)
      dim(ans)  <- dim(x)
      colSumsPrim(t.default(ans) * c(100000,1))
      ## ans[,1L] <- ans[,1L] * 100000L
##       rowSumsPrim(ans)
    }
}

