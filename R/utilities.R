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
which.arr.ind <- function(m){
  d <- nrow(m)
  rr<-rep(1:d, d)
  cc<-rep(1:d, each=d)
  epp<- cbind(rr[m!=0], cc[m!=0])
  epp
}


## Codes a p x 2 matrix of characters or a list with pairs
## of characters into a vector of numbers.
pairs2num <- function(x, vn, sort=TRUE){
  if (is.null(x))
    return(NULL)

  if (inherits(x,"list"))
    x <- do.call(rbind,x)
  else {
    if (inherits(x,"character"))
      x <- matrix(x,nrow=1)
  }

  # From here x should be a p x 2 matrix

  dd <- dim(x)
  if (dd[1]==0){
      return(numeric(0))
  } else {
      if (sort){
          i     <- x[,2]< x[,1]
          c1    <- i+1
          c2    <- -1*(i-1) + 1
          x  <- cbind(
                      x[cbind(seq_along(c1),c1)],
                      x[cbind(seq_along(c2),c2)])
      }
      ans       <- charmatch(x,vn)
      dim(ans)  <- dim(x)
      ans       <- colSumsPrim(t(ans) * c(100000,1))
      ans
  }
}
