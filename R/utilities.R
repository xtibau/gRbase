

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
