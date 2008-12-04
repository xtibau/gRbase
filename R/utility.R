


## GENERAL STUFF ##

## Log-likelihood ##
## Sigma = Covariance matrix parameter ##
## S = sample covariance matrix ##
## n = sample size ##

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
