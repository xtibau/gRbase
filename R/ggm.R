##ggm.R --- 
##Author          : Claus Dethlefsen
##Created On      : Mon May 02 09:35:07 2005
##Last Modified By: 
##Last Modified On: 
##Update Count    : 0
##Status          : Unknown, Use with caution!
##

ggm <- function(formula=~.^1, gmData, marginal){
  value <- processFormula(formula,gmData, marginal,"Continuous")
  value$gmData <- gmData
  class(value) <- c("ggm","gModel")
  return(value)
}

fit.ggm <- function(m, ...){
  Ydf  <- observations(m$gmData)
  nobs <- nrow(Ydf)
  gc <- m$numformula
  Ymat <- as.matrix(Ydf)
  Smat   <- cov(Ymat)*(nobs-1)/nobs
  ipsFit <- ips(gc,Smat)
  fit      <- outfun( ipsFit$MLE, Smat,nrow(Ydf))
  fit$n    <- nobs
  fit$mean <- apply(Ymat,2,mean)
  fit$df   <- length(which(fit$part==0))/2
  fit$iterations <- ipsFit$iterations
  value<-m
  value$fit <- fit
  class(value) <- c("gRfit", "ggm",class(m))
  return(value)
}





## Partial correlation matrix ##
## computes partial correlation matrix for covariance matrix ##
partial.corr.matrix <- function(S){
  A <- solve(S)
  temp <- diag(1/sqrt(diag(A)))
  temp <- zapsmall(-temp%*%A%*%temp)
  diag(temp) <- 1
  return(temp)
}


## Output function ##
outfun <- function(Sigma, S, n){
  return(list(Sigma=round(Sigma,3),
              eigenvalues=eigen(Sigma)[[1]],
              correlation=cov2cor(Sigma),###corr.matrix(Sigma),
              partial.correlations=partial.corr.matrix(Sigma),
              loglik=ell(Sigma,S,n)))
}




## UNDIRECTED GRAPHS ###

## cliques must be a list with all cliques ##
## the components of this list must be ##
## vectors enumerating the vertices in a clique ##
ips <- function(cliques, S){
  if(!is.matrix(S)){
    return("Second argument is not a matrix!")
  }
  if(dim(S)[1]!=dim(S)[2]){
    return("Second argument is not a square matrix!")
  }
  if(min(eigen(S)[[1]])<=0){
    return("Second argument is not a positive definite matrix!")
  }
  start <- diag(diag(S)) # starting value
  p <- dim(S)[1] # dimensionality
  K <- solve(start)
  i <- 0
  if(length(cliques)==1){
    return(list(MLE=S, iterations=1))
  }
  my.complement <- function(C) return(setdiff(1:p,C))
  cliques.complements <- lapply(cliques, my.complement)
  repeat{
    K.old <- K
    i <- i+1
    for(j in 1:length(cliques)){
      C <- cliques[[j]]
      notC <- cliques.complements[[j]]
      K[C,C] <- solve( S[C,C] ) +
        K[C,notC]%*%solve(K[notC,notC])%*%K[notC,C]
    }
    if(sum(abs(K.old-K)) < 1e-10) break
  }
  return(list(MLE=solve(K), iterations=i))
}

