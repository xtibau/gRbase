## #######################################
##
## Interface to RcppEigen function for triangulation (based on sparse adj. matrix)
##
## Søren Højsgaard, December, 2012
## Known issues: Should do one for standard matrices.
## FIXME: Add tests, .Rd files and vignette
## #######################################

triangulateMAT_spCpp <- function(XX_, LL_=rep(2,ncol(XX_))){
  res <- .Call("C_triangulate_sp", XX_, log(LL_), 0, 0
               ,package="gRbase"
               )
  dimnames(res) <- dimnames(XX_)
  res
}

triangulateMAT_stCpp <- function(XX_, LL_=rep(2,ncol(XX_))){
  as(triangulateMAT_spCpp(asdgCMatrix(XX_), LL_), "matrix")
  
}
