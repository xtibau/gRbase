## #######################################
##
## Interface to RcppEigen function for triangulation (based on sparse adj. matrix)
##
## Søren Højsgaard, December, 2012
## Known issues: Should do one for standard matrices.
## FIXME: Add tests, .Rd files and vignette
## #######################################

mcsMAT_spCpp <- function(XX_, OO_=0:(ncol(XX_)-1) ){
  res <- .Call("C_mcsMAT_sp", XX_, OO_, 0, 0
               ,package="gRbase"
               )
  res
}

mcsMAT_stCpp <- function(XX_, OO_=0:(ncol(XX_)-1) ){
  .asdgCMatrix <- function(object){
    .Call("C_asdgCMatrix_st", object*1.0 
                 ,package="gRbase"
                 )
  }
  
  mcsMAT_spCpp(.asdgCMatrix(XX_), OO_)
}
