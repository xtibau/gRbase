## #######################################
##
## Søren Højsgaard, January, 2013
## Known issues: Should do one for standard matrices.
## FIXME: Add tests, .Rd files and vignette
## #######################################

asdgCMatrix <- function(object){
  UseMethod("asdgCMatrix")
}

asdgCMatrix.matrix <- function(object){
  res <- .Call("C_asdgCMatrix_st", object*1.0 
               ,package="gRbase"
               )
  dimnames(res) <- dimnames(object)
  res
}

asdgCMatrix.Matrix <- function(object){
  as(object, "dgCMatrix")
}

