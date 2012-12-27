.dgCMatrix <- function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL,
                      sparse = TRUE, doDiag = TRUE, forceCheck = FALSE){
  as(Matrix(data=data, nrow=nrow, ncol=ncol, dimnames=dimnames, sparse=TRUE), "dgCMatrix")
}

.asdgCMatrix <- function(x){
  as(Matrix(x, sparse=TRUE), "dgCMatrix")
}
