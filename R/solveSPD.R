
solveSPD <- function(a){
  ##.Call("La_chol2inv",   .Call("La_chol", a, PACKAGE = "base"), ncol(a), PACKAGE = "base")
  ##chol2inv(chol(a))
  .Call("C_spdinv_arma", a , PACKAGE="gRbase"   )
}
