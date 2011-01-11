rowSumsPrim <- function(x){
  .Call("R_rowSums", x
	,PACKAGE="gRbase"
	)
}

colSumsPrim <- function(x){
  .Call("R_colSums", x
	,PACKAGE="gRbase"
	)
}

colProd <- function(vv, mm){
  .Call("R_colProd", vv, mm
	,PACKAGE="gRbase"
	)
}
