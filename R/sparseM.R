## #######################################
##
## Interface to RcppEigen functions for setting and extracting
## elements in sparse matrices
##
## Søren Højsgaard, July, 2012
## Known issues: None
## FIXME: Add tests, .Rd files and vignette
## #######################################

sp_getXij <- function(XX_, ii_, jj_){
  .Call("C_getXij", XX_, ii_, jj_
        ,package="gRbase"
        )
}

sp_setXij1 <- function(XX_, ii_, jj_){
  .Call("C_setXij1", XX_, ii_, jj_
        ,package="gRbase"
        )
}

sp_getXtf <- function(XX_, TF_){
  .Call("C_getXtf", XX_, TF_
        ,package="gRbase"
        )
}

sp_getXM <- function(XX_, MM_){
  .Call("C_getXM", XX_, MM_
        ,package="gRbase"
        )
}


sp_setXtf1 <- function(XX_, TF_){
  .Call("C_setXtf1", XX_, TF_
        ,package="gRbase"
        )
}

sp_setXM1 <- function(XX_, MM_){
  .Call("C_setXM1", XX_, MM_
        ,package="gRbase"
        )
}

sp_getXi <- function(XX_, ii_){
  .Call("C_getXi", XX_, ii_
        ,package="gRbase"
        )
}

sp_getXj <- function(XX_, jj_){
  .Call("C_getXj", XX_, jj_
        ,package="gRbase"
        )
}

sp_moralize <- function(XX_){
  .Call("C_moralizeM", XX_
        ,package="gRbase"
        )
}

sp_topoSort <- function(XX_){
  .Call("C_topoSortM", XX_
        ,package="gRbase"
        )
}

sp_fromto <- function(XX_){ 
  .Call("C_fromtoM", XX_
        ,package="gRbase"
        )
}

st_fromto <- function(XX_){ 
  .Call("C_fromtoS", XX_
        ,package="gRbase"
        )
}



