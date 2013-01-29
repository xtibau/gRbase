## #######################################
##
## Interface to RcppEigen functions topological sorting of
## adjacency matrix
##
## Søren Højsgaard, December, 2012
## Known issues: None
## FIXME: Add tests, .Rd files and vignette
## #######################################

topoSortMAT_spCpp <- function(XX_){
  .Call("C_topoSortMAT_sp", XX_ 
        ,package="gRbase"
        )
}

topoSortMAT_stCpp <- function(XX_){
  .Call("C_topoSortMAT_st", XX_ 
        ,package="gRbase"
        )
}
