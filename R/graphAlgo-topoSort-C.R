## #######################################
##
## Interface to RcppEigen functions topological sorting of
## adjacency matrix
##
## Søren Højsgaard, December, 2012
## Known issues: None
## FIXME: Add tests, .Rd files and vignette
## #######################################

topoSort_sp <- function(XX_){
  .Call("C_topoSort_sp", XX_ 
        ,package="gRbase"
        )
}

topoSort_st <- function(XX_){
  .Call("C_topoSort_sp", XX_ 
        ,package="gRbase"
        )
}
