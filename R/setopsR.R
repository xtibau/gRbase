subsetof <- function(g1, g2){
  all(.Internal(match( g1, g2, 0))>0)
}
