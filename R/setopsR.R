## Faster versions of 'standard R functions'
##

subsetof <- function(x, y){
  all(.Internal(match( x, y, 0, NULL))>0)
}

subsetofList <- function(x,l){ 
  any(unlistPrim(lapply(l, function(y) subsetof(x,y))))
}





