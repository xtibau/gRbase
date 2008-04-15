## Faster versions of 'standard R functions'
##

subsetof <- function(g1, g2){
  all(.Internal(match( g1, g2, 0))>0)
}

uniquePrim <- function(x)
  .Internal(unique(x, FALSE))

setdiffPrim <- function (x, y) 
{
    x <- as.vector(x)
    y <- as.vector(y)
    uniquePrim(if (length(x) || length(y)) 
        x[match(x, y, 0) == 0]
    else x)
}

unlistPrim <- function(x, recursive=TRUE, use.names=TRUE)
  .Internal(unlist(x, recursive, use.names))
