## Faster versions of 'standard R functions'
##

subsetof <- function(x, y){
  all(.Internal(match( x, y, 0, NULL))>0)
}

subsetofList <- function(x,l){  ## Uses DEDs subsetof - faster than mine
  any(unlistPrim(lapply(l, function(y) subsetof(x,y))))
}


uniquePrim <- function(x)
  .Internal(unique(x, FALSE, FALSE))

setdiffPrim <- function (x, y) 
{
    x <- as.vector(x)
    y <- as.vector(y)
    uniquePrim(if (length(x) || length(y)) 
        x[match(x, y, 0) == 0]
    else x)
}

unlistPrim <- function(l, recursive=TRUE, use.names=TRUE)
  .Internal(unlist(l, recursive, use.names))
