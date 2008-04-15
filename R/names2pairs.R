
## If y is not NULL then x and y must be disjoint
##
names2pairs <- function(x, y=NULL, sort=TRUE){
  lenx  <- length(x)
  leny  <- length(y)
  if (length(y)){
    val   <- as.list(rep(NA, lenx*leny))
    k <- 1
    for (i in 1:lenx){
      for (j in 1:leny){
        val[[k]] <- c(x[i], y[j])
        k <- k+1
      }
    }  
  } else {
    if (length(x)==1)
      return(list(x))
    val   <- as.list(rep(NA, lenx*(lenx-1)/2))
    k <- 1
    for (i in 1:(lenx-1)){
      for (j in (i+1):lenx){
        val[[k]] <- c(x[i], x[j])
        k <- k+1
      }
    }  
  }
  if (sort)
    val <- lapply(val, sort)
  val 
}
