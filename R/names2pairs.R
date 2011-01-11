
## If y is not NULL then x and y must be disjoint
##


names2pairs <- function(x, y=NULL, sort=TRUE){
  lenx  <- length(x)
  leny  <- length(y)
  if (lenx>1){
    if (leny==0){
      val <- .n2pxPrim(x, lenx)
    } else {
      val <- .n2pxyPrim(x,y,lenx,leny)
    }
  } else {
    if (lenx==1){
      if (leny==0){
        val <- list()
      } else {
        val <- .n2pxyPrim(x,y,lenx,leny)
      }
    } else {
      if (leny<=1){
        val <- list()
      } else {
        val <- .n2pxPrim(x, lenx)
      }
    }
  }    
  
  if (sort)
     val <- lapply(val, sort.int)
  
  return(val) 
}

.n2pxyPrim <- function(x,y,lenx=length(x),leny=length(y)){
  zzz <- cbind(rep(x, leny), rep(y, each=lenx))
  val <- vector("list",nrow(zzz))
  for (ii in 1:nrow(zzz))
    val[[ii]] <- zzz[ii,] 
  val
}

.n2pxPrim <- function(x,lenx=length(x)){
  val <- vector("list", lenx*(lenx-1)/2)
  k <- 1
  for (ii in 1:(lenx-1)){
    for (jj in (ii+1):lenx){
      val[[k]] <- c(x[ii], x[jj])
      k <- k+1
    }
  }  
  val
}





## names2pairs <- function(x, y=NULL, sort=TRUE){
##   lenx  <- length(x)
##   leny  <- length(y)
##   if (length(y)){
##     val   <- as.list(rep(NA, lenx*leny))
##     k <- 1
##     for (i in 1:lenx){
##       for (j in 1:leny){
##         val[[k]] <- c(x[i], y[j])
##         k <- k+1
##       }
##     }  
##   } else {
##     if (length(x)==1)
##       return(list(x))
##     val   <- as.list(rep(NA, lenx*(lenx-1)/2))
##     k <- 1
##     for (i in 1:(lenx-1)){
##       for (j in (i+1):lenx){
##         val[[k]] <- c(x[i], x[j])
##         k <- k+1
##       }
##     }  
##   }
##   if (sort)
##     val <- lapply(val, sort)
##   val 
## }


