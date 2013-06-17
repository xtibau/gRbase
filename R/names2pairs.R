####################################################
####
#### Create all possible pairs from a vector
#### or all possible pairs combining one element
#### from each of two vectors
####
#### NOTICE: If y is not NULL then x and y must be disjoint
####
####################################################

names2pairs <- function(x, y=NULL, sort=TRUE, result="list"){
  result <- match.arg(result, c("list","matrix"))
  lenx <- length(x)
  leny <- length(y)
  if (leny==0){
    if (lenx==1){
      if (result=="matrix")
        return(matrix(nrow=0,ncol=2))
      else 
        return(list())
    } else {
      cc   <- combnPrim(1:length(x),2)
      ans  <- x[cc]
      dim(ans) <- dim(cc)		
      if (sort){
        idx <- ans[1,]>ans[2,]
        ans[1:2,idx] <- ans[2:1,idx]
      }
      if (result=="matrix")
        return(t.default(ans))
      else
        return(colmat2list(ans))
    }
  } else {
    ans <- cbind(rep(x, each=leny), rep(y, times=lenx))
    if (sort){
      idx <- ans[,1]>ans[,2]
      ans[idx, 1:2] <- ans[idx,2:1]
    }
    if (result=="matrix")
      return(ans)
    else
      rowmat2list(ans)	
  }
}


# x <-letters[1:6]
# y <-letters[8:14]

# system.time({for (ii in 1:10000) n2p2(x,y, sort=F)})
# system.time({for (ii in 1:10000) names2pairs(x,y, sort=F)})

# Rprof()
# system.time({for (ii in 1:50000) n2p2(x,y)})
# Rprof(NULL)
# summaryRprof()


# n2p1 <- names2pairs

# n2p1("a")
# n2p2("a")

# n2p1(c("k","a","b","c"))
# n2p2(c("k","a","b","c"))

# n2p1(c("k","a","b","c"), sort=F)
# n2p2(c("k","a","b","c"), sort=F)

# n2p1(c("k","a","b","c"), c("i"))
# n2p2(c("k","a","b","c"), c("i"))

# n2p1(c("k","a","b","c"), c("i"), sort=F)
# n2p2(c("k","a","b","c"), c("i"), sort=F)

# n2p1(c("k","a","b","c"), c("i","j"))
# n2p2(c("k","a","b","c"), c("i","j"))



## names2pairs <- function(x, y=NULL, sort=TRUE){
##   lenx  <- length(x)
##   leny  <- length(y)
##   if (lenx>1){
##     if (leny==0){
##       val <- .n2pxPrim(x, lenx)
##     } else {
##       val <- .n2pxyPrim(x,y,lenx,leny)
##     }
##   } else {
##     if (lenx==1){
##       if (leny==0){
##         val <- list()
##       } else {
##         val <- .n2pxyPrim(x,y,lenx,leny)
##       }
##     } else {
##       if (leny<=1){
##         val <- list()
##       } else {
##         val <- .n2pxPrim(x, lenx)
##       }
##     }
##   }    
  
##   if (sort)
##      val <- lapply(val, sort.int)
  
##   return(val) 
## }

## .n2pxyPrim <- function(x,y,lenx=length(x),leny=length(y)){
##   zzz <- cbind(rep(x, leny), rep(y, each=lenx))
##   val <- vector("list",nrow(zzz))
##   for (ii in 1:nrow(zzz))
##     val[[ii]] <- zzz[ii,] 
##   val
## }

## .n2pxPrim <- function(x,lenx=length(x)){
##   val <- vector("list", lenx*(lenx-1)/2)
##   k <- 1
##   for (ii in 1:(lenx-1)){
##     for (jj in (ii+1):lenx){
##       val[[k]] <- c(x[ii], x[jj])
##       k <- k+1
##     }
##   }  
##   val
## }




