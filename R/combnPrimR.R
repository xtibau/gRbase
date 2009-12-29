combnPrim <- function(x,m,simplify=TRUE){
  if (length(x)==1 && is.numeric(x))
    x <- seq(x)
  if (length(x) < m)
    stop("Error in combnPrim: n < m\n")
  NCAND = as.integer(length(x))
  NSEL  = as.integer(m)
  NSET <- as.integer(choose(NCAND,NSEL))
  ANS  <- as.integer(rep.int(0, NSET*NSEL))
  res <- .C("combnC", NSEL, NCAND, NSET, ANS, DUP=FALSE
            ,PACKAGE="gRbase"
  )[[4]]
  res <- x[res]
  dim(res) <- c(NSEL, NSET)  
  if (!simplify){
    res <- split(res, col(res))
    names(res) <- NULL
  }
  res
}
