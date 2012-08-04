combnPrim <- function(x,m,simplify=TRUE){
  if (length(x)==1 && is.numeric(x))
    x <- seq(x)
  if (length(x) < m)
    stop("Error in combnPrim: n < m\n")
  NCAND <- length(x)
  NSEL  <- as.integer(m)
  NSET <- as.integer(choose(NCAND,NSEL))
  ANS  <- rep.int(0L, NSET*NSEL)
  res <- .C("combnC", NSEL, NCAND, NSET, ANS, DUP=FALSE
            ,PACKAGE="gRbase"
  )[[4]]

  if (simplify){
    matrix(x[res], nrow=NSEL, ncol=NSET)
  } else {
    res <- matrix(x[res], nrow=NSEL, ncol=NSET)
    res <- split(res, col(res))
    ## FIXME: This is *very* inefficient
    ## FIXME  - use colmat2list or something like that instead
    names(res) <- NULL
    res
  }
}
