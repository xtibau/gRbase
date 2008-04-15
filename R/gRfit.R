##gRfit.R --- 
##Author          : Claus Dethlefsen
##Created On      : Mon May 02 09:35:51 2005
##Last Modified By: 
##Last Modified On: 
##Update Count    : 0
##Status          : Unknown, Use with caution!
##

fit <- function(m, ...) UseMethod("fit")

"getFit.gRfit" <- function(x){x$fit}
"getFit" <- function(x) UseMethod("getFit")

"getFit<-.gRfit" <- function(tmp,value){ tmp$fit <-value; return(tmp)}
"getFit<-" <- function(tmp,value) UseMethod("getFit<-")

print.gRfit <- function(x, ...){
  print.gModel(x)
  cat("Fit information (gRbase)\n")
  cat("   logL", deviance(getFit(x)), "df", x$fit$df,"\n")
}

summary.gRfit <- function(object,...)
  summary(getFit(object))


