##                              -*- Mode: Ess -*- 
##gModel.R --- 
##Author          : Claus Dethlefsen
##Created On      : Mon May 02 09:35:24 2005
##Last Modified By: 
##Last Modified On: 
##Update Count    : 0
##Status          : Unknown, Use with caution!
##


gModel <- function(formula, gmData){
  value <- list(formula=formula, gmData=gmData)
  class(value) <- "gModel"
  return(value)
}

"formula<-.gModel" <- function(tmp,value){tmp$formula<-value; return(tmp)}
"formula<-" <- function(tmp,value) UseMethod("formula<-")

"gmData.gModel" <- function(x){x$gmData}
"gmData" <- function(x) UseMethod("gmData")

"gmData<-.gModel" <- function(tmp,value){tmp$gmData<-value; return(tmp)}
"gmData<-" <- function(tmp,value) UseMethod("gmData<-")

print.gModel <- function(x, ...){
  cat("Model information (gRbase)\n")
  cat(" Class:   ", paste(class(x),collapse=' <- '),"\n")
  cat(" Formula: ", paste(paste(x$formula),collapse=''), "\n")
}

## necessary for use with dynamicGraph
## setOldClass("gModel")
