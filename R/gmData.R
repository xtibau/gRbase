## ##########################################################
## ###### gmData #############
## ##########################################################

## a virtual class for data in any format (or NULL)
setClassUnion("dataOrNULL", c("NULL","data.frame","table"))

##setClassUnion("displayOrNULL", c("NULL","dynamicDisplay"))

## Other datatypes may be added later:
## setIs("mydataclass", "dataOrNULL")
##
## existing may be inspected as
## getClass("dataOrNULL")

## ####################################################################
## CLASS: gmData. 
setClass('gmData', representation(description='data.frame',
                                  valueLabels='vector',
                                  observations='dataOrNULL'
                                  )
         )

## creator. Default: no data 
setMethod("initialize", "gmData", function(.Object,
                                           varNames=vector(),
                                           varTypes=
                                           rep(validVarTypes()[1],
                                               length(varNames)),
                                           numberLevels=NA,
                                           latent=FALSE,
                                           valueLabels=list(),
                                           observations=NULL) {
  
  .Object@description              <- data.frame(I(varNames))
  .Object@description$numberLevels <- numberLevels
  .Object@description$latent       <- latent
  .Object@description$varTypes <- factor(varTypes,levels=validVarTypes())
  
  ## TODO: create numberLevels=2 for discrete variables
  ## TODO: create valueLabels for discrete variables
  
  .Object@valueLabels   <- valueLabels
  .Object@observations  <- observations
  
  .Object
}
          )

## note that 'description' can be extended by further information
## about the variables.
##
## description(mydata)$mimName <- letters[1:nrow(description(mydata))]

## ####################################################################
## get/set methods

## description

if (!isGeneric("description")) {
  if (is.function("description")) 
    fun <- description
  else fun <- function(x) standardGeneric("description")
  setGeneric("description", fun)
}
setMethod("description","gmData",function(x) x@description)

setGeneric("description<-", function(x, value) standardGeneric("description<-"))
setReplaceMethod("description", "gmData", function(x, value){
  x@description <- value
  x
})

## get/set variable properties

## varTypes

if (!isGeneric("varTypes")) {
  if (is.function("varTypes")) 
    fun <- varTypes
  else fun <- function(x) standardGeneric("varTypes")
  setGeneric("varTypes", fun)
}
setMethod("varTypes","gmData",function(x) description(x)$varTypes)

setGeneric("varTypes<-", function(x, value) standardGeneric("varTypes<-"))
setReplaceMethod("varTypes", "gmData", function(x, value){
  tmp <- 
    description(x)$varTypes <- value
  x
})

## varNames

if (!isGeneric("varNames")) {
  if (is.function("varNames")) 
    fun <- varNames
  else fun <- function(x) standardGeneric("varNames")
  setGeneric("varNames", fun)
}
setMethod("varNames","gmData",function(x) description(x)$varNames)

setGeneric("varNames<-", function(x, value) standardGeneric("varNames<-"))
setReplaceMethod("varNames", "gmData", function(x, value){
  description(x)$varNames <- value
  x
})

## numberLevels

if (!isGeneric("numberLevels")) {
  if (is.function("numberLevels")) 
    fun <- numberLevels
  else fun <- function(x) standardGeneric("numberLevels")
  setGeneric("numberLevels", fun)
}
setMethod("numberLevels","gmData",function(x) description(x)$numberLevels)

setGeneric("numberLevels<-", function(x, value) standardGeneric("numberLevels<-"))
setReplaceMethod("numberLevels", "gmData", function(x, value){
  description(x)$numberLevels <- value
  x
})

## latent

if (!isGeneric("latent")) {
  if (is.function("latent")) 
    fun <- latent
  else fun <- function(x) standardGeneric("latent")
  setGeneric("latent", fun)
}
setMethod("latent","gmData",function(x) description(x)$latent)

setGeneric("latent<-", function(x, value) standardGeneric("latent<-"))
setReplaceMethod("latent", "gmData", function(x, value){
  description(x)$latent <- value
  x
})

## valueLabels

if (!isGeneric("valueLabels")) {
  if (is.function("valueLabels")) 
    fun <- valueLabels
  else fun <- function(x) standardGeneric("valueLabels")
  setGeneric("valueLabels", fun)
}
setMethod("valueLabels","gmData",function(x) x@valueLabels)

setGeneric("valueLabels<-", function(x, value) standardGeneric("valueLabels<-"))
setReplaceMethod("valueLabels", "gmData", function(x, value){
  x@valueLabels <- value
  x
})


## observations

if (!isGeneric("observations")) {
  if (is.function("observations")) 
    fun <- observations
  else fun <- function(x) standardGeneric("observations")
  setGeneric("observations", fun)
}
setMethod("observations","gmData",function(x) x@observations)

setGeneric("observations<-", function(x, value) standardGeneric("observations<-"))
setReplaceMethod("observations", "gmData", function(x, value){
  x@observations <- value
  
  ## How do we ensure that varNames that are not latent can be
  ## found in data (should we?)?
  
  x
})



setMethod("show","gmData", function(object) {
  cat("Description:\n")
  show(description(object))
  cat("To see the values of the factors use the 'valueLabels' function\n")
  cat("To see the data use the 'observations' function\n")
}
          )

## ####################################################################
## Convert data.frame into gmData

setAs("data.frame","gmData", function(from,to) {
  
  fact   <- unlist(lapply(1:ncol(from), function(j)
                          is.factor(from[,j])))
  Types <- rep(validVarTypes()[3],length(fact))
  Types[fact] <- validVarTypes()[1]
  
  levels <- unlist(lapply(1:ncol(from),
                          function(j)
                          {
                            if(is.factor(from[,j]))
                              length(levels(from[,j]))
                            else NA}
                          )
                   )
  
  if (length(which(fact))>0){
    vallabels <- list()
    for (j in which(fact)){
      vallabels <- c(vallabels, list(levels(from[,j])))
    }
    names(vallabels) <- names(from[which(fact)])
  } else {
    vallabels <- list()
  }
  
  new("gmData",
      varNames=names(from),
      varTypes=Types,
      numberLevels=levels,
      valueLabels=vallabels,
      observations=from
      )
}
      )

## ####################################################################
## Convert table into gmData

setAs("table","gmData", function(from,to) {
  counts <- as.vector(from)
  dn     <- dimnames(from)
  name   <- names(lapply(dn,function(x)names(x)))
  dim    <- unlist(lapply(dn,length))
  new("gmData",
      varNames=name,
      varTypes="Discrete",
      numberLevels=dim,
      valueLabels=dn,
      observations=from
      )
}
      )

## ####################################################################
## Convert array into gmData
## example of adding a data-type

setIs("array", "dataOrNULL")

setAs("array","gmData", function(from,to) {
  res <- as(as.table(from),"gmData")
  observations(res) <- from
  res
}
      )

## ##########################################################
##  ###### end gmData #############
## ##########################################################

