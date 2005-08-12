## ygRbasedynamic.R
## Author          : Claus Dethlefsen
## Created On      : Fri Jan 28 11:40:07 2005
## Last Modified By: Claus Dethlefsen
## Last Modified On: Mon May 02 13:42:19 2005
## Update Count    : 35
## Status          : Unknown, Use with caution!
###############################################################################
setClass("gModelTestClass", representation(deviance = "numeric", 
                                           df = "numeric", p = "numeric"))


if (!isGeneric("label") && !isGeneric("label", where = 3)) {
  if (is.function("label"))
    fun <- label
  else
    fun <- function(object) standardGeneric("label")
  setGeneric("label", fun)
}
  
  setMethod("label", "gModelTestClass", function(object)
            format(object@p, digits = 4))
  
  if (!isGeneric("width") && !isGeneric("width", where = 3)) {
    if (is.function("width"))
      fun <- width
    else
      fun <- function(object) standardGeneric("width")
    setGeneric("width", fun)
  }
  
  setMethod("width", "gModelTestClass", function(object)
            round(2 + 5 * (1 - object@p)))
  


if (!isGeneric("dynamic.Graph")) {
  if (is.function("dynamic.Graph")) 
    fun <- dynamic.Graph
  else fun <- function(object, ...) standardGeneric("dynamic.Graph")
  setGeneric("dynamic.Graph", fun)
}
setMethod("dynamic.Graph", signature(object = "gModel"), 
          function(object, ...) {
            dynamic.gR.Graph(object, title="gRaphical model",...)
          })
setMethod("dynamic.Graph", signature(object = "gRfit"), 
          function(object, ...) {
            dynamic.gR.Graph(object, title="Fitted Model",...)
          })

    ## ####################################################################
  ## Editing gModel object 
  ## ####################################################################
  ## modifyModel is identical to the one from CoCo, except the
  ## class. Except also that subModifyModel is replaced with a method
  ## for each of the possibilities.
  if (!isGeneric("modifyModel")) {
    if (is.function("modifyModel")) 
      fun <- modifyModel
    else fun <- function(object, action, name, name.1, name.2, 
                         ...) standardGeneric("modifyModel")
    setGeneric("modifyModel", fun)
  }
  setMethod("modifyModel", signature(object = "gModel"), 
            function(object, action, name, name.1, name.2, ...) {
              args <- list(...)
              FactorVertices <- NULL
              FactorEdges <- NULL
              if (!is.null(args$Arguments$ArgBlocks)) 
                warning("Interface for Block-recursive models not implemented!!!")
              f <- function(type)
                if (is.null(type)) 
                  ""
                else paste("(", type, ")")
              if (action == "dropEdge") {
                
                new.object <- dropEdge(object,name.1,name.2)
              
              }
              else if (action == "addEdge") {

                new.object <- addEdge(object,name.1,name.2)
                
              }
              else if (action == "dropVertex") {
                if (!is.null(args$Arguments) &&
                    (args$index > 0)
                    && !is.null(args$Arguments$ArgFactorVertices) && 
                    !is.null(args$Arguments$ArgVertices)) {
                  x <- (args$Arguments$ArgFactorVertices)
                  factors <- lapply(x, function(i) i@vertex.indices)
                  types <- lapply(x, function(i) class(i))
                  factors <- lapply(factors, function(x) x[x != 
                                                           args$index])
                  if (!(is.null(factors))) {
                    result <- returnFactorVerticesAndEdges(args$Arguments$ArgVertices, 
                                                           factors, types)
                    FactorVertices <- result$FactorVertices
                    FactorEdges <- result$FactorEdges
                  }
                }
                
                new.object <- dropVertex(object,name)
              }
              else if (action == "addVertex") 
                new.object <- addVertex(object,name)
              result <- list(object = new.object,
                             FactorVertices = FactorVertices, 
                             FactorEdges = FactorEdges)
              return(result)
            })
  
  if (!isGeneric("testEdge")) {
    if (is.function("testEdge")) 
      fun <- testEdge
    else fun <- function(object, action, name.1, name.2, 
                         ...) standardGeneric("testEdge")
    setGeneric("testEdge", fun)
  }
  setMethod("testEdge", signature(object = "gModel"), 
            function(object, action, name.1, name.2, ...) {
              from.type <- args$from.type
              to.type <- args$to.type
              f <- function(type) if (is.null(type)) 
                ""
              else paste("(", type, ")")
              if (!is.null(args$Arguments$ArgBlocks) || (!is.null(args$Arguments$oriented) && 
                                                         args$Arguments$oriented)) {
                message <- paste("Test of the edge from", name.1, 
                                 "to", name.2, " is not implemented for causal models!!!")
                message(message)
                warning(message)
              }

              if (!inherits(object,"gRfit"))
                { 
                object <- fit(object)}

              object.small <- dropEdge(object,name.1,name.2)
              
              fit.big <- getFit(object)
              fit.small <- getFit(object.small)
              
              dev.big <- deviance(fit.big)
              df.big  <- fit.big$df
              
              dev.small <- deviance(fit.small)
              df.small  <- fit.small$df
              
              dev.diff <- -(dev.big - dev.small)
              df.diff  <- -(df.big  - df.small)

              return(new("gModelTestClass",
                  deviance = dev.diff,
                  df = df.diff,
                  p = 1-pchisq(dev.diff,df.diff)
                  ))
          })

## #################################################################
##
## Add and drop edges
##
## #################################################################

dropEdge <- function(object, name.1, name.2) UseMethod("dropEdge")
dropEdge.gModel <- 
          function(object,name.1,name.2) {
            
            ## cat("Drop:",name.1,name.2,"\n",sep=" ")              
            ## edit hllm formula
            form <- formula(object)
            listform <- readf(form[2])
            new.form <- delete.edge(listform,c(name.1,name.2))
              
            form <- paste("~",showf(new.form))
            formula(object) <- as.formula(form)

            if (inherits(object,"gRfit"))
              object <- fit(object)

            return(object)
          }


addEdge <- function(object, name.1, name.2) UseMethod("addEdge")
addEdge.gModel <- 
          function(object,name.1,name.2) {
            
            new.object <- object
            ## edit hllm formula
            form <- formula(object)
            listform <- readf(form[2])
            new.form <- add.edge(listform,c(name.1,name.2))
            form <- paste("~",showf(new.form))
            formula(new.object) <- as.formula(form)

            if (inherits(new.object,"gRfit"))
              new.object <- fit(new.object)
            
            return(new.object)
          }



dropVertex <- function(object, name) UseMethod("dropVertex")
dropVertex.gModel <- 
  function(object,name) {
            ## edit hllm formula
            form <- formula(object)
            listform <- readf(form[2])

            ## delete 'name' from generators
            new.form <- lapply(listform,setdiff,name)
            form <- paste("~",showf(new.form))
            formula(object) <- as.formula(form)
            
            if (inherits(object,"gRfit"))
              object <- fit(object)
            
            return(object)
          }


addVertex <- function(object, name) UseMethod("addVertex")
addVertex.gModel <- 
          function(object,name) {
            ## edit formula
            form <- formula(object)
            listform <- readf(form[2])
            listform[[length(listform)+1]] <- name
            form <- paste("~",showf(listform))
            formula(object) <- as.formula(form)
            if (inherits(object,"gRfit"))
              object <- fit(object)
            
            return(object)
            }
