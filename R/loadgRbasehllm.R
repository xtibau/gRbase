.Load.gRbase.hllm <- function() {

    ## ####################################################################
  ## S4 class for hllm
  ## ####################################################################
  
  setClass("hllm", contains="gModel")
           
  ## creator
  setMethod("initialize", "hllm", function(.Object,formula=.^1~1,gmData,marginal)
            {
              .Object@formula <- process.formula(formula,gmData,marginal,type="Discrete")$formula
              .Object@gmData  <- gmData
              .Object
            }
            )

    setMethod("gmData", "hllm", function(object) object@gmData)
    setMethod("Formula", "hllm", function(object) object@formula)
  setReplaceMethod("Formula", "hllm", function(x,value) {
    
    x@formula <- value
    x
  })

    setMethod("dropEdge", "hllm",
            function(object,name.1,name.2) {
              
              ## edit hllm formula
              form <- Formula(object)
              u <- as.UG(form)
              u[name.1,name.2] <- u[name.2,name.1] <- 0
              u.cl <- cliques(u)
              u.form <- paste(unlist(lapply(u.cl,paste,collapse=":")),collapse=" + ")
            form <- paste("~",u.form)
              #form <- paste(u.form,form[1],form[3])
              Formula(object) <- as.formula(form)
#            object@formula <- as.formula(form)
              new.object <- object
              
              if (extends(class(new.object),"hllmengine"))
                new.object <- fit(new.object)
              
              return(new.object)
            }
            )

    setMethod("addEdge", "hllm",
            function(object,name.1,name.2) {

              ## edit hllm formula
              form <- Formula(object)
              u <- as.UG(form)
              u[name.1,name.2] <- u[name.2,name.1] <- 1
              u.cl <- cliques(u)
              u.form <- paste(unlist(lapply(u.cl,paste,collapse=":")),collapse=" + ")
#              form <- paste(u.form,form[1],form[3])
            form <- paste("~",u.form)              
              Formula(object) <- as.formula(form)
#            object@formula <- as.formula(form)
              new.object <- object
              
              if (extends(class(new.object),"hllmengine"))
                new.object <- fit(new.object)
              return(new.object)
            })
  setMethod("dropVertex", "hllm",
            function(object,name) {
              ## edit hllm formula
              form <- Formula(object)
              u <- as.UG(form)
              idx <- match(name,colnames(u))
              u <- u[-idx,-idx] 
              u.cl <- cliques(u)
              u.form <- paste(unlist(lapply(u.cl,paste,collapse=":")),collapse=" + ")
#              form <- paste(u.form,form[1],form[3])
            form <- paste("~",u.form)              
              Formula(object) <- as.formula(form)
#            object@formula <- as.formula(form)
              new.object <- object
              
              if (extends(class(new.object),"hllmengine"))
                new.object <- fit(new.object)
              return(new.object)
            })

    setMethod("addVertex", "hllm",
            function(object,name) {
              ## edit hllm formula
              form <- Formula(object)
              u <- as.UG(form)
              u <- cbind(u,0)
              u <- rbind(u,0)
              rownames(u)[nrow(u)] <- name
              colnames(u)[ncol(u)] <- name
              u.cl <- cliques(u)
              u.form <- paste(unlist(lapply(u.cl,paste,collapse=":")),collapse=" + ")
#              form <- paste(u.form,form[1],form[3])
            form <- paste("~",u.form)
              Formula(object) <- as.formula(form)
#            object@formula <- as.formula(form)
              new.object <- object
              
              if (extends(class(new.object),"hllmengine"))
                new.object <- fit(new.object)
              return(new.object)
            })

  
  ## ####################################################################
  ## hllmTest  (adopted from CoCoObjects)
  ## ####################################################################

  setClass("hllmTestClass", representation(deviance = "numeric", 
                                           df = "numeric", p = "numeric"))

  ## ####################################################################
  ## end hllmTest  (adopted from CoCoObjects)
  ## ####################################################################

}


.Load.gRbase.hllmfit <- function() {
    ## ####################################################################
  ## hllm fit
  ## ####################################################################
  
  ## a virtual class for fit output in any format depending on the
  ## engine
  setClassUnion("hllmengine")
  
  if(!isGeneric("getFit")){
    if (is.function("getFit"))
      fun <- getFit
    else fun <- function(object,...) standardGeneric("getFit")
    setGeneric("getFit", fun)
  }
  
  setMethod("getFit","hllm",function(object) object@fit)

  if(!isGeneric("summary")){
    if (is.function("summary"))
      fun <- summary
    else fun <- function(object,...) standardGeneric("summary")
    setGeneric("summary", fun)
  }

setMethod("summary","hllm",function(object) summary(getFit(object)))

  
  ## test
  ## extends(class(currentObject),"hllmengine")
  
  
  if(!isGeneric("fit")){
    if (is.function("fit"))
      fun <- fit
    else fun <- function(object,...) standardGeneric("fit")
    setGeneric("fit", fun)
  }
  
  setMethod("fit", "hllm",
            function(object,engine="loglm",...) {
              obj <- as(object,paste("hllm",engine,sep=""))
              res <- fit(obj,...)
              res
            }
            )

  ## loglin engine
  setClass("hllmloglin", contains="hllm",representation(fit="list"))
  setIs("hllmloglin","hllmengine")
  setIs("hllmloglin","hllm")
  
  setAs("hllm","hllmloglin", function(from,to) {
    new("hllmloglin",Formula(from),gmData(from))
  }
        )
  setMethod("fit","hllmloglin", function(object,...) {
    
    rawdata <- observations(gmData(object))
    if (is.data.frame(rawdata)){
      rawdata <- xtabs(~., rawdata)
      ##rawdata <- as.data.frame(rawdata)
    }
    numform <- process.formula(Formula(object),gmData(object),type="Discrete")$numformula
    val <- loglin(rawdata, numform,...)
#    print(class(val))
    object@fit <- val
    object
  }
            )
  
  ## loglm engine
  setClass("hllmloglm", contains="hllm",representation(fit="loglm"))
  setIs("hllmloglm","hllmengine")
  setIs("hllmloglm","hllm")
  
  setAs("hllm","hllmloglm", function(from,to) {
    new("hllmloglm",Formula(from),gmData(from))
  }
        )
  setMethod("fit","hllmloglm", function(object,...) {
    require(MASS)
    
    rawdata <- observations(gmData(object))
    if (is.data.frame(rawdata)){
      rawdata <- xtabs(~., rawdata)
      ##rawdata <- as.data.frame(rawdata)
    }
    mimform <- process.formula(Formula(object),gmData(object),type="Discrete")$mimformula
    loglm.formula <- formula(paste("~",mimform))
    val <- loglm(loglm.formula, rawdata,...) 
    
    object@fit <- val
    object
  }
            )
  
  ## ####################################################################
  ## end hllm fit
  ## ####################################################################

}

.Load.gRbase.hllmodify <- function() {
    ## ####################################################################
  ## Editing hllm object 
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
  setMethod("modifyModel", signature(object = "hllm"), 
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
                ## message(paste("Should return an object with the edge from", 
                ## name.1, f(args$from.type), "to", name.2, f(args$to.type), 
                ## "deleted from the argument object"))
                
                new.object <- dropEdge(object,name.1,name.2)
              
              }
              else if (action == "addEdge") {
                ## message(paste("Should return an object with the edge from", 
                ## name.1, f(args$from.type), "to", name.2, f(args$to.type), 
                ## "added to the argument object"))

                new.object <- addEdge(object,name.1,name.2)
                
              }
              else if (action == "dropVertex") {
                ##  message(paste("Should return an object with the vertex", 
                ##  name, f(args$type), "deleted from the argument object"))
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
              else if (action == "addVertex") {
                ##  message(paste("Should return an object with the vertex", 
                ## name, f(args$type), args$index, "added to the argument object"))
                new.object <- addVertex(object,name)
              
                ## new.object <- subModifyModel(object, action = "add.interactions", 
                ##  modification = name, ...)
              }
              result <- list(object = new.object, FactorVertices = FactorVertices, 
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
  setMethod("testEdge", signature(object = "hllm"), 
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

              if (!extends(class(object),"hllmengine")) 
                object <- fit(object)
              object.small <- dropEdge(object,name.1,name.2)
              
              fit.big <- getFit(object)
              fit.small <- getFit(object.small)
              
              dev.big <- deviance(fit.big)
              df.big  <- fit.big$df
              
              dev.small <- deviance(fit.small)
              df.small  <- fit.small$df
              
              dev.diff <- -(dev.big - dev.small)
              df.diff  <- -(df.big  - df.small)

              return(new("hllmTestClass",
                  deviance = dev.diff,
                  df = df.diff,
                  p = 1-pchisq(dev.diff,df.diff)
                  ))
          })


  ## ####################################################################
  ## end Interface   gRbase <-> dynamicGraph
  ## ####################################################################
}
