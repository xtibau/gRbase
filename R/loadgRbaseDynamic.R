#.Load.gRbase.dynamic <- function() {
#  require(dynamicGraph)
  
  if (!isGeneric("label") && !isGeneric("label", where = 3)) {
    if (is.function("label"))
      fun <- label
    else
      fun <- function(object) standardGeneric("label")
    setGeneric("label", fun)
  }
  
  setMethod("label", "hllmTestClass", function(object)
            format(object@p, digits = 4))
  
  if (!isGeneric("width") && !isGeneric("width", where = 3)) {
    if (is.function("width"))
      fun <- width
    else
      fun <- function(object) standardGeneric("width")
    setGeneric("width", fun)
  }
  
  setMethod("width", "hllmTestClass", function(object)
            round(2 + 5 * (1 - object@p)))


  
#}

#.Load.dynamicgraph <- function() {
if (!isGeneric("dynamic.Graph")) {
  if (is.function("dynamic.Graph")) 
    fun <- dynamic.Graph
  else fun <- function(object, ...) standardGeneric("dynamic.Graph")
  setGeneric("dynamic.Graph", fun)
}
setMethod("dynamic.Graph", signature(object = "hllm"), 
          function(object, ...) {
            dynamic.gR.Graph(object, title="Hierarchical log-linear model",...)
          })
setMethod("dynamic.Graph", signature(object = "gmData"), 
          function(object, ...) {
            dynamic.gR.Graph(new("hllm",.^.~1,object), title="Hierarchical log-linear model",...)
          })
#    invisible()
#  }
