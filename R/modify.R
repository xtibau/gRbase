  
  ## #############################
  ## Modify methods 
  ## #############################
  
  if(!isGeneric("dropEdge")){
    if (is.function("dropEdge"))
      fun <- dropEdge
    else fun <- function(object,name.1,name.2) standardGeneric("dropEdge")
    setGeneric("dropEdge", fun)
  }
  
  
  
  if(!isGeneric("addEdge")){
    if (is.function("addEdge"))
      fun <- addEdge
    else fun <- function(object,name.1,name.2) standardGeneric("addEdge")
    setGeneric("addEdge", fun)
  }
  
  
  if(!isGeneric("dropVertex")){
    if (is.function("dropVertex"))
      fun <- dropVertex
    else fun <- function(object,name) standardGeneric("dropVertex")
    setGeneric("dropVertex", fun)
  }
  
  
  if(!isGeneric("addVertex")){
    if (is.function("addVertex"))
      fun <- addVertex
    else fun <- function(object,name) standardGeneric("addVertex")
    setGeneric("addVertex", fun)
  }
  
#}
