
  ## ####################################################################
  ## S4 class for gModel -- graphical models
  ## ####################################################################
  setClass("gModel", representation(
                                    formula = "formula",
                                    gmData = "gmData"
                                    )
           )

  
  
  ## ####################################################################
  ## end S4 class for gModel -- graphical models
  ## ####################################################################
  
    ## get function - gmData
  if(!isGeneric("gmData")){
    if (is.function("gmData"))
      fun <- gmData
    else fun <- function(object) standardGeneric("gmData")
    setGeneric("gmData", fun)
  }
  

  ## get function - formula
  if(!isGeneric("Formula")){
    if (is.function("Formula"))
      fun <- Formula
    else fun <- function(object) standardGeneric("Formula")
    setGeneric("Formula", fun)
  }
  
  
  ## set function - formula
  
  setGeneric("Formula<-", function(x, value) standardGeneric("Formula<-"))



