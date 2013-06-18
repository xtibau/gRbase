
hllm <- function(formula = ~.^1,  gmData, marginal){
  stop("function 'hllm' from gRbase is defunct. Please use the gRim package for hierarchical log-linear models.")

##   value <- processFormula(formula, gmData, marginal,"Discrete")
##   value$gmData <- gmData
##   class(value) <- c("hllm","gModel")
##   return(value)
}

fit.hllm <- function(object,engine="loglm",...){
  stop("function 'fit.hllm' from gRbase is defunct. Please use the gRim package for hierarchical log-linear models.")
##   rawdata <- observations(object$gmData)
##   if (is.data.frame(rawdata)){
##     rawdata <- xtabs(~., rawdata)
##   }
##   value <- object
##   mimform <- processFormula(formula(object),gmData(object),type="Discrete")
##                                         #cat("processFormula done...\n")
##   switch(engine,
##          "loglm"={
##            loglm.formula <- mimform$formula
##            val <- loglm(loglm.formula, data=rawdata)
##            val$call$data <- rawdata
##            class(value) <- c("gRfit","loglm", class(object))
##          },
##          {stop("Only engine 'loglm' currently implemented...")
##         })
##   value$fit <- val  
##   return(value)
}

stepwise.hllm <-    function (object, ...)  {
  stop("function 'stepwise.hllm' from gRbase is defunct. Please use the gRim package for hierarchical log-linear models.")  
##   if (!exists("rawdata",envir=.GlobalEnv)&
##       !exists("loglm.formula",envir=.GlobalEnv)) {
##     assign("rawdata",observations(gmData(object)),.GlobalEnv)
##     assign("loglm.formula",formula(object),.GlobalEnv)
    
##     if (!inherits(object,"gRfit"))
##       object <- fit(object)
    
##     res <- step(getFit(object),...)
##     gRobj <- object
##     formula(gRobj) <- formula(res$formula)
##     getFit(gRobj) <- res
##     rm(rawdata,loglm.formula,envir=.GlobalEnv)
##     return(gRobj)
##   }
##   else
##     stop("You will have to move/rename rawdata and loglm.formula from .GlobalEnv\n")
}




# loglmSHD <- function (formula, data, subset, na.action, ...) 
# {
#     .call <- match.call()
#     if (missing(data) || inherits(data, "data.frame")) {
#         m <- match.call(expand.dots = FALSE)
#         m$... <- NULL
#         m[[1]] <- as.name("model.frame")
#         data <- eval.parent(m)
#         .formula <- as.formula(attr(data, "terms"))
#     }
#     else {
#       trms <- attr(data, "terms") <- terms(formula <- denumerate(formula))
#       .formula <- renumerate(as.formula(trms))
#     }
#     loglm1(formula, data, ..., .call = .call, .formula = .formula)
# }



## .fit.hllm <- function(m,engine="loglm",...){

##   rawdata <- observations(m$gmData)
##   if (is.data.frame(rawdata)){
##     rawdata <- xtabs(~., rawdata)
##   }
##   value <- m
##   mimform <- processFormula(formula(m),gmData(m),type="Discrete")
##   switch(engine,
##          "loglm"={
##            mimformula <- mimform$mimformula
##            loglm.formula <- formula(paste("~",mimformula))
##            ##val <- loglm(loglm.formula, rawdata)
##            val <- loglm(loglm.formula, data=rawdata)
##            val$call$data <- rawdata
##            class(value) <- c("gRfit","loglm", class(m))
##          },
##          {stop("Only engine 'loglm' currently implemented...")
##         })
##   value$fit <- val  
##   return(value)
## }










# ## example of how to extende the user menu in dynamicGraph
# UserMenus <- 
#   list(
#        MainUser =
#        list(label = "Stepwise",
#               command = function(object, ...)
#             stepwise(object,...)
#               )
#           )


# for use with dynamicGraph
##setOldClass("hllm")
##setIs("hllm","gModel")

