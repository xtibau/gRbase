processFormula <- function (formula, data, marginal, type = c("Discrete", "Continuous"), 
    v.sep = "*", g.sep = "+") 
{
    get.var.of.type <- function(type) {
        varNames(data)[varTypes(data) == type]
    }
    used.var <- get.var.of.type(type)


    if (!inherits(formula, "formula")) {
      formula <- list2rhsFormula(formula)
    }

    list.formula <- rhsFormula2list(formula)
    
    pow <- extract.power(formula)

    if (!is.numeric(pow)){
      
      ## Check if abbreviations are valid
      ##
      if (any(is.na(pmatch(unlist(list.formula), used.var, duplicates.ok=TRUE))))
        stop("An invalid variable specification has been found\n")        

      ## Replace abbreviations with variable names
      ##
      list.formula <-
        lapply(list.formula,
               function(x) {
                 i <- pmatch(x,used.var)
                 used.var[i] })

      formula     <- list2rhsFormula(list.formula)
      str.formula <- paste(deparse(formula[[2]]), collapse = "")
    } else {
      if (!missing(marginal)) {
        used.var <- intersect(marginal, used.var)
      }
      if (pow == -1) 
        str.formula <- paste(used.var, collapse = v.sep, sep = "")
      else {
        pow <- min(c(pow, length(used.var)))
        tmp <- selectOrder(used.var, pow)
        str.formula <- paste(unlist(lapply(tmp, paste, collapse = v.sep)), 
                      collapse = g.sep, sep = "")
      }          
      formula      <- formula(paste("~", str.formula, sep = ""))
      list.formula <- rhsFormula2list(formula)
    }
    

    num.formula <- lapply(list.formula, function(l) {
        match(l, used.var)
    })

    value <- list(formula = formula, str.formula = str.formula, num.formula = num.formula, 
        list.formula = list.formula, gmData = data, varnames = used.var)
    value
}















## .processFormula <- function(formula, data, marginal,
##                            type=c("Discrete","Continuous"), v.sep="*", g.sep="+"){
  
##   get.var.of.type <- function(type){varNames(data)[varTypes(data)==type]}

##   if (!inherits(formula,"formula")){
##     formula <- list2rhsFormula(formula)
##   }

  
##   used.var <- get.var.of.type(type)
##   pow <- extract.power(formula)
## #  print(pow); print(used.var)

##   if (is.numeric(pow)){
##     if (!missing(marginal)){
##       used.var <- intersect(marginal,used.var)
## #      print(used.var); print(marginal)

##     }
##     if (pow==-1)
##       mimf <- paste(used.var,collapse=v.sep,sep="")
##     else{
##       pow <- min(c(pow, length(used.var)))
##       tmp <- selectOrder(used.var, pow)
##       mimf <- paste(unlist(lapply(tmp, paste, collapse=v.sep)),collapse=g.sep,sep="")
      
##     }
##   } else {
##     mf    <- as.formula(formula)
##     mimf <-  paste(deparse(mf[[2]]), collapse="")
##   }
##   cat("mf:\n"); print(mf)
##   cat("mimf:\n"); print(mimf)
##   formula <- formula(paste("~",mimf,sep=""))

##   interactions <- strsplit(mimf,paste("\\",g.sep,sep=""))[[1]]
##   interactions <- gsub(" ","",interactions,fixed=TRUE)
##                                         #  interactions <- gsub(g.sep,"",interactions)
  
##   if (v.sep == "*") v.sep <- "[*|:]"
##   varformula <- strsplit(interactions, v.sep)
  
##   numformula   <- lapply(varformula, function(l){ match(l,used.var) })
  
##   value <- list(formula=formula, mimformula=mimf,
##                 numformula=numformula,
##                 varformula=varformula,
##                 gmData=data, varnames=used.var)
##   value
## }


