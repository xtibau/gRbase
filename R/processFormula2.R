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
