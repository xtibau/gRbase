
stepwise <- function(object,...) UseMethod("stepwise")

stepwise.default <- function(object,...) return(step(getFit(object)))
