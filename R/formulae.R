## ##########################################################
## ###### formulae #############
## ##########################################################
  
## Interface gRbase <-> ggm
##
## This should be replaced with an interface to gRaph, since ggm
## uses adjancancy-matrix representation and not lists as gRaph does.
## ####################################################################

as.UG <- function(gRformula) {
  ## gives adjacancy matrix using UG() from ggm-package
  
  require(ggm)
  strgRformula <- paste(gRformula[2])
  ## gRformulas use ":" in place of "*" from ggm
  strUGformula <- gsub(":","*",strgRformula)
  ## gRformulas has "formula~1" in place of "~formula" in ggm
  UGformula    <- formula(paste("~",strUGformula))
  
  u <- UG(UGformula)
  u
}

all.subsets <- function(x){
  if (length(x)==1)
    return(x)
  else {
    val <- x[1]
    for (i in 2:length(x)){
      v <- paste(val,x[i],sep='+')
      val <- c(val,x[i],v)
    }
    val <- strsplit(val,"\\+")
    return(val)
  }
}

select.order  <- function(x,order=2){
  v <- all.subsets(x)
  ##print(x); print(v); print(order)
  value <- v[lapply(v,length)==as.numeric(order)]
  return(value)
}

extract.power<-function(fff){
  mimf  <- paste(as.formula(fff))[2]
  mimf.split <- unlist(strsplit(mimf,""))
  if(length(grep("[a-z]", mimf))>0){
    pow <- mimf 
  } else {
    has.hat <- match("^",mimf.split)
    sub <- unlist(strsplit(mimf,"\\^"))
    ##print(sub)
    if (!is.na(has.hat)){
      pow <- ifelse (sub[2]==".", -1, as.numeric(sub[2]))
    }
    else {
      pow <- length(unlist(strsplit(sub,"\\.")))
    }
  }
  return(pow)
}



process.formula <- function(formula, data, marginal, type=c("Discrete","Continuous")){
  
  
  get.var.of.type <- function(type){varNames(data)[varTypes(data)==type]}
  
  used.var <- get.var.of.type(type)
  pow <- extract.power(formula)
  if (is.numeric(pow)){
    if (!missing(marginal)){
      ##      print(used.var); print(marginal)
      used.var <- intersect(marginal,used.var)
    }
    if (pow==-1)
      mimf <- paste(used.var,collapse=":")
    else{
      pow <- min(c(pow, length(used.var)))
      ##      print(used.var)
      tmp <- select.order(used.var, pow)
      ##      print(tmp)
      mimf <- paste(unlist(lapply(tmp, paste, collapse=":")),collapse=" + ")
      ##      cat("1\n");print(mimf)
      ##      formula <- formula(paste(mimf, "~1 "))
    }
  } else {
    mf    <- as.formula(formula)
    mimf  <- paste(mf)[2]
  }
#  formula <- formula(paste(mimf, "~1 "))
  formula <- formula(paste("~ ",mimf))
  interactions <- strsplit(mimf,"\\+")[[1]]
  interactions <- gsub(" +","",interactions)
  int.list <- strsplit(interactions, ":")
  gc1   <- lapply(int.list, function(l){ match(l,used.var) })
  
  value <- list(formula=formula, mimformula=mimf, numformula=gc1,
                gmData=data, varnames=used.var)
  ##  print(value)
  value
}

## ##########################################################
## ###### end formulae #############
## ##########################################################

