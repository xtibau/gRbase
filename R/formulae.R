## ##########################################################
## ###### formulae #############
## ##########################################################
  
## Interface gRbase <-> ggm
##
## This should be replaced with an interface to gRaph, since ggm
## uses adjancancy-matrix representation and not lists as gRaph does.
## ####################################################################


all.subsets <- function(x,g.sep="+"){
  if (length(x)==1)
    return(x)
  else {
    val <- x[1]
    for (i in 2:length(x)){
      v <- paste(val,x[i],sep=g.sep)
      val <- c(val,x[i],v)
    }
    val <- strsplit(val,paste("\\",g.sep,sep=""))
    return(val)
  }
}

selectOrder  <- function(x,order=2){
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



process.formula <- function(formula, data, marginal, type=c("Discrete","Continuous"),v.sep=":",g.sep="+"){
  
  get.var.of.type <- function(type){varNames(data)[varTypes(data)==type]}
  
  used.var <- get.var.of.type(type)
  pow <- extract.power(formula)
  if (is.numeric(pow)){
    if (!missing(marginal)){
      ##      print(used.var); print(marginal)
      used.var <- intersect(marginal,used.var)
    }
    if (pow==-1)
      mimf <- paste(used.var,collapse=v.sep,sep="")
    else{
      pow <- min(c(pow, length(used.var)))
      tmp <- selectOrder(used.var, pow)
      mimf <- paste(unlist(lapply(tmp, paste, collapse=v.sep)),collapse=g.sep,sep="")
    }
  } else {
    mf    <- as.formula(formula)
    mimf  <- paste(mf,sep="")[2]
  }
  formula <- formula(paste("~",mimf,sep=""))
  interactions <- strsplit(mimf,paste("\\",g.sep,sep=""))[[1]]
  interactions <- gsub(g.sep,"",interactions)
  int.list <- strsplit(interactions, v.sep)
  gc1   <- lapply(int.list, function(l){ match(l,used.var) })
  
  value <- list(formula=formula, mimformula=mimf, numformula=gc1,
                gmData=data, varnames=used.var)
  value
}

## ##########################################################
## ###### end formulae #############
## ##########################################################

