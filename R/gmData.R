##gmData.R --- 
##Author          : Claus Dethlefsen
##Created On      : Mon May 02 09:34:40 2005
##Last Modified By: 
##Last Modified On: 
##Update Count    : 0
##Status          : Unknown, Use with caution!
##

### Some generic functions

"latent.gmData" <- function(x){attr(x,"latent")}
"latent" <- function(x) UseMethod("latent")

"latent<-.gmData" <- function(tmp,value){attr(tmp,"latent")<-value; return(tmp)}
"latent<-" <- function(tmp,value) UseMethod("latent<-")

valueLabels.gmData<- function(x) attr(x,"valueLabels")
valueLabels       <- function(x) UseMethod("valueLabels")

"valueLabels<-.gmData"<- function(tmp,value){attr(tmp,"valueLabels")<-value; return(tmp)}
"valueLabels<-"       <- function(tmp,value) UseMethod("valueLabels<-")

observations.gmData <- function(x) attr(x,"observations")
observations    <- function(x) UseMethod("observations")
obs             <- function(x) UseMethod("observations")

"observations<-.gmData"<- function(tmp,value){attr(tmp,"observations")<-value; return(tmp)}
"observations<-"       <- function(tmp,value)UseMethod("observations<-")

## "description.gmData" <- function(x){attr(x,"description")}
## "description" <- function(x) UseMethod("description")

"description<-.gmData" <- function(tmp,value){attr(tmp,"description")<-value; return(tmp)}
"description<-" <- function(tmp,value) UseMethod("description<-")

"varTypes.gmData" <- function(x){x$varTypes}
"varTypes" <- function(x) UseMethod("varTypes")

"varTypes<-.gmData" <- function(tmp,value){ tmp$varTypes <-value; return(tmp)}
"varTypes<-" <- function(tmp,value) UseMethod("varTypes<-")

varNames.gmData <- function(x)as.vector(x$varNames)
varNames <- function(x)UseMethod("varNames")

"varNames<-.gmData" <- function(tmp,value){ tmp$varNames <-value; return(tmp)}
"varNames<-" <- function(tmp,value) UseMethod("varNames<-")

nLevels.gmData <- function(x)as.vector(x$nLevels)
nLevels <- function(x)UseMethod("nLevels")

"nLevels<-.gmData" <- function(tmp,value){ tmp$nLevels <-value; return(tmp)}
"nLevels<-" <- function(tmp,value) UseMethod("nLevels<-")


shortNames.gmData <- function(x)as.vector(x$shortNames)
shortNames <- function(x)UseMethod("shortNames")

"shortNames<-.gmData" <- function(tmp,value){ tmp$shortNames <-value; return(tmp)}
"shortNames<-" <- function(tmp,value) UseMethod("shortNames<-")

dataOrigin.gmData   <- function(x) attr(x,"dataOrigin")[1]
dataOrigin   <- function(x)UseMethod("dataOrigin")

"ordinal"           <- function(tmp) UseMethod("ordinal")
"ordinal<-"         <- function(tmp,value) UseMethod("ordinal<-")

"ordinal.gmData" <- function(tmp)attr(tmp,"ordinal")

"ordinal<-.gmData" <- function(tmp,value){
  varTypes(tmp)[match(value, varNames(tmp))]<-"Ordinal"
  return(tmp)}

"nominal"           <- function(tmp) UseMethod("nominal")
"nominal<-"         <- function(tmp,value) UseMethod("nominal<-")


"nominal.gmData" <- function(tmp){
  varNames(tmp)["Discrete"==varTypes(tmp)]
}

"nominal<-.gmData" <- function(tmp,value){
  varTypes(tmp)[match(value, varNames(tmp))]<-"Discrete"
  return(tmp)}






##################################################################################
as.gmData       <- function(from) UseMethod("as.gmData")
##################################################################################

print.gmData  <- function(x, ...){
  xx<-attr(x,"description")
  if (!is.null(xx))
    cat("Description:", xx, "\n")
  print.data.frame(x);
  ##cat("Data origin:     ", .dataOrigin(x),"\n")
  if (!is.null(latent(x)))
    cat ("Latent variables:", paste(latent(x),collapse=' '), "\n")
  if (!is.null(valueLabels(x)))
  cat("To see the values of the factors use the 'valueLabels' function\n")
  if (!is.null(observations(x)))
  cat("To see the data use the 'observations' function\n")
  return(invisible(x))
}

# summary.gmData  <- function(object, ...){
#   print(table(object$varTypes))
#   if (!is.null(observations(object))) {
#     cat("\nObservation summary:\n")
#     print(summary(obs(object)))
#   }
#   invisible(object)
# }



summary.gmData <- function(object, ...){
  print(object)
  mapply(function(xx,ll){
    cat("Factor:", ll, "\n Levels:", paste(xx,sep=' '),"\n")
  }, valueLabels(object),names(valueLabels(object)))
  return(invisible(object))

}






#### ##############################################################

# newgmData <- function(varNames,
#                    varTypes=rep(validVarTypes()[1],length(varNames)),
#                    nLevels=NA,
#                    latent=NA,
#                    valueLabels=NULL,
#                    observations=NULL,
#                    description=NA,
#                    shortNames=c(letters,LETTERS)
#                    ){
#   value <- data.frame(varNames, abbreviate(varNames,1),row.names=NULL)
  
#   names(value) <- c("varNames","shortNames")
#   value$varTypes <- factor(varTypes,levels=validVarTypes())
#   value$nLevels  <- nLevels

#   obsclass <- class(observations)
#   class(value) <- c("gmData","data.frame")

  
#   attr(value,"valueLabels")    <- valueLabels
#   attr(value,"latent")         <- latent
#   attr(value,"description")    <- description
#   attr(value,"observations")   <- observations
#   ##switch(class(data),
#   switch(obsclass,
#          "table"=     { attr(value,"dataOrigin")     <- "table"      },
#          "data.frame"={ attr(value,"dataOrigin")     <- "data.frame" },
#          NULL=        { attr(value,"dataOrigin")     <- "table"      })
#   return(value)
# }



newgmData <-
function (varNames,
          varTypes = rep(validVarTypes()[1], length(varNames)), 
          nLevels  = NULL,
          latent   = NULL,
          valueLabels  = NULL,
          observations = NULL, 
          description  = NULL,
          shortNames   = NULL) 
{

  cl <- match.call()
  
  .is.subset <- function(x,y){
    setequal(intersect(x,y),x)
  }

  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
  }

  ## Find good short names...
  ##
  if (is.null(shortNames)){
    nam <- varNames
    nama  <- abbreviate(nam,1)
    nc    <- nchar(nama)
      rest  <- setdiff(c(letters,LETTERS), nama[nc==1])
    if (length(which(nc>1)) <= length(rest))
      nama[nc>1]<- rest[1:length(which(nc>1))]    
  } else {
    nama <- shortNames
  }

  value <- data.frame(varNames, nama, row.names = NULL)
  names(value) <- c("varNames", "shortNames")

  ## Deal with abbreviated varTypes
  ##
  varTypes <- sapply(varTypes, .simpleCap)
  varTypes <- sapply(varTypes, match.arg, choices=validVarTypes(), several.ok = FALSE)

  value$varTypes <- factor(varTypes, levels = validVarTypes())
  discidx        <- which("Discrete"==varTypes | "Ordinal"==varTypes)
  aa             <- rep(NA, length(varNames))

  ## If valueLabels=c(1,2,3) turn into list(c(1,2,3))
  ##
  if (!is.null(valueLabels) & !is.list(valueLabels))
    valueLabels <- list(valueLabels)

  if (is.null(nLevels) & is.null(valueLabels)){
    ## If neither nLevels or valueLabels are given; make all
    ## categorical variables binary
    ##
    aa[discidx]   <-  2
    nLevels <- aa
  }
 
  if (!is.null(valueLabels)){
    ## If valueLabels are given, infer nLevels from these; recycle if necessary...
    ##
    if (!.is.subset(varNames[discidx], names(valueLabels))){
      vl <- rep(valueLabels, length(discidx))
      valueLabels <- vl[1:length(discidx)]
      names(valueLabels) <- varNames[discidx]
    }        
    uu            <- valueLabels[varNames[discidx]]
    uu            <- sapply(uu,length)
    aa[discidx]   <- uu
    value$nLevels <- aa
  } else {
    ## Use nLevels as given; recycle if necessary
    ## Infer valueLabels from these
    v <- nLevels[discidx]
    v <- v[!is.na(v)]
    if (length(v)==0)
      v <- 2
    v <- rep(v, length(discidx))
    v <- v[discidx]
    aa[discidx]   <-  v
    value$nLevels <- aa
    uu <- varNames[discidx]
    ##v  <<- v
    
    valueLabels <- mapply(function(nn,vv){paste(nn,1:vv,sep='')},uu,v, SIMPLIFY=FALSE)
    
  }


  class(value) <- c("gmData", "data.frame")
  attr(value, "valueLabels")  <- valueLabels
  attr(value, "latent")       <- latent
  attr(value, "description")  <- description
  attr(value, "observations") <- observations
  attr(value, "dataOrigin")   <- class(observations)
  
  obsclass <- class(observations)
  
  if (is.null(obsclass)){
    attr(value, "dataOrigin") <- NULL
  } else {
    if(is.element("table", obsclass))
      attr(value, "dataOrigin") <- c("table",setdiff(obsclass, "table"))
    else{
      if(is.element("data.frame", obsclass))
        attr(value, "dataOrigin") <- c("data.frame", setdiff(obsclass, "data.frame"))
      else
        attr(value, "dataOrigin") <- "other"
    }
  }
  
    
    
  return(value)
}







#     switch(class(observations), 
#     table = {
#         attr(value, "dataOrigin") <- "table"
#     }, data.frame = {
#         attr(value, "dataOrigin") <- "data.frame"
#     }, "NULL" = {
#         attr(value, "dataOrigin") <- "table"
#     })

validVarTypes <- function() {c("Discrete","Ordinal","Continuous")}


## ####################################################################
## Convert data.frame into gmData

as.gmData.data.frame <- function(from){
  fact   <- unlist(lapply(1:ncol(from), function(j)
                          is.factor(from[,j])))
  Types <- rep(validVarTypes()[3],length(fact))
  Types[fact] <- validVarTypes()[1]
  
  levels <- unlist(lapply(1:ncol(from),
                          function(j)
                          {
                            if(is.factor(from[,j]))
                              length(levels(from[,j]))
                            else NA}
                          )
                   )
  
  if (length(which(fact))>0){
    vallabels <- list()
    for (j in which(fact)){
      vallabels <- c(vallabels, list(levels(from[,j])))
    }
    names(vallabels) <- names(from[which(fact)])
  } else {
    vallabels <- list()
  }
  
  newgmData(
      varNames=names(from),
      varTypes=Types,
      nLevels=levels,
      valueLabels=vallabels,
      observations=from
 )
}



## ####################################################################
## Convert table into gmData

as.gmData.table <- function(from){
  counts <- as.vector(from)
  dn     <- dimnames(from)
  name   <- names(lapply(dn,function(x)names(x)))
  dim    <- unlist(lapply(dn,length))
  newgmData(
         varNames=name,
         varTypes=rep("Discrete",length(name)),
         nLevels=dim,
         valueLabels=dn,
         observations=from
         )
}
  

## ####################################################################
## Convert array into gmData

as.gmData.array <- function(from){
  res <- as.gmData(as.table(from))
  observations(res) <- from
  res
}

