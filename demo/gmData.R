######################################################################
## examples:

## add new type of variable
oldtypes <- validVarTypes()
validVarTypes <- function() {
  c(oldtypes,"MyVarType")
}


new("gmData",1:3,"MyVarType")

mydata <- new("gmData",paste("x",1:6,sep=""),"Discrete",2,TRUE,list(x3=c("lab1","lab2")))

valueLabels(mydata)

data(iris)
mydata <- as(iris,"gmData")                      
mydata

observations(mydata) <- observations(mydata)[1:10,]

description(mydata)$mimName <- letters[1:nrow(description(mydata))]

varNames(mydata)
varTypes(mydata)
numberLevels(mydata)
valueLabels(mydata)

varTypes(mydata)[3]     <- "Discrete"
numberLevels(mydata)[3] <- 2


library(gRbase)
data(rats)
gmd.rats <- as(rats,"gmData")
data(HairEyeColor)
gmd.hec  <- as(HairEyeColor,"gmData")

gmd.rats.nodata  <-  new("gmData",
                         varNames=c("Sex","Drug","W1","W2"),
                         varTypes=c("Discrete","Discrete","Continuous","Continuous"),
                         numberLevels=c(2,3,NA,NA), 
                         valueLabels=list(Sex=c("M","F"), Drug=c("D1","D2","D3")))
observations(gmd.rats.nodata) <- rats
valueLabels(gmd.rats.nodata)$Sex <- c("Male","Female")

nVar <- nrow( description( gmd.rats.nodata ) )
description(gmd.rats.nodata)$shortName <-  letters[1:nVar]


