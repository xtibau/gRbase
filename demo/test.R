#setwd("c:/R/grbasedevel/grbase/R")
#source("gmData.R")
#source("formulae.R")
#source("hllmclass.R")
#source("hllmfit.R")
#source("dynamichllm.R")

data(ksl,package="deal")
a <- ksl[,5:7]
aa <- as(a,"gmData")

b <- new("hllm",.^1~1,aa)

fit(b)
fit(b,engine="loglin")
dynamic.Graph(b)
