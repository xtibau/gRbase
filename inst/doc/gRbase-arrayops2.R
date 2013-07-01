### R code from vignette source 'gRbase-arrayops2.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRbase-arrayops2.Rnw:21-24
###################################################
require( gRbase )
prettyVersion <- packageDescription("gRbase")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: gRbase-arrayops2.Rnw:70-73
###################################################
library(gRbase)
options("width"=100)
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 3: gRbase-arrayops2.Rnw:93-95
###################################################
data(lizard)
lizard


###################################################
### code chunk number 4: gRbase-arrayops2.Rnw:101-105
###################################################
class(lizard)
is.array(lizard)
dim(lizard)
dimnames(lizard)


###################################################
### code chunk number 5: gRbase-arrayops2.Rnw:181-183
###################################################
T1.U <- tableMargin(lizard, c("species","height"))
T1.V <- tableMargin(lizard, c("diam","species"))


###################################################
### code chunk number 6: gRbase-arrayops2.Rnw:189-190
###################################################
T1.UV<-tableOp(T1.U, T1.V, op = "*")


###################################################
### code chunk number 7: gRbase-arrayops2.Rnw:196-197
###################################################
tableSlice(lizard, "species", "anoli")


###################################################
### code chunk number 8: gRbase-arrayops2.Rnw:203-204
###################################################
tablePerm(T1.UV, c("species","height","diam"))


###################################################
### code chunk number 9: gRbase-arrayops2.Rnw:222-225
###################################################
yn <- c('y','n')
T.U <- array(c(5,95,1,99), dim=c(2,2), dimnames=list("tub"=yn, "asia"=yn))
T.U <- parray(c("tub","asia"), levels=list(yn, yn), values=c(5,95,1,99))


###################################################
### code chunk number 10: gRbase-arrayops2.Rnw:239-243
###################################################
T.U <- parray(c("tub","asia"), levels=list(yn, yn),
              values=c(5,95,1,99), normalize="first")
T.V <- parray("asia", list(yn), values=c(1,99),
              normalize="all")


###################################################
### code chunk number 11: gRbase-arrayops2.Rnw:253-254
###################################################
T.all <- tableOp(T.U, T.V, op="*")


###################################################
### code chunk number 12: gRbase-arrayops2.Rnw:260-261
###################################################
T.W <- tableMargin(T.all, "tub")


###################################################
### code chunk number 13: gRbase-arrayops2.Rnw:267-268
###################################################
tableOp(T.all, T.W, op="/")


