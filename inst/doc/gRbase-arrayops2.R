### R code from vignette source 'gRbase-arrayops2.Rnw'

###################################################
### code chunk number 1: gRbase-arrayops2.Rnw:43-46
###################################################
library(gRbase)
options("width"=100)
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 2: gRbase-arrayops2.Rnw:66-68
###################################################
data(lizard)
lizard


###################################################
### code chunk number 3: gRbase-arrayops2.Rnw:74-78
###################################################
class(lizard)
is.array(lizard)
dim(lizard)
dimnames(lizard)


###################################################
### code chunk number 4: gRbase-arrayops2.Rnw:154-156
###################################################
T1.U <- tableMargin(lizard, c("species","height"))
T1.V <- tableMargin(lizard, c("diam","species"))


###################################################
### code chunk number 5: gRbase-arrayops2.Rnw:162-163
###################################################
T1.UV<-tableOp(T1.U, T1.V, op = "*")


###################################################
### code chunk number 6: gRbase-arrayops2.Rnw:169-170
###################################################
tableSlice(lizard, "species", "anoli")


###################################################
### code chunk number 7: gRbase-arrayops2.Rnw:176-177
###################################################
tablePerm(T1.UV, c("species","height","diam"))


###################################################
### code chunk number 8: gRbase-arrayops2.Rnw:195-198
###################################################
yn <- c('y','n')
T.U <- array(c(5,95,1,99), dim=c(2,2), dimnames=list("tub"=yn, "asia"=yn))
T.U <- parray(c("tub","asia"), levels=list(yn, yn), values=c(5,95,1,99))


###################################################
### code chunk number 9: gRbase-arrayops2.Rnw:212-216
###################################################
T.U <- parray(c("tub","asia"), levels=list(yn, yn),
              values=c(5,95,1,99), normalize="first")
T.V <- parray("asia", list(yn), values=c(1,99),
              normalize="all")


###################################################
### code chunk number 10: gRbase-arrayops2.Rnw:226-227
###################################################
T.all <- tableOp(T.U, T.V, op="*")


###################################################
### code chunk number 11: gRbase-arrayops2.Rnw:233-234
###################################################
T.W <- tableMargin(T.all, "tub")


###################################################
### code chunk number 12: gRbase-arrayops2.Rnw:240-241
###################################################
tableOp(T.all, T.W, op="/")


