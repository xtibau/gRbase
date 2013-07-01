### R code from vignette source 'gRbase-arrayops1.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRbase-arrayops1.Rnw:16-19
###################################################
require( gRbase )
prettyVersion <- packageDescription("gRbase")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: gRbase-arrayops1.Rnw:65-67
###################################################
options(prompt="R> ")
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 3: gRbase-arrayops1.Rnw:72-73
###################################################
library(gRbase)


###################################################
### code chunk number 4: gRbase-arrayops1.Rnw:103-122
###################################################
## 1-dimensional array
##
x1 <- 1:8
dim(x1) <- 8
x1
c(is.array(x1), is.matrix(x1))

## 2-dimensional array (matrix)
##
x2 <- 1:8
dim(x2) <- c(2,4)
x2
c(is.array(x2), is.matrix(x2))

## 3-dimensional array
##
x3 <- array(1:8, dim=c(2,2,2))
x3
c(is.array(x3), is.matrix(x3))


###################################################
### code chunk number 5: gRbase-arrayops1.Rnw:144-146
###################################################
adim2222 <- c(2,2,2,2)
adim2323 <- c(2,3,2,3)


###################################################
### code chunk number 6: gRbase-arrayops1.Rnw:158-162
###################################################
cell2entry(c(1,1,1,1), adim2222)
entry2cell(1, adim2222)
cell2entry(c(2,1,2,1), adim2222)
entry2cell(6, adim2222)


###################################################
### code chunk number 7: gRbase-arrayops1.Rnw:187-189
###################################################
nextCell(c(1,1,2,1), adim2222)
nextCell(c(2,2,2,1), adim2222)


###################################################
### code chunk number 8: gRbase-arrayops1.Rnw:201-203
###################################################
nextCellSlice(c(2,1,1,2),  sliceset=c(2), adim2323)
nextCellSlice(c(1,3,2,1),  sliceset=c(2,3), adim2323)


###################################################
### code chunk number 9: gRbase-arrayops1.Rnw:221-222
###################################################
(r1<-slice2entry(slicecell=c(1,2), sliceset=c(2,3), adim2222))


###################################################
### code chunk number 10: gRbase-arrayops1.Rnw:228-229
###################################################
do.call(rbind, lapply(r1, entry2cell, adim2222))


###################################################
### code chunk number 11: gRbase-arrayops1.Rnw:241-242
###################################################
(p<-permuteCellEntries(perm=c(2,1), adim=c(2,3)))


###################################################
### code chunk number 12: gRbase-arrayops1.Rnw:248-252
###################################################
(A <- array(11:16, dim=c(2,3)))
Ap <- A[p]
dim(Ap) <- c(3,2)
Ap


###################################################
### code chunk number 13: gRbase-arrayops1.Rnw:258-259
###################################################
aperm(A, c(2,1))


###################################################
### code chunk number 14: gRbase-arrayops1.Rnw:276-279
###################################################
ff <- factGrid(adim2222)
head(ff)
tail(ff)


###################################################
### code chunk number 15: gRbase-arrayops1.Rnw:285-287
###################################################
aa <- expand.grid(list(1:2,1:2,1:2,1:2))
head(aa)


###################################################
### code chunk number 16: gRbase-arrayops1.Rnw:293-294
###################################################
factGrid(adim2222, slicecell=c(1,2), sliceset=c(2,3))


