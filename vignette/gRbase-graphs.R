### R code from vignette source 'gRbase-graphs.Rnw'

###################################################
### code chunk number 1: gRbase-graphs.Rnw:23-26
###################################################
require( gRbase )
prettyVersion <- packageDescription("gRbase")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: gRbase-graphs.Rnw:44-48
###################################################
dir.create("fig")
oopt <- options()
options("digits"=4, "width"=80, "prompt"="R> ", "continue"="  ")
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 3: gRbase-graphs.Rnw:53-54
###################################################
library(gRbase)


###################################################
### code chunk number 4: gRbase-graphs.Rnw:127-132
###################################################
ug11 <- ug(~a:b:c + c:d + d:e + a:e + f:g)
ug11 <- ug(~a*b*c + c*d + d*e + a*e + f*g)
ug12 <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"))
ug13 <- ug(~a:b:c, ~c:d, ~d:e + a:e + f:g)
ug13 <- ug(~a*b*c, ~c*d, ~d*e + a*e + f*g)


###################################################
### code chunk number 5: gRbase-graphs.Rnw:136-137
###################################################
ug11


###################################################
### code chunk number 6: gRbase-graphs.Rnw:147-150
###################################################
ug11m <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="matrix")
ug12m <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"), 
            result="matrix")


###################################################
### code chunk number 7: gRbase-graphs.Rnw:154-155
###################################################
ug11m


###################################################
### code chunk number 8: gRbase-graphs.Rnw:160-163
###################################################
ug11M <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="Matrix")
ug12M <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"), 
            result="Matrix")


###################################################
### code chunk number 9: gRbase-graphs.Rnw:167-168
###################################################
ug11M


###################################################
### code chunk number 10: gRbase-graphs.Rnw:186-192
###################################################
dag11 <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f)
dag11 <- dag(~a + b*a + c*a*b + d*c*e + e*a + g*f)
dag12 <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"))
dag13 <- dag(~a, ~b:a,  ~c:a:b, ~d:c:e, ~e:a, ~g:f)
dag13 <- dag(~a, ~b*a,  ~c*a*b, ~d*c*e, ~e*a, ~g*f)


###################################################
### code chunk number 11: gRbase-graphs.Rnw:196-197
###################################################
dag11


###################################################
### code chunk number 12: gRbase-graphs.Rnw:209-212
###################################################
dag11m <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="matrix")
dag12m <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"), result="matrix")


###################################################
### code chunk number 13: gRbase-graphs.Rnw:216-217
###################################################
dag11m


###################################################
### code chunk number 14: gRbase-graphs.Rnw:220-223
###################################################
dag11M <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="Matrix")
dag12M <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"), result="Matrix")


###################################################
### code chunk number 15: gRbase-graphs.Rnw:227-228
###################################################
dag11M


###################################################
### code chunk number 16: gRbase-graphs.Rnw:242-246
###################################################
as(ug11,"matrix")
as(as(ug11,"matrix"),"dgCMatrix")
as(as(as(ug11,"matrix"),"dgCMatrix"),"graphNEL")
as(as(as(as(ug11,"matrix"),"Matrix"),"graphNEL"),"igraph")


###################################################
### code chunk number 17: gRbase-graphs.Rnw:255-258
###################################################
m <- matrix(1:4,nrow=2)
as(m, "Matrix")
as(m, "dgCMatrix")


###################################################
### code chunk number 18: gRbase-graphs.Rnw:268-269
###################################################
asdgCMatrix(m)


###################################################
### code chunk number 19: gRbase-graphs.Rnw:286-289
###################################################
par(mfrow=c(1,2))
plot(ug11)
plot(as(ug11m,"graphNEL"))


###################################################
### code chunk number 20: gRbase-graphs.Rnw:296-300 (eval = FALSE)
###################################################
## par(mfrow=c(1,2))
## library(sna)
## gplot(ug11m, label=colnames(ug11m),gmode="graph")
## gplot(dag11m, label=colnames(dag11m))


###################################################
### code chunk number 21: gRbase-graphs.Rnw:314-315
###################################################
apropos("^moralize\\.")


###################################################
### code chunk number 22: gRbase-graphs.Rnw:322-323
###################################################
dag11.mor <- moralize(dag11)


###################################################
### code chunk number 23: gRbase-graphs.Rnw:327-330
###################################################
par(mfrow=c(1,2))
plot(dag11)
plot(dag11.mor)


###################################################
### code chunk number 24: gRbase-graphs.Rnw:336-338
###################################################
moralize(dag11m)
moralize(dag11M) 


###################################################
### code chunk number 25: gRbase-graphs.Rnw:353-356
###################################################
topoSort(dag11)
topoSort(dag11m)
topoSort(dag11M)


###################################################
### code chunk number 26: gRbase-graphs.Rnw:364-365
###################################################
topoSort(dag(~a:b+b:c+c:a))


###################################################
### code chunk number 27: gRbase-graphs.Rnw:382-385
###################################################
str(getCliques(ug11))
str(getCliques(ug11m))
str(getCliques(ug11M))


###################################################
### code chunk number 28: gRbase-graphs.Rnw:392-393
###################################################
apropos("^mcs\\.")


###################################################
### code chunk number 29: gRbase-graphs.Rnw:402-405
###################################################
mcs(ug11)
mcs(ug11m)
mcs(ug11M)


###################################################
### code chunk number 30: gRbase-graphs.Rnw:410-414
###################################################
mcs(dag11.mor)
mcs(as(dag11.mor,"matrix"))
mcs(as(dag11.mor,"Matrix")) 
mcs(dag11) 


###################################################
### code chunk number 31: gRbase-graphs.Rnw:421-422
###################################################
apropos("^triangulate\\.")


###################################################
### code chunk number 32: gRbase-graphs.Rnw:428-431
###################################################
(tug11<-triangulate(ug11))
(tug11m<-triangulate(ug11m))
(tug11M<-triangulate(ug11M))


###################################################
### code chunk number 33: gRbase-graphs.Rnw:435-438
###################################################
par(mfrow=c(1,2))
plot(ug11)
plot(tug11)


###################################################
### code chunk number 34: gRbase-graphs.Rnw:446-447
###################################################
apropos("^rip\\.")


###################################################
### code chunk number 35: gRbase-graphs.Rnw:455-459
###################################################
rr <- rip(tug11)
rr
rr <- rip(tug11m)
rr <- rip(tug11M) 


###################################################
### code chunk number 36: gRbase-graphs.Rnw:463-464
###################################################
plot(rr)


###################################################
### code chunk number 37: gRbase-graphs.Rnw:500-502
###################################################
system.time({for (ii in 1:200) RBGL::maxClique(ug11)})     ## in RBGL
system.time({for (ii in 1:200) maxCliqueMAT(ug11m)}) ## in gRbase


###################################################
### code chunk number 38: gRbase-graphs.Rnw:509-511
###################################################
system.time({for (ii in 1:2000) ug11m[2,]})
system.time({for (ii in 1:2000) ug11M[2,]})


###################################################
### code chunk number 39: gRbase-graphs.Rnw:518-519
###################################################
system.time({for (ii in 1:2000) sp_getXj(ug11M,2)})


###################################################
### code chunk number 40: gRbase-graphs.Rnw:535-552
###################################################
V <- 1:100
M <- 1:10
## Sparse graph 
##
g1 <- randomGraph(V, M, 0.05)
length(edgeList(g1))
c(NEL=object.size(g1),
mat=object.size(as(g1, "matrix")),
Mat=object.size(as.adjMAT(g1, "Matrix")))

## More dense graph 
##
g1 <- randomGraph(V, M, 0.5)
length(edgeList(g1))
c(NEL=object.size(g1),
mat=object.size(as(g1, "matrix")),
Mat=object.size(as.adjMAT(g1, "Matrix")))


###################################################
### code chunk number 41: gRbase-graphs.Rnw:570-571
###################################################
args(querygraph)


###################################################
### code chunk number 42: gRbase-graphs.Rnw:824-826
###################################################
#rm(print.list)
options("width"=85)


