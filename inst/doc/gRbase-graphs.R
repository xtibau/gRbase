### R code from vignette source 'gRbase-graphs.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRbase-graphs.Rnw:23-26
###################################################
require( gRbase )
prettyVersion <- packageDescription("gRbase")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: gRbase-graphs.Rnw:70-74
###################################################
dir.create("fig")
oopt <- options()
options("digits"=4, "width"=80, "prompt"="R> ", "continue"="  ")
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 3: gRbase-graphs.Rnw:79-80
###################################################
library(gRbase)


###################################################
### code chunk number 4: gRbase-graphs.Rnw:153-158
###################################################
ug11 <- ug(~a:b:c + c:d + d:e + a:e + f:g)
ug11 <- ug(~a*b*c + c*d + d*e + a*e + f*g)
ug12 <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"))
ug13 <- ug(~a:b:c, ~c:d, ~d:e + a:e + f:g)
ug13 <- ug(~a*b*c, ~c*d, ~d*e + a*e + f*g)


###################################################
### code chunk number 5: gRbase-graphs.Rnw:162-163
###################################################
ug11


###################################################
### code chunk number 6: gRbase-graphs.Rnw:173-176
###################################################
ug11m <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="matrix")
ug12m <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"), 
            result="matrix")


###################################################
### code chunk number 7: gRbase-graphs.Rnw:180-181
###################################################
ug11m


###################################################
### code chunk number 8: gRbase-graphs.Rnw:186-189
###################################################
ug11M <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="Matrix")
ug12M <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"), 
            result="Matrix")


###################################################
### code chunk number 9: gRbase-graphs.Rnw:193-194
###################################################
ug11M


###################################################
### code chunk number 10: gRbase-graphs.Rnw:212-218
###################################################
dag11 <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f)
dag11 <- dag(~a + b*a + c*a*b + d*c*e + e*a + g*f)
dag12 <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"))
dag13 <- dag(~a, ~b:a,  ~c:a:b, ~d:c:e, ~e:a, ~g:f)
dag13 <- dag(~a, ~b*a,  ~c*a*b, ~d*c*e, ~e*a, ~g*f)


###################################################
### code chunk number 11: gRbase-graphs.Rnw:222-223
###################################################
dag11


###################################################
### code chunk number 12: gRbase-graphs.Rnw:235-238
###################################################
dag11m <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="matrix")
dag12m <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"), result="matrix")


###################################################
### code chunk number 13: gRbase-graphs.Rnw:242-243
###################################################
dag11m


###################################################
### code chunk number 14: gRbase-graphs.Rnw:246-249
###################################################
dag11M <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="Matrix")
dag12M <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"), result="Matrix")


###################################################
### code chunk number 15: gRbase-graphs.Rnw:253-254
###################################################
dag11M


###################################################
### code chunk number 16: gRbase-graphs.Rnw:268-272
###################################################
as(ug11,"matrix")
as(as(ug11,"matrix"),"dgCMatrix")
as(as(as(ug11,"matrix"),"dgCMatrix"),"graphNEL")
as(as(as(as(ug11,"matrix"),"Matrix"),"graphNEL"),"igraph")


###################################################
### code chunk number 17: gRbase-graphs.Rnw:281-284
###################################################
m <- matrix(1:4,nrow=2)
as(m, "Matrix")
as(m, "dgCMatrix")


###################################################
### code chunk number 18: gRbase-graphs.Rnw:294-295
###################################################
asdgCMatrix(m)


###################################################
### code chunk number 19: gRbase-graphs.Rnw:312-315
###################################################
par(mfrow=c(1,2))
plot(ug11)
plot(as(ug11m,"graphNEL"))


###################################################
### code chunk number 20: gRbase-graphs.Rnw:322-326 (eval = FALSE)
###################################################
## par(mfrow=c(1,2))
## library(sna)
## gplot(ug11m, label=colnames(ug11m),gmode="graph")
## gplot(dag11m, label=colnames(dag11m))


###################################################
### code chunk number 21: gRbase-graphs.Rnw:340-341
###################################################
apropos("^moralize\\.")


###################################################
### code chunk number 22: gRbase-graphs.Rnw:348-349
###################################################
dag11.mor <- moralize(dag11)


###################################################
### code chunk number 23: gRbase-graphs.Rnw:353-356
###################################################
par(mfrow=c(1,2))
plot(dag11)
plot(dag11.mor)


###################################################
### code chunk number 24: gRbase-graphs.Rnw:362-364
###################################################
moralize(dag11m)
moralize(dag11M) 


###################################################
### code chunk number 25: gRbase-graphs.Rnw:379-382
###################################################
topoSort(dag11)
topoSort(dag11m)
topoSort(dag11M)


###################################################
### code chunk number 26: gRbase-graphs.Rnw:390-391
###################################################
topoSort(dag(~a:b+b:c+c:a))


###################################################
### code chunk number 27: gRbase-graphs.Rnw:408-411
###################################################
str(getCliques(ug11))
str(getCliques(ug11m))
str(getCliques(ug11M))


###################################################
### code chunk number 28: gRbase-graphs.Rnw:418-419
###################################################
apropos("^mcs\\.")


###################################################
### code chunk number 29: gRbase-graphs.Rnw:428-431
###################################################
mcs(ug11)
mcs(ug11m)
mcs(ug11M)


###################################################
### code chunk number 30: gRbase-graphs.Rnw:436-440
###################################################
mcs(dag11.mor)
mcs(as(dag11.mor,"matrix"))
mcs(as(dag11.mor,"Matrix")) 
mcs(dag11) 


###################################################
### code chunk number 31: gRbase-graphs.Rnw:447-448
###################################################
apropos("^triangulate\\.")


###################################################
### code chunk number 32: gRbase-graphs.Rnw:454-457
###################################################
(tug11<-triangulate(ug11))
(tug11m<-triangulate(ug11m))
(tug11M<-triangulate(ug11M))


###################################################
### code chunk number 33: gRbase-graphs.Rnw:461-464
###################################################
par(mfrow=c(1,2))
plot(ug11)
plot(tug11)


###################################################
### code chunk number 34: gRbase-graphs.Rnw:472-473
###################################################
apropos("^rip\\.")


###################################################
### code chunk number 35: gRbase-graphs.Rnw:481-485
###################################################
rr <- rip(tug11)
rr
rr <- rip(tug11m)
rr <- rip(tug11M) 


###################################################
### code chunk number 36: gRbase-graphs.Rnw:489-490
###################################################
plot(rr)


###################################################
### code chunk number 37: gRbase-graphs.Rnw:526-528
###################################################
system.time({for (ii in 1:200) RBGL::maxClique(ug11)})     ## in RBGL
system.time({for (ii in 1:200) maxCliqueMAT(ug11m)}) ## in gRbase


###################################################
### code chunk number 38: gRbase-graphs.Rnw:535-537
###################################################
system.time({for (ii in 1:2000) ug11m[2,]})
system.time({for (ii in 1:2000) ug11M[2,]})


###################################################
### code chunk number 39: gRbase-graphs.Rnw:544-545
###################################################
system.time({for (ii in 1:2000) sp_getXj(ug11M,2)})


###################################################
### code chunk number 40: gRbase-graphs.Rnw:561-578
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
### code chunk number 41: gRbase-graphs.Rnw:596-597
###################################################
args(querygraph)


###################################################
### code chunk number 42: gRbase-graphs.Rnw:850-852
###################################################
#rm(print.list)
options("width"=85)


