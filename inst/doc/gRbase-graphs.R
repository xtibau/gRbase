### R code from vignette source 'gRbase-graphs.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRbase-graphs.Rnw:45-49
###################################################
dir.create("fig")
oopt <- options()
options("digits"=4, "width"=80, "prompt"="R> ", "continue"="  ")
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 2: gRbase-graphs.Rnw:52-53
###################################################
library(gRbase)


###################################################
### code chunk number 3: gRbase-graphs.Rnw:126-131
###################################################
ug11 <- ug(~a:b:c + c:d + d:e + a:e + f:g)
ug11 <- ug(~a*b*c + c*d + d*e + a*e + f*g)
ug12 <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"))
ug13 <- ug(~a:b:c, ~c:d, ~d:e + a:e + f:g)
ug13 <- ug(~a*b*c, ~c*d, ~d*e + a*e + f*g)


###################################################
### code chunk number 4: gRbase-graphs.Rnw:135-136
###################################################
ug11


###################################################
### code chunk number 5: gRbase-graphs.Rnw:146-149
###################################################
ug11m <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="matrix")
ug12m <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"), 
            result="matrix")


###################################################
### code chunk number 6: gRbase-graphs.Rnw:153-154
###################################################
ug11m


###################################################
### code chunk number 7: gRbase-graphs.Rnw:159-162
###################################################
ug11M <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="Matrix")
ug12M <- ug(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g"), 
            result="Matrix")


###################################################
### code chunk number 8: gRbase-graphs.Rnw:166-167
###################################################
ug11M


###################################################
### code chunk number 9: gRbase-graphs.Rnw:185-191
###################################################
dag11 <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f)
dag11 <- dag(~a + b*a + c*a*b + d*c*e + e*a + g*f)
dag12 <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"))
dag13 <- dag(~a, ~b:a,  ~c:a:b, ~d:c:e, ~e:a, ~g:f)
dag13 <- dag(~a, ~b*a,  ~c*a*b, ~d*c*e, ~e*a, ~g*f)


###################################################
### code chunk number 10: gRbase-graphs.Rnw:195-196
###################################################
dag11


###################################################
### code chunk number 11: gRbase-graphs.Rnw:208-211
###################################################
dag11m <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="matrix")
dag12m <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"), result="matrix")


###################################################
### code chunk number 12: gRbase-graphs.Rnw:215-216
###################################################
dag11m


###################################################
### code chunk number 13: gRbase-graphs.Rnw:219-222
###################################################
dag11M <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="Matrix")
dag12M <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"), 
             c("e","a"),c("g","f"), result="Matrix")


###################################################
### code chunk number 14: gRbase-graphs.Rnw:226-227
###################################################
dag11M


###################################################
### code chunk number 15: gRbase-graphs.Rnw:241-245
###################################################
as(ug11,"matrix")
as(as(ug11,"matrix"),"dgCMatrix")
as(as(as(ug11,"matrix"),"dgCMatrix"),"graphNEL")
as(as(as(as(ug11,"matrix"),"Matrix"),"graphNEL"),"igraph")


###################################################
### code chunk number 16: gRbase-graphs.Rnw:254-257
###################################################
m <- matrix(1:4,nrow=2)
as(m, "Matrix")
as(m, "dgCMatrix")


###################################################
### code chunk number 17: gRbase-graphs.Rnw:267-268
###################################################
asdgCMatrix(m)


###################################################
### code chunk number 18: gRbase-graphs.Rnw:285-288
###################################################
par(mfrow=c(1,2))
plot(ug11)
plot(as(ug11m,"graphNEL"))


###################################################
### code chunk number 19: gRbase-graphs.Rnw:295-299 (eval = FALSE)
###################################################
## par(mfrow=c(1,2))
## library(sna)
## gplot(ug11m, label=colnames(ug11m),gmode="graph")
## gplot(dag11m, label=colnames(dag11m))


###################################################
### code chunk number 20: gRbase-graphs.Rnw:313-314
###################################################
apropos("^moralize\\.")


###################################################
### code chunk number 21: gRbase-graphs.Rnw:321-322
###################################################
dag11.mor <- moralize(dag11)


###################################################
### code chunk number 22: gRbase-graphs.Rnw:326-329
###################################################
par(mfrow=c(1,2))
plot(dag11)
plot(dag11.mor)


###################################################
### code chunk number 23: gRbase-graphs.Rnw:335-337
###################################################
moralize(dag11m)
moralize(dag11M) 


###################################################
### code chunk number 24: gRbase-graphs.Rnw:352-355
###################################################
topoSort(dag11)
topoSort(dag11m)
topoSort(dag11M)


###################################################
### code chunk number 25: gRbase-graphs.Rnw:363-364
###################################################
topoSort(dag(~a:b+b:c+c:a))


###################################################
### code chunk number 26: gRbase-graphs.Rnw:381-384
###################################################
str(getCliques(ug11))
str(getCliques(ug11m))
str(getCliques(ug11M))


###################################################
### code chunk number 27: gRbase-graphs.Rnw:391-392
###################################################
apropos("^mcs\\.")


###################################################
### code chunk number 28: gRbase-graphs.Rnw:401-404
###################################################
mcs(ug11)
mcs(ug11m)
mcs(ug11M)


###################################################
### code chunk number 29: gRbase-graphs.Rnw:409-413
###################################################
mcs(dag11.mor)
mcs(as(dag11.mor,"matrix"))
mcs(as(dag11.mor,"Matrix")) 
mcs(dag11) 


###################################################
### code chunk number 30: gRbase-graphs.Rnw:420-421
###################################################
apropos("^triangulate\\.")


###################################################
### code chunk number 31: gRbase-graphs.Rnw:427-430
###################################################
(tug11<-triangulate(ug11))
(tug11m<-triangulate(ug11m))
(tug11M<-triangulate(ug11M))


###################################################
### code chunk number 32: gRbase-graphs.Rnw:434-437
###################################################
par(mfrow=c(1,2))
plot(ug11)
plot(tug11)


###################################################
### code chunk number 33: gRbase-graphs.Rnw:445-446
###################################################
apropos("^rip\\.")


###################################################
### code chunk number 34: gRbase-graphs.Rnw:454-458
###################################################
rr <- rip(tug11)
rr
rr <- rip(tug11m)
rr <- rip(tug11M) 


###################################################
### code chunk number 35: gRbase-graphs.Rnw:462-463
###################################################
plot(rr)


###################################################
### code chunk number 36: gRbase-graphs.Rnw:499-501
###################################################
system.time({for (ii in 1:200) maxClique(ug11)})     ## in RBGL
system.time({for (ii in 1:200) maxCliqueMAT(ug11m)}) ## in gRbase


###################################################
### code chunk number 37: gRbase-graphs.Rnw:508-510
###################################################
system.time({for (ii in 1:2000) ug11m[2,]})
system.time({for (ii in 1:2000) ug11M[2,]})


###################################################
### code chunk number 38: gRbase-graphs.Rnw:517-518
###################################################
system.time({for (ii in 1:2000) sp_getXj(ug11M,2)})


###################################################
### code chunk number 39: gRbase-graphs.Rnw:534-551
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
### code chunk number 40: gRbase-graphs.Rnw:569-570
###################################################
args(querygraph)


###################################################
### code chunk number 41: gRbase-graphs.Rnw:823-825
###################################################
#rm(print.list)
options("width"=85)


