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
### code chunk number 3: gRbase-graphs.Rnw:78-80
###################################################
library(gRbase)
library(graph)


###################################################
### code chunk number 4: gRbase-graphs.Rnw:152-156
###################################################
ug11 <- ug( ~a:b:c + c:d + d:e + a:e + f:g )
ug12 <- ug( c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g") )
ug13 <- ug(~a:b:c, ~c:d, ~d:e + a:e + f:g)
ug14 <- ugList( list(c("a","b","c"),c("c","d"),c("d","e"),c("a","e"),c("f","g")) )


###################################################
### code chunk number 5: gRbase-graphs.Rnw:165-166
###################################################
ug11


###################################################
### code chunk number 6: gRbase-graphs.Rnw:170-171
###################################################
plot( ug11 )


###################################################
### code chunk number 7: gRbase-graphs.Rnw:178-179
###################################################
ug11m <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="matrix"); ug11m


###################################################
### code chunk number 8: gRbase-graphs.Rnw:185-186
###################################################
ug11M <- ug(~a*b*c + c*d + d*e + a*e + f*g, result="dgCMatrix"); ug11M


###################################################
### code chunk number 9: gRbase-graphs.Rnw:197-203
###################################################
dag11 <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f)
dag12 <- dag("a", c("b","a"), c("c","a","b"), c("d","c","e"),
             c("e","a"),c("g","f"))
dag13 <- dag(~a, ~b:a,  ~c:a:b, ~d:c:e, ~e:a, ~g:f)
dag14 <- dagList(list( "a", c("b","a"), c("c","a","b"), c("d","c","e"),
                      c("e","a"),c("g","f") ))


###################################################
### code chunk number 10: gRbase-graphs.Rnw:211-212
###################################################
dag11


###################################################
### code chunk number 11: gRbase-graphs.Rnw:218-219
###################################################
dag11m <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="matrix")


###################################################
### code chunk number 12: gRbase-graphs.Rnw:223-224
###################################################
dag11M <- dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="dgCMatrix")


###################################################
### code chunk number 13: gRbase-graphs.Rnw:233-237
###################################################
d1.bi <- dag(~a:b + b:a)
edgemode( d1.bi )
str( nodes(d1.bi) )
str( edges(d1.bi) )


###################################################
### code chunk number 14: gRbase-graphs.Rnw:243-244
###################################################
d2.cyc <- dag(~a:b+b:c+c:a)


###################################################
### code chunk number 15: gRbase-graphs.Rnw:248-249
###################################################
par(mfrow=c(1,2)); plot(d1.bi); plot(d2.cyc)


###################################################
### code chunk number 16: gRbase-graphs.Rnw:255-256
###################################################
print( try( dag(~a:b+b:c+c:a, forceCheck=TRUE) ) )


###################################################
### code chunk number 17: gRbase-graphs.Rnw:265-268
###################################################
str( graph::nodes( ug11 ) )
str( graph::edges( ug11 ) )
graph::edgeMatrix( ug11 )


###################################################
### code chunk number 18: gRbase-graphs.Rnw:272-273
###################################################
str( RBGL::maxClique( ug11 ) )


###################################################
### code chunk number 19: gRbase-graphs.Rnw:328-336
###################################################
properties <- function(x){
    c(is.UG=is.UG(x), is.TUG=is.TUG(x), is.DG=is.DG(x), is.DAG=is.DAG(x),
      isD=graph::isDirected(x))
}
properties( ug11 )
properties( dag11 )
properties( d1.bi )
properties( d2.cyc )


###################################################
### code chunk number 20: gRbase-graphs.Rnw:347-350
###################################################
(mat <- as(ug11, "matrix"))
(Mat <- as(mat, "dgCMatrix"))
(NEL <- as(Mat, "graphNEL"))


###################################################
### code chunk number 21: gRbase-graphs.Rnw:355-361
###################################################
(mat <- coerceGraph(ug11, "matrix"))
(Mat <- coerceGraph(mat, "dgCMatrix"))
(NEL <- as(Mat, "graphNEL"))
if( require(microbenchmark) ){
microbenchmark( as(ug11, "matrix"), coerceGraph(ug11, "matrix") )
}


###################################################
### code chunk number 22: gRbase-graphs.Rnw:383-384
###################################################
par(mfrow=c(1,2)); plot(ug11); plot(as(ug11m,"graphNEL"))


###################################################
### code chunk number 23: gRbase-graphs.Rnw:391-396 (eval = FALSE)
###################################################
## if (require(sna)){
##     par(mfrow=c(1,2))
##     gplot(ug11m, label=colnames(ug11m), gmode="graph")
##     gplot(dag11m, label=colnames(dag11m))
## }


###################################################
### code chunk number 24: gRbase-graphs.Rnw:416-417
###################################################
dag11.mor <- moralize(dag11)


###################################################
### code chunk number 25: gRbase-graphs.Rnw:421-422
###################################################
par(mfrow=c(1,2)); plot(dag11); plot(dag11.mor)


###################################################
### code chunk number 26: gRbase-graphs.Rnw:428-430
###################################################
class( moralize( dag11, result="matrix" ) )
class( moralize( dag11, result="dgCMatrix" ) )


###################################################
### code chunk number 27: gRbase-graphs.Rnw:453-456
###################################################
topoSort(dag11)
topoSort(dag11m)
topoSort(dag11M)


###################################################
### code chunk number 28: gRbase-graphs.Rnw:463-464
###################################################
topoSort(dag(~a:b+b:c+c:a))


###################################################
### code chunk number 29: gRbase-graphs.Rnw:470-471
###################################################
topoSort( ug( ~a:b ) )


###################################################
### code chunk number 30: gRbase-graphs.Rnw:487-490
###################################################
str( getCliques(ug11) )
str( getCliques(ug11m) )
str( getCliques(ug11M) )


###################################################
### code chunk number 31: gRbase-graphs.Rnw:497-502
###################################################
if (require(microbenchmark)){
    microbenchmark(
        RBGL::maxClique( ug11 ), getCliques( ug11 ), getCliques( ug11m ),
        getCliques( ug11M ))
}


###################################################
### code chunk number 32: gRbase-graphs.Rnw:519-522
###################################################
mcs(ug11)
mcs(ug11m)
mcs(ug11M)


###################################################
### code chunk number 33: gRbase-graphs.Rnw:527-531
###################################################
mcs( dag11.mor )
mcs( as( dag11.mor, "matrix" ))
mcs( as( dag11.mor, "dgCMatrix" ))
mcs( dag11 )


###################################################
### code chunk number 34: gRbase-graphs.Rnw:538-541
###################################################
(tug11  <- triangulate(ug11))
(tug11m <- triangulate(ug11m))
(tug11M <- triangulate(ug11M))


###################################################
### code chunk number 35: gRbase-graphs.Rnw:545-546
###################################################
par(mfrow=c(1,2)); plot(ug11); plot(tug11)


###################################################
### code chunk number 36: gRbase-graphs.Rnw:552-554
###################################################
class( triangulate( ug11, result = "matrix" ) )
class( triangulate( ug11, result = "dgCMatrix" ) )


###################################################
### code chunk number 37: gRbase-graphs.Rnw:564-565
###################################################
rp <- rip(tug11); rp


###################################################
### code chunk number 38: gRbase-graphs.Rnw:569-570
###################################################
plot( rp )


###################################################
### code chunk number 39: gRbase-graphs.Rnw:593-595
###################################################
g1 <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e)
g1mt <- minimalTriang(g1) # A minimal triangulation


###################################################
### code chunk number 40: gRbase-graphs.Rnw:599-600
###################################################
par(mfrow = c(1,2)); plot(g1); plot(g1mt)


###################################################
### code chunk number 41: gRbase-graphs.Rnw:606-608
###################################################
g2 <- ug(~a:b:e:f+b:c:d:e)
g1mt2 <- minimalTriang(g1, tobject=g2)


###################################################
### code chunk number 42: gRbase-graphs.Rnw:612-613
###################################################
par(mfrow = c(1,2)); plot(g2); plot(g1mt2)


###################################################
### code chunk number 43: gRbase-graphs.Rnw:618-619
###################################################
mm <- mpd( g1 ); mm


###################################################
### code chunk number 44: gRbase-graphs.Rnw:623-626
###################################################
par(mfrow = c(1,2))
plot(subGraph(mm$cliques[[1]], g1))
plot(subGraph(mm$cliques[[2]], g1))


###################################################
### code chunk number 45: gRbase-graphs.Rnw:644-650
###################################################
if(require(microbenchmark)){
    microbenchmark(
        RBGL::maxClique(ug11),
        getCliques(ug11),
        getCliques(ug11m),
        getCliques(ug11M)  ) }


###################################################
### code chunk number 46: gRbase-graphs.Rnw:666-682
###################################################
V <- 1:300
M <- 1:10
## Sparse graph
##
g1 <- randomGraph(V, M, 0.05)
length(edgeList(g1))
s<-c(NEL=object.size(g1), dense=object.size(as(g1, "matrix")),
     sparse=object.size(as(g1, "dgCMatrix")))
s/max(s)
## More dense graph
##
g1 <- randomGraph(V, M, 0.5)
length(edgeList(g1))
s <- c(NEL=object.size(g1), dense=object.size(as(g1, "matrix")),
       sparse=object.size(as(g1, "dgCMatrix")))
s/max(s)


###################################################
### code chunk number 47: gRbase-graphs.Rnw:700-701
###################################################
args(querygraph)


###################################################
### code chunk number 48: gRbase-graphs.Rnw:708-710
###################################################
#rm(print.list)
options("width"=85)


