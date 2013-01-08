pkgname <- "gRbase"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('gRbase')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("DATA-BodyFat")
### * DATA-BodyFat

flush(stderr()); flush(stdout())

### Name: BodyFat
### Title: Body Fat Data
### Aliases: BodyFat
### Keywords: datasets

### ** Examples

data(BodyFat)
head(BodyFat)



cleanEx()
nameEx("DATA-Nutrimouse")
### * DATA-Nutrimouse

flush(stderr()); flush(stdout())

### Name: Nutrimouse
### Title: The Nutrimouse Dataset
### Aliases: Nutrimouse
### Keywords: datasets

### ** Examples

data(Nutrimouse)



cleanEx()
nameEx("DATA-ashtrees")
### * DATA-ashtrees

flush(stderr()); flush(stdout())

### Name: ashtrees
### Title: Crown dieback in ash trees
### Aliases: ashtrees
### Keywords: datasets

### ** Examples

data(ashtrees)
## maybe str(ashtrees) ; plot(ashtrees) ...



cleanEx()
nameEx("DATA-breastcancer")
### * DATA-breastcancer

flush(stderr()); flush(stdout())

### Name: breastcancer
### Title: Gene expression signatures for p53 mutation status in 250 breast
###   cancer samples
### Aliases: breastcancer
### Keywords: datasets

### ** Examples

data(breastcancer)
## maybe str(breastcancer) ; plot(breastcancer) ...



cleanEx()
nameEx("DATA-cad")
### * DATA-cad

flush(stderr()); flush(stdout())

### Name: cad
### Title: Coronary artery disease data
### Aliases: cad1 cad2
### Keywords: datasets

### ** Examples

data(cad1)
## maybe str(cad1) ; plot(cad1) ...



cleanEx()
nameEx("DATA-chestSim")
### * DATA-chestSim

flush(stderr()); flush(stdout())

### Name: chestSim
### Title: Simulated data from the Chest Clinic example
### Aliases: chestSim chestSim500 chestSim1000 chestSim10000 chestSim50000
###   chestSim100000
### Keywords: datasets

### ** Examples

data(chestSim500)
## maybe str(chestSim500) ; plot(chestSim500) ...



cleanEx()
nameEx("DATA-dietox")
### * DATA-dietox

flush(stderr()); flush(stdout())

### Name: dietox
### Title: Growth curves of pigs in a 3x3 factorial experiment
### Aliases: dietox
### Keywords: datasets

### ** Examples

data(dietox)



cleanEx()
nameEx("DATA-dumping")
### * DATA-dumping

flush(stderr()); flush(stdout())

### Name: dumping
### Title: Gastric Dumping
### Aliases: dumping
### Keywords: datasets

### ** Examples

data(dumping) 
plot(dumping) 



cleanEx()
nameEx("DATA-lizard")
### * DATA-lizard

flush(stderr()); flush(stdout())

### Name: lizard
### Title: Lizard behaviour
### Aliases: lizard lizardRAW lizardAGG
### Keywords: datasets

### ** Examples

data(lizard)

# Datasets lizardRAW and lizardDF are generated with the following code
#lizardAGG <- as.data.frame(lizard)
#f   <- lizardAGG$Freq
#idx <- unlist(mapply(function(i,n) rep(i,n),1:8,f))
#set.seed(0805)
#idx <- sample(idx)
#lizardRAW <- as.data.frame(lizardAGG[idx,1:3])
#rownames(lizardRAW) <- 1:NROW(lizardRAW)




cleanEx()
nameEx("DATA-mathmark")
### * DATA-mathmark

flush(stderr()); flush(stdout())

### Name: mathmark
### Title: Mathematics marks for students
### Aliases: mathmark math
### Keywords: datasets

### ** Examples

data(mathmark)



cleanEx()
nameEx("DATA-mildew")
### * DATA-mildew

flush(stderr()); flush(stdout())

### Name: mildew
### Title: Mildew fungus
### Aliases: mildew
### Keywords: datasets

### ** Examples

data(mildew)
## maybe str(mildew) ; plot(mildew) ...



cleanEx()
nameEx("DATA-milkcomp")
### * DATA-milkcomp

flush(stderr()); flush(stdout())

### Name: milkcomp
### Title: Milk composition data
### Aliases: milkcomp milkcomp1
### Keywords: datasets

### ** Examples

data(milkcomp)
## maybe str(milk) ; plot(milk) ...



cleanEx()
nameEx("DATA-wine")
### * DATA-wine

flush(stderr()); flush(stdout())

### Name: wine
### Title: Chemical composition of wine
### Aliases: wine
### Keywords: datasets

### ** Examples

data(wine)
## maybe str(wine) ; plot(wine) ...



cleanEx()
nameEx("GraphAlgo-edgeList")
### * GraphAlgo-edgeList

flush(stderr()); flush(stdout())

### Name: edgeList
### Title: Find edges in a graph and edges not in a graph.
### Aliases: edgeList edgeList.graphNEL edgeList.matrix edgeListMAT
###   nonEdgeList nonEdgeList.graphNEL nonEdgeList.matrix nonEdgeListMAT
### Keywords: utilities

### ** Examples

## A graph with edges
g  <- ug(~a:b+b:c+c:d)
gm <- as.adjMAT(g)

edgeList(g)
edgeList(gm)
edgeListMAT(gm)

edgeList(g, matrix=TRUE)
edgeList(gm, matrix=TRUE)
edgeListMAT(gm, matrix=TRUE)

nonEdgeList(g)
nonEdgeList(gm)
nonEdgeListMAT(gm)

## A graph without edges
g  <- ug(~a+b+c)
gm <- as.adjMAT(g)

edgeList(g)
edgeList(gm)
edgeListMAT(gm)

edgeList(g, matrix=TRUE)
edgeList(gm, matrix=TRUE)
edgeListMAT(gm, matrix=TRUE)

nonEdgeList(g)
nonEdgeList(gm)
nonEdgeListMAT(gm)



cleanEx()
nameEx("GraphAlgo-glist2adjMAT")
### * GraphAlgo-glist2adjMAT

flush(stderr()); flush(stdout())

### Name: glist2adjMAT
### Title: Creates adjacency matrix for a graph from a list of generators
### Aliases: glist2adjMAT vpaL2adjMAT vpaL2tfM
### Keywords: utilities

### ** Examples

glist <- list(1:3,2:4,4:5)
am1 <- glist2adjMAT(glist)
am2 <- vpaL2adjMAT(glist)
## plot(coerceGraph(am1, "igraph"))
## plot(coerceGraph(am2, "igraph"))



cleanEx()
nameEx("GraphAlgo-is-graph")
### * GraphAlgo-is-graph

flush(stderr()); flush(stdout())

### Name: is.DAG
### Title: Check properties of graphs.
### Aliases: is.DAG is.DAG.matrix is.DAG.Matrix is.DAGMAT is.DAG.graphNEL
###   is.UG is.UG.matrix is.UG.Matrix is.UGMAT is.UG.graphNEL is.TUG
###   is.TUG.matrix is.TUG.Matrix is.TUGMAT is.TUG.graphNEL is.adjMAT
### Keywords: utilities

### ** Examples


## DAGs
dagNEL  <- dag(~a:b:c+c:d:e, result="NEL")
dagMAT  <- dag(~a:b:c+c:d:e, result="matrix")
dagMATS <- dag(~a:b:c+c:d:e, result="Matrix")

## Undirected graphs
ugNEL  <- ug(~a:b:c+c:d:e, result="NEL")
ugMAT  <- ug(~a:b:c+c:d:e, result="matrix")
ugMATS <- ug(~a:b:c+c:d:e, result="Matrix")

## Is it a DAG?
is.DAG(dagNEL)
is.DAG(dagMAT)
is.DAG(dagMATS)

is.DAG(ugNEL)
is.DAG(ugMAT)
is.DAG(ugMATS)

## Is it an undirected graph
is.UG(dagNEL)
is.UG(dagMAT)
is.UG(dagMATS)

is.UG(ugNEL)
is.UG(ugMAT)
is.UG(ugMATS)

## Is it a triangulated (i.e. chordal)  undirected graph
is.TUG(dagNEL)
is.TUG(dagMAT)
is.TUG(dagMATS)

is.TUG(ugNEL)
is.TUG(ugMAT)
is.TUG(ugMATS)

## Example where the graph is not triangulated
ug2NEL  <- ug(~a:b+b:c+c:d+d:a, result="NEL")
ug2MAT  <- ug(~a:b+b:c+c:d+d:a, result="matrix")
ug2MATS <- ug(~a:b+b:c+c:d+d:a, result="Matrix")

is.TUG(ug2NEL)
is.TUG(ug2MAT)
is.TUG(ug2MATS)

## Bidirected graphs
edgemode(ugNEL)
edgemode(ugNEL) <- "directed"
edgemode(ugNEL)
is.DAG(ugNEL)
is.UG(ugNEL)




cleanEx()
nameEx("GraphAlgo-mcs")
### * GraphAlgo-mcs

flush(stderr()); flush(stdout())

### Name: mcs
### Title: Maximum cardinality search on undirected graph.
### Aliases: mcs mcs.graphNEL mcs.igraph mcs.matrix mcs.Matrix mcsMAT
###   mcsmarked mcsmarked.graphNEL mcsmarked.igraph mcsmarked.matrix
###   mcsmarkedMAT
### Keywords: utilities

### ** Examples

uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
mcs(uG)
mcsMAT(as.adjMAT(uG))
## Same as
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st,result="matrix")
mcsMAT(uG)

## Marked graphs
uG1 <- ug(~a:b+b:c+c:d)
uG2 <- ug(~a:b+a:d+c:d)
## Not strongly decomposable:
mcsmarked(uG1, discrete=c("a","d"))
## Strongly decomposable:
mcsmarked(uG2, discrete=c("a","d"))



cleanEx()
nameEx("GraphAlgo-minimalTriang")
### * GraphAlgo-minimalTriang

flush(stderr()); flush(stdout())

### Name: minimalTriang
### Title: Minimal triangulation of an undirected graph
### Aliases: minimalTriang minimalTriangMAT
### Keywords: utilities

### ** Examples

## A graphNEL object
g1 <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e)
x <- minimalTriang(g1)

## g2 is a triangulation of g1 but it is not minimal
g2 <- ug(~a:b:e:f+b:c:d:e)
x<-minimalTriang(g1, TuG=g2)

## An adjacency matrix
g1m <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e, result="matrix")
x<-minimalTriangMAT(g1m)



cleanEx()
nameEx("GraphAlgo-moralize")
### * GraphAlgo-moralize

flush(stderr()); flush(stdout())

### Name: moralize
### Title: Moralize a directed acyclic graph
### Aliases: moralize moralize.graphNEL moralize.igraph moralize.matrix
###   moralize.Matrix moralizeMAT
### Keywords: utilities

### ** Examples

daG <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
moralize(daG)

daG <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st, result="matrix")
moralizeMAT(daG)




cleanEx()
nameEx("GraphAlgo-mpd")
### * GraphAlgo-mpd

flush(stderr()); flush(stdout())

### Name: mpd
### Title: Maximal prime subgraph decomposition
### Aliases: mpd mpdMAT
### Keywords: utilities

### ** Examples

## A graphNEL object
g1 <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e)
x <- mpd(g1)


## An adjacency matrix
g1m <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e, result="matrix")
x <- mpdMAT(g1m)





cleanEx()
nameEx("GraphAlgo-operations1")
### * GraphAlgo-operations1

flush(stderr()); flush(stdout())

### Name: graph-operations
### Title: Simple operations on undirected and directed acyclic graphs.
### Aliases: ancestors ancestralGraph ancestralSet children closure
###   is.complete is.decomposition is.simplicial parents simplicialNodes
###   as.adjMAT graphNEL2ftM maxCliqueMAT
### Keywords: utilities

### ** Examples

uG <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)
closure("me", uG)

maxClique(uG)
amat1 <- as.adjMAT(uG)
maxCliqueMAT(amat1)

amat1 <- as.adjMAT(uG, result="Matrix")
maxCliqueMAT(amat1)




cleanEx()
nameEx("GraphAlgo-querygraph")
### * GraphAlgo-querygraph

flush(stderr()); flush(stdout())

### Name: querygraph
### Title: Query a graph
### Aliases: querygraph
### Keywords: utilities

### ** Examples


ug0 <- ug(~a:b, ~b:c:d, ~e)

querygraph(ug0, "nodes")
querygraph(ug0, "edges")

querygraph(ug0, "subgraph", c("b","c","d","e"))

querygraph(ug0, "adj", "c")
querygraph(ug0, "closure", "c") 
querygraph(ug0, "is.simplicial", "b")
querygraph(ug0, "simplicialNodes")

querygraph(ug0, "is.complete")
querygraph(ug0, "is.complete", c("b","c","d"))
querygraph(ug0, "maxClique")

querygraph(ug0, "is.triangulated")
querygraph(ug0, "is.decomposition", "a","d",c("b","c"))




cleanEx()
nameEx("GraphAlgo-rip")
### * GraphAlgo-rip

flush(stderr()); flush(stdout())

### Name: rip
### Title: RIP ordering and junction tree.
### Aliases: rip rip.graphNEL rip.igraph rip.matrix rip.Matrix ripMAT
###   print.ripOrder plot.ripOrder jTree jTree.graphNEL jTree.igraph
###   jTree.matrix junctionTree junctionTree.graphNEL junctionTree.igraph
###   junctionTree.matrix
### Keywords: utilities

### ** Examples

## graphNEL
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
rip(uG)

## igraph
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st, result="igraph")
rip(uG)

## adjacency matrix
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st, result="matrix")
rip(uG)




cleanEx()
nameEx("GraphAlgo-topoSort")
### * GraphAlgo-topoSort

flush(stderr()); flush(stdout())

### Name: topoSort
### Title: Topological sort of vertices in directed
### Aliases: topoSort topoSort.graphNEL topoSort.matrix topoSort.Matrix
###   topoSortMAT topoSort_vparList
### Keywords: utilities

### ** Examples

dagMAT  <- dag(~a:b:c+c:d:e, result="matrix")
dagMATS <- as(dagMAT, "Matrix")
dagNEL  <- dag(~a:b:c+c:d:e, result="NEL")

topoSort(dagMAT)
topoSort(dagMATS)
topoSort(dagNEL)



cleanEx()
nameEx("GraphAlgo-triangulate")
### * GraphAlgo-triangulate

flush(stderr()); flush(stdout())

### Name: triangulate
### Title: Triangulation of an undirected graph
### Aliases: triangulate triangulate.graphNEL triangulate.matrix
###   triangulate.Matrix triangulate.igraph triangulateMAT
### Keywords: utilities

### ** Examples

## graphNEL
uG1 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a)
tuG1 <- triangulate(uG1)

## adjacency matrix
uG2 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="matrix")
tuG2 <- triangulate(uG2)

## adjacency matrix (sparse)
uG2 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="Matrix")
tuG2 <- triangulate(uG2)

## igraph
uG3 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="igraph")
tuG3 <- triangulate(uG3)



cleanEx()
nameEx("GraphAlgo-vpar")
### * GraphAlgo-vpar

flush(stderr()); flush(stdout())

### Name: vpar
### Title: List of vertices and their parents for graph.
### Aliases: vpar vpar.matrix vpar.Matrix vpar.graphNEL vparMAT
### Keywords: utilities

### ** Examples


## DAGs
dagMAT <- dag(~a:b:c+c:d:e, result="matrix")
dagNEL <- dag(~a:b:c+c:d:e, result="NEL")

vpar(dagMAT)
vpar(dagNEL)
vpar(dagMAT, getv=FALSE)
vpar(dagNEL, getv=FALSE)

## Undirected graphs
ugMAT <- ug(~a:b:c+c:d:e, result="matrix")
ugNEL <- ug(~a:b:c+c:d:e, result="NEL")

## Not run: 
##D ## This will fail because the adjacency matrix is symmetric and the
##D ## graphNEL has undirected edges
##D vpar(ugMAT)
##D vpar(ugNEL)
## End(Not run)

## When forceCheck is FALSE, it will not be detected that the graphs are undirected.
vpar(ugMAT, forceCheck=FALSE)
vpar(ugNEL, forceCheck=FALSE)

## Bidirected graphs
## This is, for graphNEL's, the same as working with bidirected edges:
edgemode(ugNEL)
edgemode(ugNEL) <- "directed"
edgemode(ugNEL)
vpar(ugNEL,FALSE)




cleanEx()
nameEx("ZOLD-gModel")
### * ZOLD-gModel

flush(stderr()); flush(stdout())

### Name: gModel
### Title: Class "gModel" - graphical models
### Aliases: gModel gModel-class formula formula.gModel formula<-
###   formula<-.gModel gmData gmData<- gmData.gModel gmData<-.gModel
###   print.gModel
### Keywords: models

### ** Examples

data(rats)
rats <- as.gmData(rats)

m1 <- gModel(~.^. , rats)
m1.form <- formula(m1)
m1.data <- gmData(m1)
observations(gmData(m1)) <- observations(rats)[1:10,]



cleanEx()
nameEx("ZOLD-gmData")
### * ZOLD-gmData

flush(stderr()); flush(stdout())

### Name: gmData
### Title: Class "gmData" graphical meta data
### Aliases: newgmData as.gmData as.gmData.array as.gmData.data.frame
###   as.gmData.table latent latent.gmData latent<- latent<-.gmData nLevels
###   nLevels.gmData nLevels<- nLevels<-.gmData description
###   description.gmData description<- description<-.gmData obs
###   observations observations.gmData observations<- observations<-.gmData
###   print.gmData shortNames shortNames.gmData shortNames<-
###   shortNames<-.gmData summary.gmData valueLabels valueLabels.gmData
###   valueLabels<- valueLabels<-.gmData varNames varNames.gmData
###   varNames<- varNames<-.gmData varTypes varTypes.gmData varTypes<-
###   varTypes<-.gmData dataOrigin dataOrigin.gmData ordinal<-
###   ordinal<-.gmData ordinal ordinal.gmData nominal<- nominal<-.gmData
###   nominal nominal.gmData
### Keywords: models

### ** Examples


vn <- c("a","b","c","d")
z<-newgmData(vn,varTypes=c("dis","dis","con","con"))
summary(z)
z<-newgmData(vn,varTypes=c("dis","dis","con","con"),nLevels=c(4,3,NA,NA))
summary(z)
z<-newgmData(vn,varTypes=c("dis","dis","con","con"),nLevels=c(4,NA,NA,NA))
summary(z)
z<-newgmData(vn,varTypes=c("dis","dis","ord","con"),valueLabels=list("a"=1:2, "b"=1:4))
summary(z)

ccnames <- c("asia", "smoke", "tub", "lung", "bronc", "either", "xray", "dysp")
gmd <- newgmData(ccnames,valueLabels=c("yes","no"), description="Chest clinic")
summary(gmd)

data(mathmark)
as.gmData(mathmark)

data(HairEyeColor)
as.gmData(HairEyeColor)




cleanEx()
nameEx("ZOLD-validVarTypes")
### * ZOLD-validVarTypes

flush(stderr()); flush(stdout())

### Name: validVarTypes
### Title: Admissible variable types in gmData objects
### Aliases: validVarTypes
### Keywords: models

### ** Examples

oldtypes <- validVarTypes()
validVartypes <- function() c(oldtypes, "MyVarType")
validVartypes()



cleanEx()
nameEx("arrayCombine")
### * arrayCombine

flush(stderr()); flush(stdout())

### Name: arrayCombine
### Title: Combine arrays
### Aliases: arrayCombine arrayExtendDomain
### Keywords: utilities

### ** Examples

## Case 1: t1 and t2 are arrays defined over identical sets of variables:
t1 <- parray(c("y","x1"), c(2,2), 1:4)
t2 <- parray(c("y","x1"), c(2,2), c(-11,12,-13,14))
tc <- arrayCombine(list(t1,t2), aux=list(Z=c(1,2)))
as.data.frame.table(tc)
## The "auxilary" variable Z adds a new dimension to the table

## Case 2: t1 and t2 are arrays defined over non-identical sets of variables:
t1 <- parray(c("y","x1"), c(2,2), 1:4)
t2 <- parray(c("y","x2"), c(2,2), c(-11,12,-13,14))
tc <- arrayCombine(list(t1,t2), aux=list(Z=c(1,2)))
as.data.frame.table(tc)
## The "auxilary" variable Z adds a new dimension to the table
## When Z=Z1, tc is constant as a function of x2
## When Z=Z2, tc is constant as a function of x1

## Case 3: t1 and t2 are arrays defined over non-identical sets of variables,
## but the variables for t1 is a subset of the variables for t2:
t1 <- parray(c("y","x1"), c(2,2), 1:4)
t2 <- parray(c("y","x1","x2"), c(2,2,2), 11:18)
tc <- arrayCombine(list(t1,t2), aux=list(Z=c(1,2)))
as.data.frame.table(tc)
## The "auxilary" variable Z adds a new dimension to the table
## When Z=Z1, tc is constant as a function of x2




cleanEx()
nameEx("combnPrim")
### * combnPrim

flush(stderr()); flush(stdout())

### Name: combnPrim
### Title: Generate All Combinations of n Elements Taken m at a Time
### Aliases: combnPrim
### Keywords: utilities

### ** Examples


x <- letters[1:20]
m <- 3

combn(x,m)
combnPrim(x,m)

combn(m,m)
combnPrim(m,m)

combn(x,m, simplify=FALSE)
combnPrim(x,m, simplify=FALSE)

system.time({ for (ii in 1:100) { combnPrim(x,m) }})
system.time({ for (ii in 1:100) { combn(x,m) }})

system.time({ for (ii in 1:100) { combnPrim(x,m, simplify=FALSE) }})
system.time({ for (ii in 1:100) { combn(x,m, simplify=FALSE) }})




cleanEx()
nameEx("cov2pcor")
### * cov2pcor

flush(stderr()); flush(stdout())

### Name: cov2pcor
### Title: Partial correlation (matrix)
### Aliases: cov2pcor conc2pcor
### Keywords: utilities

### ** Examples

data(math)
S <- cov.wt(math)$cov
cov2pcor(S)



cleanEx()
nameEx("gRbase-utilities")
### * gRbase-utilities

flush(stderr()); flush(stdout())

### Name: gRbase-utilities
### Title: Utility functions for gRbase
### Aliases: colSumsPrim intersectPrim matchPrim outerPrim setdiffPrim
###   uniquePrim unlistPrim rowSumsPrim colwiseProd
### Keywords: utilities

### ** Examples

uniquePrim(c(1,2,3,2,1,2))
setdiffPrim(c(1,3,2), c(2,3,4,5))
unlistPrim(list(c(1,2),c(2,3)))

## colwiseProd
M <- matrix(1:16, nrow=4)
v <- 1:4

t(v*t(M))
colwiseProd(v,M)

system.time(for (ii in 1:100000)  t(v*t(M)))
system.time(for (ii in 1:100000)  colwiseProd(v,M))



cleanEx()
nameEx("iplot")
### * iplot

flush(stderr()); flush(stdout())

### Name: iplot
### Title: Function for plotting graphs using the 'igraph' package.
### Aliases: iplot iplot.graphNEL
### Keywords: graphics

### ** Examples

UG <- ug(~a:b+b:c:d)
iplot(UG)



cleanEx()
nameEx("parray")
### * parray

flush(stderr()); flush(stdout())

### Name: parray
### Title: Representation of and operations on multidimensional tables
### Aliases: parray as.parray varNames.parray nLevels.parray
###   valueLabels.parray varNames.array nLevels.array valueLabels.array
###   print.parray tableMarginPrim
### Keywords: utilities

### ** Examples

t1 <- parray(c("gender","answer"),list(c('male','female'),c('yes','no')), values=1:4)
t1 <- parray(~gender:answer,list(c('male','female'),c('yes','no')), values=1:4)
t1 <- parray(~gender:answer,c(2,2), values=1:4)

t2 <- parray(c("answer","category"), list(c('yes','no'),c(1,2)), values=1:4+10)
t3 <- parray(c("category","foo"), c(2,2), values=1:4+100)

varNames(t1)
nLevels(t1)
valueLabels(t1)




cleanEx()
nameEx("randomDag")
### * randomDag

flush(stderr()); flush(stdout())

### Name: random_dag
### Title: Random directed acyclic graph
### Aliases: random_dag
### Keywords: utilities

### ** Examples

dg   <- random_dag(1:1000, maxpar=5, wgt=.9)
table(sapply(vpar(dg),length))

dg   <- random_dag(1:1000, maxpar=5, wgt=.5)
table(sapply(vpar(dg),length))

dg   <- random_dag(1:1000, maxpar=5, wgt=.1)
table(sapply(vpar(dg),length))




cleanEx()
nameEx("removeRedundant")
### * removeRedundant

flush(stderr()); flush(stdout())

### Name: Setoperations
### Title: Set operations
### Aliases: is.insetlist isin is.subsetof subsetof removeRedundant
###   maximalSets minimalSets
### Keywords: utilities

### ** Examples


is.subsetof(c(1,2),c(1,2,3))
is.subsetof(c(1,2,3), c(1,2))

l <- list(c(1,2),c(1,2,3),c(2,4),c(5,6), 5)

#subsetofList(c(1,2), l)
#subsetofList(c(1,2,3,4), l)

removeRedundant(l)
removeRedundant(l, maximal=FALSE)

is.insetlist (c(2,4), l)
is.insetlist (c(2,8), l)




cleanEx()
nameEx("table-operations")
### * table-operations

flush(stderr()); flush(stdout())

### Name: table-operations
### Title: Compute table margin or table slice
### Aliases: tableSlice tableSlicePrim tableMargin tableOp tableMult
###   tableDiv tableOp2 tablePerm
### Keywords: utilities

### ** Examples


data(HairEyeColor)

tableMargin(HairEyeColor, "Hair")
tableMargin(HairEyeColor, 1)
tableMargin(HairEyeColor, c("Hair","Eye"))
tableMargin(HairEyeColor, c(1,2))


tableSlice(HairEyeColor, "Sex","Male")
tableSlice(HairEyeColor, 3,1)
tableSlice(HairEyeColor, "Sex","Male", impose=1000)
tableSlice(HairEyeColor, 3,1, impose=1000)

t1 <- array(1:4, dim=c(2,2), dimnames=list(gender=c('male','female'),answer=c('yes','no')))
t2 <- array(1:4+10, dim=c(2,2), dimnames=list(answer=c('yes','no'),category=c(1,2)))

tableOp(t1,t2, "*")
tableOp(t1,t2, "/")

data(reinis)

t1 <- tableMargin(reinis, c(6,5,2,1))
t2 <- tableMargin(reinis, c(6,5,3,4))

tt1 <- tableOp(t1,t2)

t1 <- tableMargin(reinis, c(6,5,2,4,1))
t2 <- tableMargin(reinis, c(6,5,4))

tt1 <- tableOp2(t1,t2)





cleanEx()
nameEx("ugdag")
### * ugdag

flush(stderr()); flush(stdout())

### Name: ug
### Title: Create undirected and directed graphs
### Aliases: ug dag ugList dagList
### Keywords: utilities

### ** Examples

ugr <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)

ugr <- ug(~me:ve:al,~al:an:st)

ugr <- ug(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st")) 

ugr <- ug(~me:ve:al, c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))


dagr <- dag(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))


dagr <- dag(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)


dagr <- dag(~me:ve:al,~ve:al:an)

edges(ugr)
nodes(ugr)

edges(dagr)
nodes(dagr)

ugList(list(~me:ve:al,~al:an:st))
dagList(list(~me:ve:al,~ve:al:an))






### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
