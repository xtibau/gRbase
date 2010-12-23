pkgname <- "gRbase"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('gRbase')

assign(".oldSearch", search(), pos = 'CheckExEnv')
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
nameEx("DATA-lizard")
### * DATA-lizard

flush(stderr()); flush(stdout())

### Name: lizard
### Title: Lizard behaviour
### Aliases: lizard
### Keywords: datasets

### ** Examples

data(lizard)
## maybe str(lizard) ; plot(lizard) ...



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
g1m <- ugMAT(~a:b+b:c+c:d+d:e+e:f+a:f+b:e)
x<-minimalTriangMAT(g1m)



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
g1m <- ugMAT(~a:b+b:c+c:d+d:e+e:f+a:f+b:e)
x <- mpdMAT(g1m)





cleanEx()
nameEx("GraphAlgo-operations1")
### * GraphAlgo-operations1

flush(stderr()); flush(stdout())

### Name: graph-operations1
### Title: Simple operations on undirected and directed acyclic graphs.
### Aliases: as.adjMAT ancestors ancestralGraph ancestralSet children
###   closure vpar is.complete is.decomposition is.simplicial parents
###   simplicialNodes
### Keywords: utilities

### ** Examples

ugr <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)
closure("me", ugr)



cleanEx()
nameEx("GraphAlgo-operations2")
### * GraphAlgo-operations2

flush(stderr()); flush(stdout())

### Name: graph-operations2
### Title: More advanced operations on undirected and directed acyclic
###   graphs.
### Aliases: mcs mcsMAT rip ripMAT moralize moralizeMAT maxCliqueMAT jTree
###   print.ripOrder
### Keywords: utilities

### ** Examples


## Undirected graphs
##
ugr <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
ugm <- as.adjMAT(ugr)
edges(ugr)
nodes(ugr)
mcs(ugr)
mcsMAT(ugm)
rip(ugr)
ripMAT(ugm)

maxClique(ugr)
maxCliqueMAT(ugm)

## Directed graphs
##
dagr <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
edges(dagr)
nodes(dagr)
moralize(dagr)



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
nameEx("GraphAlgo-triangulate")
### * GraphAlgo-triangulate

flush(stderr()); flush(stdout())

### Name: triangulate
### Title: Triangulation of an undirected graph
### Aliases: triangulate triangulate.graphNEL triangulateMAT
### Keywords: utilities

### ** Examples

ugr <- ug(~a:b+b:c+c:d+d:e+e:f+f:a)
triangulate(ugr)



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
nameEx("gModel")
### * gModel

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
nameEx("gRbase-utilities")
### * gRbase-utilities

flush(stderr()); flush(stdout())

### Name: gRbase-utilities
### Title: Utility functions for gRbase
### Aliases: colSumsPrim intersectPrim matchPrim outerPrim setdiffPrim
###   uniquePrim unlistPrim rowSumsPrim
### Keywords: utilities

### ** Examples

uniquePrim(c(1,2,3,2,1,2))
setdiffPrim(c(1,3,2), c(2,3,4,5))
unlistPrim(list(c(1,2),c(2,3)))



cleanEx()
nameEx("gRfit")
### * gRfit

flush(stderr()); flush(stdout())

### Name: gRfit
### Title: Class "gRfit" - fitted graphical models
### Aliases: gRfit getFit getFit<- getFit<-.gRfit getFit.gRfit print.gRfit
###   summary.gRfit fit
### Keywords: models

### ** Examples

data(reinis)
reinis <- as.gmData(reinis)

m1 <- hllm(~.^. , reinis) 
m1 <- fit(m1,engine="loglm")



cleanEx()
nameEx("gmData")
### * gmData

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
nameEx("hllm")
### * hllm

flush(stderr()); flush(stdout())

### Name: hllm
### Title: Hierarchical log-linear models
### Aliases: hllm fit.hllm stepwise.hllm hllm-class
### Keywords: models

### ** Examples

data(reinis)
reinis <- as.gmData(reinis)
m2 <-
hllm(~smoke*phys*protein+mental*phys+mental*family+smoke*systol*protein,
reinis)
m2 <- fit(m2,engine="loglm")
## plot(m2)



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
nameEx("ptable")
### * ptable

flush(stderr()); flush(stdout())

### Name: ptable
### Title: Representation of and operations on multidimensional tables
### Aliases: ptable as.ptable varNames.ptable nLevels.ptable
###   valueLabels.ptable varNames.array nLevels.array valueLabels.array
###   print.ptable tableMarginPrim
### Keywords: utilities

### ** Examples

t1 <- ptable(c("gender","answer"),list(c('male','female'),c('yes','no')), values=1:4)
t1 <- ptable(~gender+answer,list(c('male','female'),c('yes','no')), values=1:4)
t1 <- ptable(~gender+answer,c(2,2), values=1:4)

t2 <- ptable(c("answer","category"), list(c('yes','no'),c(1,2)), values=1:4+10)
t3 <- ptable(c("category","foo"), c(2,2), values=1:4+100)





varNames(t1)
nLevels(t1)
valueLabels(t1)




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
### Aliases: tableSlice tableSlicePrim tableMargin tableOp tablePerm
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
### Aliases: ug ugList ugMAT ugListMAT dag dagMAT dagList dagListMAT
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






cleanEx()
nameEx("validVarTypes")
### * validVarTypes

flush(stderr()); flush(stdout())

### Name: validVarTypes
### Title: Admissible variable types in gmData objects
### Aliases: validVarTypes
### Keywords: models

### ** Examples

oldtypes <- validVarTypes()
validVartypes <- function() c(oldtypes, "MyVarType")
validVartypes()



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
