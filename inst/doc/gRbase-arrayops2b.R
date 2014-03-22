### R code from vignette source 'gRbase-arrayops2b.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: gRbase-arrayops2b.Rnw:21-24
###################################################
require( gRbase )
prettyVersion <- packageDescription("gRbase")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: gRbase-arrayops2b.Rnw:70-73
###################################################
library(gRbase)
options("width"=100)
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 3: gRbase-arrayops2b.Rnw:90-92
###################################################
data(lizard, package="gRbase")
lizard


###################################################
### code chunk number 4: gRbase-arrayops2b.Rnw:98-101
###################################################
class( lizard )
str( lizard )
str( dimnames( lizard ) )


###################################################
### code chunk number 5: gRbase-arrayops2b.Rnw:161-163
###################################################
P <- arrayMargin(lizard, c("species","height")); P
Q <- arrayMargin(lizard, c("diam","species")); Q


###################################################
### code chunk number 6: gRbase-arrayops2b.Rnw:168-169
###################################################
tableSlice(lizard, "species", "anoli")


###################################################
### code chunk number 7: gRbase-arrayops2b.Rnw:174-176
###################################################
R  <- lizard
Rp <- arrayPerm(R, c("species","height","diam")); ftable( Rp )


###################################################
### code chunk number 8: gRbase-arrayops2b.Rnw:181-182
###################################################
R <- arrayOp( P, Q, op = "*" ); ftable( R )


###################################################
### code chunk number 9: gRbase-arrayops2b.Rnw:188-190
###################################################
PeQe <- extendArrays( P, Q ); lapply( PeQe, ftable )
#str( lapply( PeQe, dimnames ), max.level=2)


###################################################
### code chunk number 10: gRbase-arrayops2b.Rnw:196-199
###################################################
PeQe2 <- alignArrays( P, Q )
lapply( PeQe2, ftable )
#str( lapply( PeQe, dimnames ), max.level=2)


###################################################
### code chunk number 11: gRbase-arrayops2b.Rnw:213-216
###################################################
yn <- c('y','n')
parray(c("tub","asia"), levels=list(yn, yn), values=c(5,95,1,99))
array(c(5,95,1,99), dim=c(2,2), dimnames=list("tub"=yn, "asia"=yn))


###################################################
### code chunk number 12: gRbase-arrayops2b.Rnw:230-234
###################################################
p.t.a <- parray(c("tub","asia"), levels=list(yn, yn),
              values=c(5,95,1,99), normalize="first")
p.a <- parray("asia", list(yn), values=c(1,99),
              normalize="all")


###################################################
### code chunk number 13: gRbase-arrayops2b.Rnw:244-245
###################################################
p.ta <- arrayOp(p.t.a, p.a, op="*")


###################################################
### code chunk number 14: gRbase-arrayops2b.Rnw:251-252
###################################################
p.t <- arrayMargin(p.t.a, "tub")


###################################################
### code chunk number 15: gRbase-arrayops2b.Rnw:258-259
###################################################
arrayOp(p.ta, p.t, op="/")


###################################################
### code chunk number 16: gRbase-arrayops2b.Rnw:269-289
###################################################
myips <- function(indata, glist){
    fit   <- indata
    fit[] <-  1
    ## List of sufficient marginal tables
    md    <- lapply(glist, function(g) arrayMargin(indata, g))

    for (i in 1:4){
        for (j in seq_along(glist)){
            mf  <- arrayMargin(fit, glist[[j]])
            adj <- arrayOp( md[[j]], mf, op="/" )
            fit <- arrayOp( fit, adj, op="*" )
        }
    }
    pearson=sum( (fit-indata)^2 / fit)
    pearson
}

glist<-list(c("species","diam"),c("species","height"),c("diam","height"))
str( myips(lizard, glist), max.level=2)
str( loglin(lizard, glist), max.level = 2)


