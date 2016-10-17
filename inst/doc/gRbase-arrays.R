### R code from vignette source 'gRbase-arrays.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: gRbase-arrays.Rnw:32-35
###################################################
#require( gRbase )
prettyVersion <- packageDescription("gRbase")$Version
prettyDate <- format(Sys.Date())


###################################################
### code chunk number 2: gRbase-arrays.Rnw:81-86
###################################################
library(gRbase)
##load_all("../gRbase")
options("width"=100)
options(useFancyQuotes="UTF-8")
##options(prompt=" ", "continue"="   ")


###################################################
### code chunk number 3: gRbase-arrays.Rnw:119-120
###################################################
HairEyeColor


###################################################
### code chunk number 4: gRbase-arrays.Rnw:128-131
###################################################
class( HairEyeColor )
is.array( HairEyeColor )
dim( HairEyeColor )


###################################################
### code chunk number 5: gRbase-arrays.Rnw:137-138
###################################################
dimnames( HairEyeColor )


###################################################
### code chunk number 6: gRbase-arrays.Rnw:147-148
###################################################
is.named.array( HairEyeColor )


###################################################
### code chunk number 7: gRbase-arrays.Rnw:153-156
###################################################
hec <- do.call("[", c(list(HairEyeColor), list(1:2, 1:3, TRUE), drop=FALSE))
hec <- HairEyeColor[1:2, 1:3, ]
hec


###################################################
### code chunk number 8: gRbase-arrays.Rnw:163-165
###################################################
flat <- function(x) {ftable(x, row.vars=1)}
hec %>% flat


###################################################
### code chunk number 9: gRbase-arrays.Rnw:173-182
###################################################
z1 <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29)
di <- c(2, 3, 2)
dn <- list(Hair = c("Black", "Brown"), 
           Eye = c("Brown", "Blue", "Hazel"), 
           Sex = c("Male", "Female"))
dim( z1 ) <- di
dimnames( z1 ) <- dn
z2 <- array( c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29),
            dim=di, dimnames=dn)


###################################################
### code chunk number 10: gRbase-arrays.Rnw:187-190
###################################################
counts <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29)
z3 <- newar( ~ Hair:Eye:Sex, levels = dn, value = counts) 
z4 <- newar(c("Hair", "Eye", "Sex"), levels=dn, values=counts)


###################################################
### code chunk number 11: gRbase-arrays.Rnw:197-199
###################################################
z5 <- newar(~Hair:Eye:Sex, levels=c(2, 3, 2), values = counts)
z5 %>% flat


###################################################
### code chunk number 12: gRbase-arrays.Rnw:208-210
###################################################
z6 <- newar(~Hair:Eye:Sex, levels=c(2, 3, 2), values = counts, normalize="first")
z6 %>% flat


###################################################
### code chunk number 13: gRbase-arrays.Rnw:214-215
###################################################
arnormalize(z5, "first") %>% flat


###################################################
### code chunk number 14: gRbase-arrays.Rnw:234-235
###################################################
arperm(hec, ~Eye:Sex:Hair) %>% flat 


###################################################
### code chunk number 15: gRbase-arrays.Rnw:240-242
###################################################
arperm(hec, c("Eye", "Sex", "Hair")) %>% flat
arperm(hec, c(2,3,1)) %>% flat


###################################################
### code chunk number 16: gRbase-arrays.Rnw:247-249
###################################################
arperm(hec, ~Ey:Se:Ha) %>% flat 
arperm(hec, c("Ey", "Se", "Ha")) %>% flat


###################################################
### code chunk number 17: gRbase-arrays.Rnw:261-265
###################################################
hec[, 2:3, ]  %>% flat
is.array( hec[, 2:3, ] ) 
hec[1, , 1]
is.array( hec[1, , 1] )


###################################################
### code chunk number 18: gRbase-arrays.Rnw:270-272
###################################################
do.call("[", c(list(hec), list(TRUE, 2:3, TRUE)))  %>% flat
do.call("[", c(list(hec), list(1, TRUE, 1)))  %>% flat


###################################################
### code chunk number 19: gRbase-arrays.Rnw:278-279
###################################################
arslice_prim(hec, slice=list(TRUE, 2:3, TRUE))  %>% flat


###################################################
### code chunk number 20: gRbase-arrays.Rnw:285-287
###################################################
hec[, 2:3, ] %>% flat
arslice(hec, slice=list(c(2,3)), margin=2) %>% flat


###################################################
### code chunk number 21: gRbase-arrays.Rnw:292-294
###################################################
arslice(hec, slice=list(Eye=2:3)) %>% flat
arslice(hec, slice=list(Eye=c("Blue","Hazel")))  %>% flat


###################################################
### code chunk number 22: gRbase-arrays.Rnw:302-304
###################################################
arslice(hec, slice=list(Eye=c(2,3), Sex="Female")) %>% flat
arslice(hec, slice=list(Eye=c(2,3), Sex="Female"), drop=FALSE) %>% flat


###################################################
### code chunk number 23: gRbase-arrays.Rnw:310-312
###################################################
z <- arslice(hec, slice=list(Hair=1, Sex="Female")); z 
is.array( z )


###################################################
### code chunk number 24: gRbase-arrays.Rnw:317-319
###################################################
z <- arslice(hec, slice=list(Hair=1, Sex="Female"), as.array=TRUE); z
is.array( z )


###################################################
### code chunk number 25: gRbase-arrays.Rnw:337-338
###################################################
he <- armarg(hec, ~Hair:Eye); he %>% flat       


###################################################
### code chunk number 26: gRbase-arrays.Rnw:343-345
###################################################
hs <- armarg(hec, c("Hair","Sex")); hs 
es <- armarg(hec, c(2,3)); es


###################################################
### code chunk number 27: gRbase-arrays.Rnw:360-361
###################################################
arexpand(he, list(Sex=c("Male", "Female"))) %>% flat


###################################################
### code chunk number 28: gRbase-arrays.Rnw:365-367
###################################################
arexpand(he, dimnames(hs)) %>% flat
arexpand(he, hs) %>% flat


###################################################
### code chunk number 29: gRbase-arrays.Rnw:386-387
###################################################
armult(he, hs) %>% flat


###################################################
### code chunk number 30: gRbase-arrays.Rnw:393-396
###################################################
ardiv(he, hs)  %>% flat 
aradd(he, hs)  %>% flat
arsubt(he, hs) %>% flat


###################################################
### code chunk number 31: gRbase-arrays.Rnw:402-404
###################################################
arprod( he, hs, es ) %>% flat
arsum( he, hs, es ) %>% flat


###################################################
### code chunk number 32: gRbase-arrays.Rnw:416-418
###################################################
hec2 <- arperm(hec, 3:1)
arequal(hec, hec2)


###################################################
### code chunk number 33: gRbase-arrays.Rnw:426-428
###################################################
hec3 <- ardiv( armult( he, es ), armarg( hec, "Eye" ) )
hec3 %>% flat


###################################################
### code chunk number 34: gRbase-arrays.Rnw:432-433
###################################################
hec %>% flat


###################################################
### code chunk number 35: gRbase-arrays.Rnw:441-442
###################################################
aralign(hec3, hec)  %>% flat


###################################################
### code chunk number 36: gRbase-arrays.Rnw:474-483
###################################################
yn <- c("y","n")
lev <- list(rain=yn, sprinkler=yn, wet=yn)
r <- newar( ~rain, levels = lev, values = c(.2, .8) )
s_r <- newar( ~sprinkler:rain, levels = lev, values = c(.01,.99, .4, .6) )
w_sr <- newar( ~wet:sprinkler:rain, levels = lev, 
             values = c(.99, .01, .8, .2, .9, .1, 0, 1))
r 
s_r  %>% flat
w_sr %>% flat


###################################################
### code chunk number 37: gRbase-arrays.Rnw:490-491
###################################################
joint <- arprod( r, s_r, w_sr ); joint %>% flat


###################################################
### code chunk number 38: gRbase-arrays.Rnw:497-499
###################################################
rw <- armarg(joint, ~rain+wet)
ardist(rw, cond=~wet)


###################################################
### code chunk number 39: gRbase-arrays.Rnw:503-507 (eval = FALSE)
###################################################
## ## Alternative:
## ardiv( rw, armarg(rw, ~wet))
## ## or
## rw %a/% (rw %am% ~wet)


###################################################
### code chunk number 40: gRbase-arrays.Rnw:513-516
###################################################
x <- arslice_mult(rw, slice=list(wet="y")); x
p <- armarg(x, ~rain); p  ## Unnormalized
ardist( p )           ## Normalized


###################################################
### code chunk number 41: gRbase-arrays.Rnw:524-526
###################################################
data( lizard, package="gRbase" )
lizard


###################################################
### code chunk number 42: gRbase-arrays.Rnw:556-585
###################################################
myips <- function(indata, glist){
    fit   <- indata
    fit[] <-  1
    ## List of sufficient marginal tables
    md    <- lapply(glist, function(g) armarg(indata, g))

    for (i in 1:4){
        for (j in seq_along(glist)){
            mf  <- armarg(fit, glist[[j]])
            ## adj <- ardiv( md[[ j ]], mf)
            ## fit <- armult( fit, adj )
            ## or
            adj <- md[[ j ]] %a/% mf
            fit <- fit %a*% adj
        }
    }
    pearson <- sum( (fit-indata)^2 / fit)
    list(pearson=pearson, fit=fit)
}

glist <- list(c("species","diam"),c("species","height"),c("diam","height"))

fm1 <- myips( lizard, glist )
fm1$pearson
fm1$fit %>% flat

fm2 <- loglin( lizard, glist, fit=T )
fm2$pearson
fm2$fit %>% flat


###################################################
### code chunk number 43: gRbase-arrays.Rnw:603-605
###################################################
hec
c(hec)


###################################################
### code chunk number 44: gRbase-arrays.Rnw:610-614
###################################################
cell2name <- function(cell, dimnames){
    unlist(lapply(1:length(cell), function(m) dimnames[[m]][cell[m]]))
}
cell2name(c(2,3,1), dimnames(hec))


###################################################
### code chunk number 45: gRbase-arrays.Rnw:628-630
###################################################
cell2entry(c(2,3,1), dim=c( 2, 3, 2 ))
entry2cell(6, dim=c( 2, 3, 2 ))


###################################################
### code chunk number 46: gRbase-arrays.Rnw:639-640
###################################################
nextCell(c(2,3,1), dim=c( 2, 3, 2 ))


###################################################
### code chunk number 47: gRbase-arrays.Rnw:658-660
###################################################
nextCellSlice(c(1,3,1), sliceset=2, dim=c( 2, 3, 2 ))
nextCellSlice(c(2,3,1), sliceset=2, dim=c( 2, 3, 2 ))


###################################################
### code chunk number 48: gRbase-arrays.Rnw:668-669
###################################################
slice2entry(slicecell=3, sliceset=2, dim=c( 2, 3, 2 ))


###################################################
### code chunk number 49: gRbase-arrays.Rnw:675-678
###################################################
r <- slice2entry(slicecell=3, sliceset=2, dim=c( 2, 3, 2 ))
lapply(lapply(r, entry2cell, c( 2, 3, 2 )),
       cell2name, dimnames(hec))


###################################################
### code chunk number 50: gRbase-arrays.Rnw:764-765
###################################################
head( factGrid( c(2, 3, 2) ), 6 )


###################################################
### code chunk number 51: gRbase-arrays.Rnw:771-772
###################################################
head( expand.grid(list(1:2,1:3,1:2)), 6 )


