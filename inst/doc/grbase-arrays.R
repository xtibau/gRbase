## ----include=FALSE------------------------------------------------------------
library(knitr)
opts_chunk$set(
tidy=FALSE
)

## ----echo=FALSE---------------------------------------------------------------
#require( gRbase )
prettyVersion <- packageDescription("gRbase")$Version
prettyDate <- format(Sys.Date())

## ----echo=F---------------------------------------------------------------------------------------
library(gRbase)
options("width"=100, "digits"=4)
options(useFancyQuotes="UTF-8")
#chk = 'markup'
chk = 'hide'

## -------------------------------------------------------------------------------------------------
hec <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29) 
dim(hec) <- c(2, 3, 2)
dimnames(hec) <- list(Hair = c("Black", "Brown"), 
                      Eye = c("Brown", "Blue", "Hazel"), 
                      Sex = c("Male", "Female"))
hec

## -------------------------------------------------------------------------------------------------
is.named.array( hec )

## -------------------------------------------------------------------------------------------------
flat <- function(x) {ftable(x, row.vars=1)}
hec %>% flat

## -------------------------------------------------------------------------------------------------
z1 <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29)
di <- c(2, 3, 2)
dn <- list(Hair = c("Black", "Brown"), 
           Eye = c("Brown", "Blue", "Hazel"), 
           Sex = c("Male", "Female"))
dim( z1 ) <- di
dimnames( z1 ) <- dn
z2 <- array( c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29),
            dim=di, dimnames=dn)

## -------------------------------------------------------------------------------------------------
counts <- c(32, 53, 11, 50, 10, 25, 36, 66, 9, 34, 5, 29)
z3 <- ar_new( ~ Hair:Eye:Sex, levels = dn, value = counts) 
z4 <- ar_new(c("Hair", "Eye", "Sex"), levels=dn, values=counts)

## -------------------------------------------------------------------------------------------------
z5 <- ar_new(~Hair:Eye:Sex, levels=c(2, 3, 2), values = counts)
z5 %>% flat

## -------------------------------------------------------------------------------------------------
z6 <- ar_new(~Hair:Eye:Sex, levels=c(2, 3, 2), values = counts, normalize="first")
z6 %>% flat

## -------------------------------------------------------------------------------------------------
ar_normalize(z5, "first") %>% flat
ar_normalize(z5, "all") %>% flat

## -------------------------------------------------------------------------------------------------
ar_slice(hec, slice=list(Eye=c("Blue", "Hazel")))  %>% flat

## ----results=chk----------------------------------------------------------------------------------
ar_slice(hec, slice=list(Eye=2:3, Sex="Female"))

## -------------------------------------------------------------------------------------------------
# 2 x 3 array : 
ar_slice(hec, slice=list(Sex="Female")) %>% flat
# 2 x 3 x 1 array :
ar_slice(hec, slice=list(Sex="Female"), drop=FALSE) %>% flat

## ----results=chk----------------------------------------------------------------------------------
## A vector:
z <- ar_slice(hec, slice=list(Hair=1, Sex="Female")); z 
## A 1-dimensional array:
z <- ar_slice(hec, slice=list(Hair=1, Sex="Female"), as.array=TRUE); z

## -------------------------------------------------------------------------------------------------
hec[, 2:3, ]  %>% flat  ## A 2 x 2 x 2 array
hec[1, , 1]             ## A vector
hec[1, , 1, drop=FALSE] ## A 1 x 3 x 1 array

## ----results=chk----------------------------------------------------------------------------------
do.call("[", c(list(hec), list(TRUE, 2:3, TRUE)))  %>% flat
do.call("[", c(list(hec), list(1, TRUE, 1))) 
do.call("[", c(list(hec), list(1, TRUE, 1), drop=FALSE)) 

## ----results=chk----------------------------------------------------------------------------------
ar_slice_prim(hec, slice=list(TRUE, 2:3, TRUE))  %>% flat
ar_slice(hec, slice=list(c(2, 3)), margin=2) %>% flat

ar_slice_prim(hec, slice=list(1, TRUE, 1))  
ar_slice(hec, slice=list(1, 1), margin=c(1,3)) 

ar_slice_prim(hec, slice=list(1, TRUE, 1), drop=FALSE)  
ar_slice(hec, slice=list(1, 1), margin=c(1,3), drop=FALSE) 

## -------------------------------------------------------------------------------------------------
he <- hec %a_% ~Hair:Eye; he %>% flat       

## ----results=chk----------------------------------------------------------------------------------
## Alternatives
he <- ar_marg(hec, ~Hair:Eye); he
hs <- ar_marg(hec, c("Hair", "Sex"))
es <- ar_marg(hec, c(2, 3))

## -------------------------------------------------------------------------------------------------
she <- he %a^% list(Sex=c("Male", "Female"))
she %>% flat

## ----results=chk----------------------------------------------------------------------------------
## Alternatives
she <- ar_expand(he, list(Sex=c("Male", "Female")))
ar_expand(he, dimnames(hs)) %>% flat
ar_expand(he, hs) %>% flat

## -------------------------------------------------------------------------------------------------
ar_perm(hec, ~Eye:Sex:Hair) %>% flat 

## ----results=chk----------------------------------------------------------------------------------
ar_perm(hec, c("Eye", "Sex", "Hair")) 
ar_perm(hec, c(2,3,1)) 
ar_perm(hec, ~Ey:Se:Ha) 
ar_perm(hec, c("Ey", "Se", "Ha"))

## -------------------------------------------------------------------------------------------------
hec2 <- ar_perm(hec, 3:1)
hec %a==% hec2

## ----results=chk----------------------------------------------------------------------------------
## Alternative
ar_equal(hec, hec2)

## ----results=chk----------------------------------------------------------------------------------
hec2 <- ar_perm(hec, 3:1)
ar_align(hec2, hec)  
## ar_align(hec2, dimnames(hec))
## ar_align(hec2, names(dimnames(hec)))

## -------------------------------------------------------------------------------------------------
she <- he %a+% hs 
she %>% flat

## ----results=chk----------------------------------------------------------------------------------
he %a+% hs
he %a-% hs
he %a*% hs
he %a/% hs
he %a/0% hs ## Convention 0/0 = 0

## ----results=chk----------------------------------------------------------------------------------
ar_add(he, hs)  %>% flat
ar_subt(he, hs) %>% flat
ar_mult(he, hs) %>% flat
ar_div(he, hs)  %>% flat 
ar_div0(he, hs)  %>% flat ## Convention 0/0 = 0

## ----results=chk----------------------------------------------------------------------------------
ar_sum( he, hs, es )  
ar_prod( he, hs, es ) 

## ----results=chk----------------------------------------------------------------------------------
ar_sum_list( list(he, hs, es) )
ar_prod_list( list(he, hs, es) )

## -------------------------------------------------------------------------------------------------
ar_dist(hec) %>% flat
ar_dist(hec, marg=~Hair:Eye) %>% flat
ar_dist(hec, cond=~Eye) %>% flat
ar_dist(hec, marg=~Hair, cond=~Sex) %>% flat

## -------------------------------------------------------------------------------------------------
ar_slice_mult(hec, list(Sex="Female"), val=10, comp=0) %>% flat

## -------------------------------------------------------------------------------------------------
yn <- c("y","n")
lev <- list(rain=yn, sprinkler=yn, wet=yn)
r <- ar_new( ~rain, levels = lev, values = c(.2, .8) )
s_r <- ar_new( ~sprinkler:rain, levels = lev, values = c(.01, .99, .4, .6) )
w_sr <- ar_new( ~wet:sprinkler:rain, levels = lev, 
             values = c(.99, .01, .8, .2, .9, .1, 0, 1))
r 
s_r  %>% flat
w_sr %>% flat

## -------------------------------------------------------------------------------------------------
joint <- ar_prod( r, s_r, w_sr ); joint %>% flat

## -------------------------------------------------------------------------------------------------
ar_dist(joint, marg=~rain, cond=~wet)

## ----results='hide'-------------------------------------------------------------------------------
## Alternative:
rw <- ar_marg(joint, ~rain + wet)
ar_div( rw, ar_marg(rw, ~wet))
## or
rw %a/% (rw %a_% ~wet)

## -------------------------------------------------------------------------------------------------
## Alternative:
x <- ar_slice_mult(rw, slice=list(wet="y")); x
ar_dist(x, marg=~rain)

## -------------------------------------------------------------------------------------------------
data( lizard, package="gRbase" )
lizard %>% flat

## -------------------------------------------------------------------------------------------------
myips <- function(indata, glist){
    fit   <- indata
    fit[] <-  1
    ## List of sufficient marginal tables
    md    <- lapply(glist, function(g) ar_marg(indata, g))

    for (i in 1:4){
        for (j in seq_along(glist)){
            mf  <- ar_marg(fit, glist[[j]])
            # adj <- ar_div( md[[ j ]], mf)
            # fit <- ar_mult( fit, adj )
            ## or
            adj <- md[[ j ]] %a/% mf
            fit <- fit %a*% adj
        }
    }
    pearson <- sum( (fit - indata)^2 / fit)
    list(pearson=pearson, fit=fit)
}

glist <- list(c("species","diam"),c("species","height"),c("diam","height"))

fm1 <- myips( lizard, glist )
fm1$pearson
fm1$fit %>% flat

fm2 <- loglin( lizard, glist, fit=T )
fm2$pearson
fm2$fit %>% flat

## -------------------------------------------------------------------------------------------------
hec
c(hec)

## -------------------------------------------------------------------------------------------------
cell2name <- function(cell, dimnames){
    unlist(lapply(1:length(cell), function(m) dimnames[[m]][cell[m]]))
}
cell2name(c(2,3,1), dimnames(hec))

## -------------------------------------------------------------------------------------------------
cell2entry(c(2,3,1), dim=c( 2, 3, 2 ))
entry2cell(6, dim=c( 2, 3, 2 ))

## -------------------------------------------------------------------------------------------------
next_cell(c(2,3,1), dim=c( 2, 3, 2 ))

## -------------------------------------------------------------------------------------------------
next_cell_slice(c(1,3,1), slice_marg=2, dim=c( 2, 3, 2 ))
next_cell_slice(c(2,3,1), slice_marg=2, dim=c( 2, 3, 2 ))

## -------------------------------------------------------------------------------------------------
slice2entry(slice_cell=3, slice_marg=2, dim=c( 2, 3, 2 ))

## -------------------------------------------------------------------------------------------------
r <- slice2entry(slice_cell=3, slice_marg=2, dim=c( 2, 3, 2 ))
lapply(lapply(r, entry2cell, c( 2, 3, 2 )),
       cell2name, dimnames(hec))

## -------------------------------------------------------------------------------------------------
head( fact_grid( c(2, 3, 2) ), 6 )

## -------------------------------------------------------------------------------------------------
head( expand.grid(list(1:2, 1:3, 1:2)), 6 )

