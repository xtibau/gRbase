## Find max cliques from adjacency matrix
##
maxCliqueMAT <- function(amat)
{
  ## Notice: No checks are made for whether the graph is undirected
##   which.arr.ind2<-function(m){
##     nr  <- nrow(m)
##     nc  <- ncol(m)
##     rr <- rep.int(1:nr, nc)
##     cc <- rep(1:nc, each=nr)
##     rbind(rr[m!=0L], cc[m!=0L])
##   }
  
  #vn <- colnames(amat)
  vn <- dimnames(amat)[[2L]]
  nv <- ncol(amat)

##   print(class(amat))
##   print(amat)
##   print(amat !=0)

  if(class(amat)=="dgCMatrix"){
    ##em <- t(which((amat!=0), arr.ind=TRUE))
    ##em <- t(sp_fromto(amat))
    em <- t.default(sp_fromto(amat)) ## FIXME: Should be possible to transpose on cpp side
    #em <- sp_fromto(amat)
    #em <- rbind(em[,1],em[,2])
  } else {
    #em <- which.arr.ind2(amat)
    em <- t.default(st_fromto(amat)) ## FIXME: Should be possible to transpose on cpp side
  }
  
  ne <- ncol(em)
  
  ans <- .Call("maxClique", 
               as.integer(nv), as.integer(ne), as.integer(em-1), 
               PACKAGE="RBGL")
  
  ans_names <- lapply(ans, function(x) { vn[x] }) ## FIXME: An index argument could control this
  list("maxCliques"=ans_names) ## FIXME: Must the result really be a list of lists?
}
