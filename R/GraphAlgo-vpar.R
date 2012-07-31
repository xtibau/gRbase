


vpar <- function(object, forceCheck=TRUE){

  type <- which(inherits(object, c("graphNEL","matrix"), which=TRUE)>0)
  switch(type,
         "1"={
           ##cat("graphNEL\n")
           if (forceCheck && edgemode(object)=="undirected")
             stop("Graph is undirected; (v,pa(v)) does not exist...\n")
           ee <- graph::edges(object)
           vn <- names(ee)
           tf <- do.call(rbind, # matrix in to-from form
                         lapply(1:length(ee),
                                function(ii) names2pairs( ee[[ii]], vn[ii],
                                                         sort=FALSE, result="matrix")))
           
           ans <- lapply(1:length(vn), function(ii) c(vn[ii], tf[tf[,1]==vn[ii],2]))
           ## FIXME: Probably faster to apply topoSort to ans rather than
           ## checking if the graph is undirected...
           names(ans) <- vn
           return(ans)
         },
         "2"={
           ##cat("matrix\n")
           if(forceCheck && sum(abs(object-t(object))>100 * .Machine$double.eps)==0)
             stop("Graph is undirected; (v,pa(v)) does not exist...\n")

           vn   <- rownames(object) 
           ans  <- vector("list", length(vn))
           for (ii in seq_along(vn)){
             ans[[ii]] <- c(vn[ii], vn[object[,ii]>0])
           }
           names(ans) <- vn
           return(ans)
         },
         {stop("function 'vpar()' only implemented for graphNELs and adjacency matrices")
        }
       )
}



## adjmat based
## vpar <- function(object){
##   if (edgemode(object)=="undirected")
##     stop("Graph is undirected; (v,pa(v)) does not exist...\n")
##   amat <- as.adjMAT(object)
##   vn   <- rownames(amat) 
##   ans  <- vector("list", length(vn))
##   for (ii in seq_along(vn)){
##     ans[[ii]] <- c(vn[ii], vn[amat[,ii]>0])
##   }
##   names(ans) <- vn
##   return(ans)
## }
