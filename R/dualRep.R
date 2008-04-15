
## SHD version of DED's dual rep
##
## Based on faster set operations
##
dual.rep <- function(glist, S, minimal=TRUE) {
  ## S is total varset - often but by no means always given by unique(unlist(g.list)) 
  list.save <- list()
  ##if (length(glist)==0) list.save <- list(S)
  if (length(glist)==1 & is.logical(glist[[1]]))
    list.save <- list(S)
  else { 
    for (v in 1:length(glist)) {
      m1 <- list.save
      if (minimal)
        m2 <- as.list(setdiffPrim(S,glist[[v]]))
      else
        m2 <- as.list(glist[[v]])
      
      if (v==1)
       list.save <- m2
      else {
        ##aaa <- unlist(lapply(m1, function(g)
        ##                     lapply(m2, union, g)),recursive=FALSE)
        aaa <- unlist(lapply(m1, function(g)
                             lapply(m2, function(oo){uniquePrim(c(oo, g))})),
                      recursive=FALSE)
        list.save <- remove.redundant(aaa,FALSE)
      }
    }
    if (!minimal)
      list.save <- lapply(list.save, function(g) setdiffPrim(S,g))}
  list.save
}  
