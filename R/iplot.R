
iplot <- function(x,...){
  UseMethod("iplot")
}



iplot.graphNEL <- function(x,...){
  ig <- igraph.from.graphNEL(x)
  V(ig)$label <- V(ig)$name
  V(ig)$size  <- 50
  ig$cex   <-  4
                                        #ig$layout   <- layout.graphopt
                                        #ig$layout <- layout.kamada.kawai
  ig$layout <- layout.lgl
  plot(ig,
       vertex.label.family="Helvetica",
       edge.label.family="Helvetica", vertex.label.cex=2,
       edge.label.cex=2)
}
