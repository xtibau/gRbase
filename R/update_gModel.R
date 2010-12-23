update.gModel <- function(object, addedge=NULL, dropedge=NULL, ...){

  if (!is.null(addedge))
    object <- addEdge.gModel(object, addedge[1], addedge[2])

  if (!is.null(dropedge))
    object <- dropEdge.gModel(object, dropedge[1], dropedge[2])

  return(object)
  

}
