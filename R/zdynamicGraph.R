##                              -*- Mode: Ess -*- 
##zdynamicGraph.R --- 
##Author          : Claus Dethlefsen
##Created On      : Mon May 02 09:39:23 2005
##Last Modified By: 
##Last Modified On: 
##Update Count    : 0
##Status          : Unknown, Use with caution!
##

  ## ####################################################################
  ## Interface   gRbase <-> dynamicGraph
  ## ####################################################################

UserMenus <- c(UserMenus,list(MainUser =
                              list(label = "Fit",
                       command = function(object, ...)
                       {
                         args <- list(...)
                         env <- args$Arguments
                         
                         object.new <- fit(object)
                         
#                         env$redrawView(
#                                        graphWindow=env$graphWindow,
#                                        edgeList=env$edgeList,
#                                        factorEdgeList=env$factorEdgeList,
#                                        blockEdgeList=env$blockEdgeList,
#                                        title = "Not used!",
#                                        width = NULL, height = NULL,
#                                        Arguments = env,
#                                        object = object.new)
    Edges <- gREdges(object = object.new)

    two.to.pairs <- function(from, to) {
        edge.list <- vector("list", length(to))
        for (j in seq(along = to)) edge.list[[j]] <- c(from[j], 
            to[j])
        return(edge.list)
    }
    ArgEdges <-   
          returnEdgeList(edge.list=two.to.pairs(Edges[,1],Edges[,2]),
                         vertices=env$vertexList,
                         oriented=TRUE
                         )
                         DynamicGraph(addModel = FALSE,
                                      addView = FALSE,
                                      frameModels = env$frameModels, 
                                      frameViews = env$frameViews,
                                      graphWindow = env$graphWindow,
                                      edgeList = ArgEdges,
                                      oriented=FALSE,
                                      object = object.new,
                                      factorVertexList = env$factorVertexList, 
                                      factorEdgeList =  env$FactorEdgeList,
                                      blockEdgeList = env$BlockEdgeList, 
                                      title = "Fitted object", Arguments = env)
                         
                         return(list(object=object.new))
                       },
                       update.vertices = TRUE,
                       update.edges = TRUE
                       ),
                  MainUser = list(label = "Label all edges", 
           command = function(object, ...) LabelAllEdges(object, 
                    slave = FALSE, ...)),

                  ))



gRVariableDescription <- function(obj) {
  
  object <- gmData(obj)
  
  return(list(
              names = as.character(varNames(object)),
              labels = NULL,
              types = as.character(varTypes(object)),
              levels = valueLabels(object)
              )
         )
}

gREdges <- function(object)
  {
    nodelabels <- varNames(gmData(object))
    form <- formula(object)
    listform <- readf(form[2])
    from <- c()
    to <- c()
    for (i in 1:length(listform)) {
      edges <- selectOrder(listform[[i]])
      if (length(edges)==0) next
      edges.ul <- unlist(edges)
      from <- c(from,edges.ul[1:length(edges.ul)%%2==1])
      to <- c(to,edges.ul[(1:length(edges.ul)+1)%%2==1])
    }
    
    from <- match(from,nodelabels)
    to   <- match(to,nodelabels)
    edgemat <- cbind(from,to)

    if (length(edgemat)==0) return(matrix(nrow=0,ncol=2))
    
        return(edgemat)
  }

dynamic.gR.Graph <-  function(object, ...)
{

  VariableDescription <- gRVariableDescription(obj = object)

   if (!is.graphical(readf(formula(object)[2]))) {
     cat("Model not graphical, using factorgraph\n")
     ## factor-edges, factorgraph
#     FactorEdges <- gRFactorEdges(object = object)
     Edges<-NULL
     FactorEdges <-     readf(formula(object)[2])
   }
  else {
    Edges <- gREdges(object = object)
    FactorEdges <- NULL
  }

    Z <- DynamicGraph(names = VariableDescription$names,
                      types = VariableDescription$types,
                      from = Edges[,1], to = Edges[,2],
                      factors=FactorEdges,
                      oriented = FALSE, 
                      object = object,
                      UserMenus = UserMenus,
                      ...)
  invisible(Z)
  }

LabelAllEdges <- function(object, slave = FALSE, 
                ...) {
                args <- list(...)
                Args <- args$Arguments
                getNodeName <- function(index, type) if (type == 
                  "Vertex") 
                  name(Args$vertexList[[index]])
                else if (type == "Factor") 
                  name(Args$factorVertexList[[abs(index)]])
                else if (type == "Block") 
                  label(Args$blockList[[abs(index)]])
                else NULL
                visitEdges <- function(edges) {
                  for (i in seq(along = edges)) {
                    vertices <- nodeIndicesOfEdge(edges[[i]])
                    types <- nodeTypesOfEdge(edges[[i]])
                    name.f <- getNodeName(vertices[1], types[1])
                    name.t <- getNodeName(vertices[2], types[2])
                    R <- testEdge(object, action = "remove", 
                      name.1 = name.f, name.2 = name.t, from = vertices[1], 
                      to = vertices[2], from.type = types[1], 
                      to.type = types[2], edge.index = i, force = force, 
                      Arguments = Args)
                    if (!is.null(R)) {
                      if (TRUE || (hasMethod("label", class(R)))) 
                        label(edges[[i]]) <- label(R)
                      if (TRUE || (hasMethod("width", class(R)))) 
                        width(edges[[i]]) <- width(R)
                    }
                  }
                  return(edges)
                }
                edgeList <- visitEdges(Args$edgeList)
                factorEdgeList <- visitEdges(Args$factorEdgeList)
                blockEdgeList <- visitEdges(Args$blockEdgeList)
                if (slave) 
                  Args$redrawView(graphWindow = NULL, 
                    edgeList = edgeList, factorEdgeList = factorEdgeList, 
                    blockEdgeList = blockEdgeList, title = "A slave window", 
                    ...)
                else Args$redrawView(graphWindow = Args$graphWindow, 
                  edgeList = edgeList, factorEdgeList = factorEdgeList, 
                  blockEdgeList = blockEdgeList, title = "Not used!", 
                  width = NULL, height = NULL, Arguments = Args)
            }
                
