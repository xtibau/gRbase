  ## ####################################################################
  ## Interface   gRbase <-> dynamicGraph
  ## ####################################################################

UserMenus <- list(MainUser =
                  list(label = "Fit",
                       command = function(object, ...)
                       {
                         args <- list(...)
                         env <- args$Arguments
                         
                         object.new <- fit(object)
                         
                         env$redrawView(
                                        graphWindow=env$graphWindow,
                                        edgeList=env$edgeList,
                                        factorEdgeList=env$factorEdgeList,
                                        blockEdgeList=env$blockEdgeList,
                                        title = "Not used!",
                                        width = NULL, height = NULL,
                                        Arguments = env,
                                        object = object.new)
                         return(list(object=object.new))
                       },
                       update.vertices = TRUE,
                       update.edges = TRUE
                       ),
                  MainUser = list(label = "Label all edges", 
           command = function(object, ...) LabelAllEdges(object, 
                    slave = FALSE, ...)),

                  )



gRVariableDescription <- function(obj) {
  ## obj is an hllm object
  
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
    ## object is an hllmclass
    nodelabels <- varNames(gmData(object))
    form <- Formula(object)
    listform <- readf(form[2])
#              new.form <- add.edge(listform,c(name.1,name.2))
#              print(listform)
    from <- c()
    to <- c()
    for (i in 1:length(listform)) {
#                print(listform[[i]])
      edges <- selectOrder(listform[[i]])
      if (length(edges)==0) next
      edges.ul <- unlist(edges)
#      print(edges.ul) 
      from <- c(from,edges.ul[1:length(edges.ul)%%2==1])
#      print(1:length(edges.ul)%%2)
#      print((1:length(edges.ul)+1)%%2)
      to <- c(to,edges.ul[(1:length(edges.ul)+1)%%2==1])
#      print(c(1:length(nodelabels))[unlist(edges)==nodelabels])
    }
#    object.UG <- as.UG(Formula(object))
#    edgemat <- allEdges(object.UG)


#      print(from)
#      print(to)
    
    from <- match(from,nodelabels)
    to   <- match(to,nodelabels)
    edgemat <- cbind(from,to)

#      print(from)
#      print(to)

#    print(edgemat)
    
#    labels.gR <- varNames(gmData(object))    
#    labels <- rownames(object.UG)

    
    ## convert ggm-edges to gR-edges
#    ggm2gr <- function(edge) match(labels[edge],labels.gR)
    
    if (length(edgemat)==0) return(matrix(nrow=0,ncol=2))
    
#    return(t(apply(edgemat,1,ggm2gr)))
#    print(edgemat)
        return(edgemat)
  }

dynamic.gR.Graph <-  function(object, ...)
{

#  require(dynamicGraph)
#  .Load.gRbase.dynamic()
    
    if (inherits(object,"gmData")) 
      object <- new("hllm",~.^.,object)

  VariableDescription <- gRVariableDescription(obj = object)

   if (!is.graphical(readf(Formula(object)[2]))) {
     cat("Model not graphical, using factorgraph\n")
     ## factor-edges, factorgraph
#     FactorEdges <- gRFactorEdges(object = object)
     Edges<-NULL
     FactorEdges <-     readf(Formula(object)[2])
   }
  else {
    Edges <- gREdges(object = object)
    FactorEdges <- NULL
  }

#    print(VariableDescription)
#    print(Edges)
#    print(FactorEdges)
    
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
                
