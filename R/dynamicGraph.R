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
                         
                         env$redrawGraphWindow(
                                               env$graphLattice,
                                               env$graphWindow,
                                               env$edgeList,
                                               env$blockEdgeList,
                                               env$factorVertexList,
                                               env$factorEdgeList,
                                               env$visibleVertices,
                                               env$extraList,
                                               object = object.new,
                                               title = "Result from Fit",
                                               objectName = "currentObject",
                                               Transformation = NULL,  
                                               background = "white",
                                               vertexcolor = "black",
                                               w = 10, width = 400,  
                                               height = 400)                                     
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
    
    object.UG <- as.UG(Formula(object))
    edgemat <- allEdges(object.UG)
    
    labels <- rownames(object.UG)
    labels.gR <- varNames(gmData(object))
    
    ## convert ggm-edges to gR-edges
    ggm2gr <- function(edge) match(labels[edge],labels.gR)
    
    if (length(edgemat)==0) return(matrix(nrow=0,ncol=2))
    
    return(t(apply(edgemat,1,ggm2gr)))
  }

dynamic.gR.Graph <-  function(object, ...)
{

  require(dynamicGraph)
  .Load.gRbase.dynamic()
    
    if (inherits(object,"gmData")) 
      object <- new("hllm",.^.~1,object)
    
    VariableDescription <- gRVariableDescription(obj = object)
    
    Edges <- gREdges(object = object)

#    print(VariableDescription)
#    print(Edges)
    
    Z <- DynamicGraph(names = VariableDescription$names,
                      types = VariableDescription$types,
                      from = Edges[,1], to = Edges[,2],
#                      factors=object@numformula,
                      oriented = FALSE, 
                      object = object,
                      UserMenus = UserMenus,
                      ...)
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
                  Args$redrawGraphWindow(graphWindow = NULL, 
                    edgeList = edgeList, factorEdgeList = factorEdgeList, 
                    blockEdgeList = blockEdgeList, title = "A slave window", 
                    ...)
                else Args$redrawGraphWindow(graphWindow = Args$graphWindow, 
                  edgeList = edgeList, factorEdgeList = factorEdgeList, 
                  blockEdgeList = blockEdgeList, title = "Not used!", 
                  width = NULL, height = NULL, Arguments = Args)
            }
                
