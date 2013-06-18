\name{triangulate}
\alias{triangulate}
\alias{triangulate.graphNEL}
\alias{triangulate.matrix}
\alias{triangulate.Matrix}
\alias{triangulateMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Triangulation of an undirected graph}
\description{
  This function will triangulate an undirected graph by adding fill-ins. 
}
\usage{
triangulate(object, ...)
\method{triangulate}{graphNEL}(object, method="mcwh", nLevels = rep(2,length(nodes(object))), 
                               result="graphNEL",...)
\method{triangulate}{matrix}(object, method="mcwh", nLevels = rep(2,ncol(object)), result="matrix",...)
\method{triangulate}{Matrix}(object, method="mcwh", nLevels = rep(2,ncol(object)), result="Matrix",...)
triangulateMAT(amat, method="mcwh", nLevels=rep(2,ncol(amat)), result=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An undirected graph represented either as a
    \code{graphNEL} object, a (dense) \code{matrix}, a (sparse) \code{dgCMatrix}}
  % \item{amat}{An adjacency matrix (symmetrical)}

  \item{method}{Triangulation method. Only valid option is "mcwh" (minimum clique
    weight heuristic). See section   'details'.}

  \item{nLevels}{Typically, the number of levels of the
    variables (nodes) when these are discrete. Used in determining the
    triangulation using a "minimum clique weight heuristic". See section
    'details'.}

  \item{result}{The type (representation) of the result. Same possible
    representations as the input graph, and default is the same type as
    the input graph.  }

  \item{...}{Additional arguments, currently not used.}

  \item{amat}{Adjacency matrix; a (dense) \code{matrix}, or a (sparse)
    \code{dgCMatrix}. } 
}


\details{ The triangulation is made so as the total state space is kept
  low by applying a minimum clique weight heuristic: When a fill-in is
  necessary, the algorithm will search for an edge to add such that the
  complete set to be formed will have as small a state-space as
  possible. 

  If \code{nLevels}
  is the same for all nodes then the heuristic aims at keeping the
  clique sizes small.
}

\value{ A
  triangulated graph represented either as a \code{graphNEL}, a (dense)
  \code{matrix} or a (sparse) \code{dgCMatrix}.

}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\note{
  The workhorse is the \code{triangulateMAT} function.  
}
\seealso{
  \code{\link{ug}}
  \code{\link{dag}}
  \code{\link{mcs}},   \code{\link{mcsMAT}}
  \code{\link{rip}},   \code{\link{ripMAT}},
  \code{\link{moralize}},   \code{\link{moralizeMAT}}
}
\examples{
## graphNEL
uG1 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a)
tuG1 <- triangulate(uG1)

## adjacency matrix
uG2 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="matrix")
tuG2 <- triangulate(uG2)

## adjacency matrix (sparse)
uG2 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="Matrix")
tuG2 <- triangulate(uG2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
