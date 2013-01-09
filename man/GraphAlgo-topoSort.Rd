\name{topoSort}
\alias{topoSort}
\alias{topoSort.graphNEL}
\alias{topoSort.matrix}
\alias{topoSort.Matrix}
\alias{topoSortMAT}


\alias{topoSort_vparList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Topological sort of vertices in directed 
}
\description{
  A topological ordering of a directed graph is a linear
  ordering of its vertices such that, for every edge (u->v), u comes before
  v in the ordering. A topological ordering is possible if and only if the graph has
  no directed cycles, that is, if it is a directed acyclic graph
  (DAG). Any DAG has at least one topological ordering.
}
\usage{
topoSort(object, index = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    A graph represented as a \code{graphNEL} object or an adjacency
    matrix. An adjacency matrix can either be a standard matrix or a
    sparse \code{dgCMatrix} matrix (from the Matrix package).
}
  \item{index}{
    If FALSE, an ordering is returned if it exists and
    \code{character(0)} otherwise. If TRUE, the index of the variables
    in an adjacency matrix is returned and \code{-1} otherwise. 
    %%     ~~Describe \code{index} here~~
}
}
\value{
    If FALSE, an ordering is returned if it exists and
    \code{character(0)} otherwise. If TRUE, the index of the variables
    in an adjacency matrix is returned and \code{-1} otherwise. 
}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}  

%% ~Make other sections like Warning with
\section{Note}{
  The workhorse is the \code{topoSortMAT} function which takes an
  adjacency matrix as input
  }
\section{Warning }{
  Do not use \code{index=TRUE} when the input is a \code{graphNEL}
  object; the result is unpredictable. 
} 

\seealso{
  \code{\link{dag}},
  \code{\link{ug}}
}
\examples{
dagMAT  <- dag(~a:b:c+c:d:e, result="matrix")
dagMATS <- as(dagMAT, "Matrix")
dagNEL  <- dag(~a:b:c+c:d:e, result="NEL")

topoSort(dagMAT)
topoSort(dagMATS)
topoSort(dagNEL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
