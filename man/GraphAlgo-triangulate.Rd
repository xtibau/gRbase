\name{triangulate}
\alias{triangulate}
\alias{triangulate.graphNEL}
\alias{triangulate.igraph}
\alias{triangulate.matrix}
\alias{triangulateMAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Triangulation of an undirected graph}
\description{
  This function will triangulate an undirected graph by adding fillins. 
}
\usage{
triangulate(object, ...)
\method{triangulate}{graphNEL}(object, method="mcwh",  nLevels = rep(2,length(nodes(object))), result="graphNEL",...)
\method{triangulate}{igraph}(object, method="mcwh",  nLevels = rep(2,length(V(object))), result="igraph",...)
\method{triangulate}{matrix}(object, method="mcwh",  nLevels = rep(2,ncol(object)), result="matrix",...)
triangulateMAT(amat, method="mcwh", nLevels=rep(2,ncol(amat)), result=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An undirected graph represented either as a 'graphNEL', a 'matrix' or
    an 'igraph'.}
%  \item{amat}{An adjacency matrix (symmetrical)}
  \item{method}{Triangulation method. Either "mcwh" (minimum clique
    weight heuristic) which is implemented in C or "r" (an experimental
    R version)}
  \item{nLevels}{Typically, the number of levels of the variables
    (nodes) when these are discrete. Used in determining the
    triangulation using a "minimum clique weight heuristic". See section
    'details'.}
  \item{result}{The type (representation) of the result. Possible values
    are 'graphNEL', 'matrix', 'Matrix' and 'igraph'. Default is the same type as
    the input graph.
  
  }
  \item{...}{Additional arguments, currently not used.}
  \item{amat}{Adjacency matrix}
}
\details{
  The triangulation is made so as the total state space is kept low by
  applying a minimum clique weight heuristic. 
}
\value{
  A triangulated graph represented either as a 'graphNEL', a 'matrix' or
  an 'igraph'.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk}
\note{
  The workhorse is the \code{triangulateMAT} function.  
}
\seealso{
  \code{\link{ug}}
  \code{\link{dag}}
  \code{\link{mcs}}
  \code{\link{rip}}
  \code{\link{moralize}}
}
\examples{
## graphNEL
uG1 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a)
tuG1 <- triangulate(uG1)

## adjacency matrix
uG2 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="matrix")
tuG2 <- triangulate(uG2)

## igraph
uG3 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="igraph")
tuG3 <- triangulate(uG3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
