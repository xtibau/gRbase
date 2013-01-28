\name{getCliques}
\alias{getCliques}
\alias{getCliques.graphNEL}
\alias{getCliques.default}
\alias{maxCliqueMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get cliques of an undirected graph}
\description{
  Return a list of (maximal) cliques of an undirected graph.
}
\usage{
getCliques(object)
maxCliqueMAT(amat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An undirected graph represented either as a
    \code{graphNEL} object, a (dense) \code{matrix}, a (sparse) \code{dgCMatrix}}
  \item{amat}{An adjacency matrix}
}

\details{
  I graph theory, a clique is often a complete subset of a graph. A
  maximal clique is a clique which can not be enlarged. In statistics
  (and that is the convention we follow here) a clique is usually
  understood  to be a maximal clique.
  
  Finding the cliques of a general graph is an NP complete
  problem. Finding the cliques of triangualted graph is linear in the
  number of cliques.

  The workhorse is the \code{maxCliqueMAT} function which calls the
  \code{maxClique} function in the \code{RBGL} package.  
}

\value{
  A list
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
% \note{
% }
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
getCliques(uG1)

## adjacency matrix
uG2 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="matrix")
getCliques(uG2)

## adjacency matrix (sparse)
uG3 <- ug(~a:b+b:c+c:d+d:e+e:f+f:a, result="Matrix")
getCliques(uG3)

}
\keyword{utilities}

