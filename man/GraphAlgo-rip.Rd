\name{rip}
\alias{rip}
\alias{rip.graphNEL}
\alias{rip.igraph}
\alias{rip.matrix}
\alias{rip.Matrix}
\alias{ripMAT}
\alias{print.ripOrder}
\alias{plot.ripOrder}

\alias{jTree}
\alias{jTree.graphNEL}
\alias{jTree.igraph}
\alias{jTree.matrix}

\alias{junctionTree}
\alias{junctionTree.graphNEL}
\alias{junctionTree.igraph}
\alias{junctionTree.matrix}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  RIP ordering and junction tree.
}
\description{
  A RIP (running intersection property) ordering of the cliques is also called a perfect ordering. If
  the graph is not chordal, then no such ordering exists. 
}
\usage{
rip(object, root = NULL, nLevels = NULL)
\method{rip}{graphNEL}(object, root = NULL, nLevels = NULL)
\method{rip}{igraph}(object, root = NULL, nLevels = NULL)
\method{rip}{matrix}(object, root = NULL, nLevels = NULL)
\method{rip}{Matrix}(object, root = NULL, nLevels = NULL)
ripMAT(amat, root = NULL, nLevels = NULL) 

jTree(object, ...)
\method{jTree}{graphNEL}(object, method="mcwh",nLevels=rep(2,length(nodes(object))),...)
\method{jTree}{igraph}(object, method="mcwh",nLevels=rep(2,length(V(object))),...)
\method{jTree}{matrix}(object, method="mcwh",nLevels=rep(2,ncol(object)),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    An undirected graph represented either as a 'graphNEL', a 'matrix' or
    an 'igraph'.
}
  \item{root}{
    A vector of variables. The first variable in the perfect ordering
    will be the first variable on 'root'. The ordering of the variables
    given in 'root' will be followed as far as possible. 
}
  \item{nLevels}{
    Typically, the number of levels of the variables
    (nodes) when these are discrete. Used in determining the
    triangulation using a "minimum clique weight heuristic". See section
    'details'.
  }
  \item{amat}{Adjacency matrix}
  \item{method}{The triangulation method, \code{"mcwh"} is a C
    implementation of a minimum clique weight heuristic,
    \code{"R"} is a corresponding R implementation (experimental)
  }
  \item{...}{Additional arguments; currently not used}
}
\details{
  The RIP ordering is obtained by first ordering the variables linearly
  with maximum cardinality search (by \code{mcs}). The root argument is
  transfered to \code{mcs} as a way of controlling which clique will be
  the first in the RIP ordering.

  The \code{jTree} (for "junction tree") is just a wrapper for a call of
  \code{triangulate} followed by a call of \code{rip}.  
}
\value{
  \code{rip} returns a list (an object of class \code{ripOrder}. A print method exists for
  such objects.)
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

\note{
  The workhorse is the \code{ripMAT} function.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{mcs}}
  \code{\link{triangulate}}
  \code{\link{moralize}}
  \code{\link{ug}}, 
  \code{\link{dag}}

}
\examples{
## graphNEL
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
rip(uG)

## igraph
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st, result="igraph")
rip(uG)

## adjacency matrix
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st, result="matrix")
rip(uG)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

