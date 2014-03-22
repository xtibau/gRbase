\name{rip}
\alias{rip}
\alias{rip.default}
%% \alias{rip.graphNEL}
%% \alias{rip.matrix}
%% \alias{rip.Matrix}
\alias{ripMAT}

\alias{jTree}
\alias{jTree.default}
%% \alias{jTree.graphNEL}
%% \alias{jTree.matrix}
%% \alias{jTree.Matrix}
\alias{jTreeMAT}


\alias{junctionTree}
\alias{junctionTree.default}
%% \alias{junctionTree.graphNEL}
%% \alias{junctionTree.matrix}
%% \alias{junctionTree.Matrix}
\alias{junctionTreeMAT}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create RIP ordering of the cliques of an undirected graph; create junction tree.
}
\description{
  A RIP (running intersection property) ordering of the cliques is also called a perfect ordering. If
  the graph is not chordal, then no such ordering exists.
}
\usage{
rip(object, root = NULL, nLevels = NULL)
\method{rip}{default}(object, root = NULL, nLevels = NULL)
%% \method{rip}{graphNEL}(object, root = NULL, nLevels = NULL)
%% \method{rip}{matrix}(object, root = NULL, nLevels = NULL)
%% \method{rip}{Matrix}(object, root = NULL, nLevels = NULL)
ripMAT(amat, root = NULL, nLevels = rep(2, ncol(amat)))

jTree(object, ...)
\method{jTree}{default}(object, nLevels=NULL, ...)
%% \method{jTree}{graphNEL}(object, nLevels=NULL, ...)
%% \method{jTree}{matrix}(object, nLevels=NULL, ...)
%% \method{jTree}{Matrix}(object, nLevels=NULL, ...)
jTreeMAT(amat, nLevels=rep(2,ncol(amat)), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    An undirected graph represented either as a
    \code{graphNEL} object, an \code{igraph}, a (dense) \code{matrix},
    a (sparse) \code{dgCMatrix}.
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
  \item{amat}{
    Adjacency matrix
  }
  \item{...}{Additional arguments; currently not used}
}
\details{
  The RIP ordering of the cliques of a decomposable (i.e. chordal) graph
  is obtained by first ordering the variables linearly
  with maximum cardinality search (by \code{mcs}). The root argument is
  transfered to \code{mcs} as a way of controlling which clique will be
  the first in the RIP ordering.

  The \code{jTree()} (and \code{jTree()})  (for "junction tree") is just
  a wrapper for a call of \code{triangulate()} followed by a call of \code{rip()}.
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
  The workhorse is the \code{ripMAT()} function.

  The \code{nLevels} argument to the \code{rip} functions has no
  meaning.
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
uG <- ug(~me:ve + me:al + ve:al + al:an + al:st + an:st)
mcs(uG)
rip(uG)
jTree(uG)

## Adjacency matrix
uG <- ug(~me:ve:al + al:an:st, result="matrix")
mcs(uG)
rip(uG)
jTree(uG)

## Sparse adjacency matrix
uG <- ug(c("me","ve","al"),c("al","an","st"), result="Matrix")
mcs(uG)
rip(uG)
jTree(uG)

## Non--decomposable graph
uG <- ug(~1:2+2:3+3:4+4:5+5:1)
mcs(uG)
rip(uG)
jTree(uG)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

