\name{moralize}
\alias{moralize}
\alias{moralize.default}
%% \alias{moralize.graphNEL}
%% \alias{moralize.matrix}
%% \alias{moralize.Matrix}
\alias{moralizeMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Moralize a directed acyclic graph
}
\description{
    Moralize a directed acyclic graph which means marrying parents and
    dropping directions
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
moralize(object, ...)
\method{moralize}{default}(object, result=NULL, ...)
%% \method{moralize}{graphNEL}(object, result="graphNEL", ...)
%% \method{moralize}{matrix}(object, result="matrix", ...)
%% \method{moralize}{Matrix}(object, result="Matrix", ...)
%%moralizeMAT(XX_)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    A directed acyclic graph represented either as a
    \code{graphNEL} object, an \code{igraph}, a (dense) \code{matrix},
    a (sparse) \code{dgCMatrix}.
  }
  \item{result}{The representation of the moralized graph.
    When NULL the representation will be the same as the input object.
  }
  \item{\dots}{
    Additional arguments, currently not used
  }
  %\item{XX_}{Adjacency matrix; either a dense matrix or a sparce dgCMatrix.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A moralized graph represented either as a 'graphNEL', a 'matrix' or a
  sparse 'dgCMatrix'.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\note{
  The workhorse is the \code{moralizeMAT} function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{mcs}}
  \code{\link{jTree}}
  \code{\link{rip}}
  \code{\link{ug}},
  \code{\link{dag}}
}
\examples{
daG <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
moralize(daG)

daG <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st, result="matrix")
moralizeMAT(daG)

if (require(igraph)){
M <- matrix(c(1,2,3,3), nrow=2)
G <- graph.edgelist(M)
G
V(G)$name
moralize(G)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

