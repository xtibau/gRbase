\name{moralize}
\alias{moralize}
\alias{moralize.graphNEL}
\alias{moralize.igraph}
\alias{moralize.matrix}
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
\method{moralize}{graphNEL}(object, result="graphNEL", ...)
\method{moralize}{igraph}(object, result="igraph", ...)
\method{moralize}{matrix}(object, result="matrix", ...)
moralizeMAT(amat, result="matrix")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
    A directed acyclic graph represented either as a 'graphNEL', a 'matrix' or
    an 'igraph'.
    %%     ~~Describe \code{object} here~~
  }
  \item{result}{The representation of the moralized graph} 
  \item{\dots}{
    Additional arguments, currently not used
  }
  \item{amat}{Adjacency matrix}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A moralized graph represented either as a 'graphNEL', a 'matrix' or
  an 'igraph'.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, sorenh@agrsci.dk}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

