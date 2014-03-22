\name{mpd}
\alias{mpd}
\alias{mpd.default}
\alias{mpdMAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maximal prime subgraph decomposition}
\description{
  Finding a junction tree representation of the MPD (maximal prime
  subgraph decomposition) of an undirected graph
}
\usage{
mpd(object, tobject = minimalTriang(object), details = 0)
mpdMAT(amat, tamat = minimalTriangMAT(amat), details = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An undirected graph; a graphNEL object}
  \item{tobject}{Any minimal triangulation of object; a graphNEL object}
  \item{amat}{An undirected graph; a symmetric adjacency matrix}
  \item{tamat}{Any minimal triangulation of object; a symmetric adjacency matrix}
  \item{details}{The amount of details to be printed. }
}
\details{
  The maximal prime
  subgraph decomposition of a graph is the smallest subgraphs into which
  the graph can be decomposed.
}
\value{
  A list with components
  "nodes", "cliques", "separators",  "parents", "children",
  "nLevels". The component "cliques" defines the subgraphs.
}

\references{
  Kristian G. Olesen and Anders L. Madsen (2002): Maximal Prime Subgraph Decomposition of Bayesian
  Networks. IEEE TRANSACTIONS ON SYSTEMS, MAN AND CYBERNETICS, PART B:
  CYBERNETICS, VOL. 32, NO. 1, FEBRUARY 2002
}

\author{Clive Bowsher \email{C.Bowsher@statslab.cam.ac.uk} with modifications
  by Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{ ~~further notes~~
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

\seealso{
  \code{\link{mcs}},
  \code{\link{mcsMAT}},
  \code{\link{minimalTriang}},
  \code{\link{minimalTriangMAT}},
  \code{\link{rip}},
  \code{\link{ripMAT}},
  \code{\link{triangulate}},
  \code{\link{triangulateMAT}}
}

\examples{
## Maximal prime subgraph decomposition - a graphNEL object
g1 <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e)
if (interactive()) plot(g1)
x <- mpd(g1)

## Maximal prime subgraph decomposition - an adjacency matrix
g1m <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e, result="matrix")
if (interactive()) plot(as(g1m, "graphNEL"))
x <- mpdMAT(g1m)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

