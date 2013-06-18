\name{minimalTriang}
\alias{minimalTriang}
\alias{minimalTriangMAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Minimal triangulation of an undirected graph}

\description{
  An undirected graph uG is triangulated (or chordal) if it
  has no cycles of length >= 4 without a chord which is equivalent to that
  the vertices can be given a perfect ordering. Any undirected graph can
  be triangulated by adding edges to the graph, so called fill-ins which
  gives the graph TuG.  A triangulation TuG is minimal if no fill-ins can
  be removed without breaking the property that TuG is triangulated.
  
}
\usage{
minimalTriang(uG, TuG = triangulate(uG, method = "mcwh"), details = 0)
minimalTriangMAT(uGmat, TuGmat = triangulateMAT(uGmat, method = "mcwh"), details = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uG}{The undirected graph which is to be triangulated; a graphNEL
    object}
  \item{TuG}{Any triangulation of uG; a graphNEL object}
  \item{uGmat}{The undirected graph which is to be triangulated; a
    symmetric adjacency matrix}
  \item{TuGmat}{Any triangulation of uG; a
    symmetric adjacency matrix}
  \item{details}{The amount of details to be printed. }
}
\details{
  
  For a given triangulation TuG it may be so that
  some of the fill-ins are superflous in the sense that they can be
  removed from TuG without breaking the property that TuG is
  triangulated. The graph obtained by doing so is a minimal
  triangulation.

  Notice: A related concept is the minimum triangulation, which is the
  the graph with the smallest number of fill-ins. The minimum
  triangulation is unique. Finding the minimum
  triangulation is NP-hard.
}
\value{
  \code{minimalTriang()} returns a graphNEL object while
  \code{minimalTriangMAT()} returns an adjacency matrix.
}
\references{
  Kristian G. Olesen and Anders L. Madsen (2002): Maximal Prime Subgraph Decomposition of Bayesian
  Networks. IEEE TRANSACTIONS ON SYSTEMS, MAN AND CYBERNETICS, PART B:
  CYBERNETICS, VOL. 32, NO. 1, FEBRUARY 2002
}


\author{Clive Bowsher <C.Bowsher@statslab.cam.ac.uk> with modifications
  by Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{mpd}},\code{\link{rip}}, \code{\link{triangulate}} }
\examples{
## A graphNEL object
g1 <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e)
x <- minimalTriang(g1)

## g2 is a triangulation of g1 but it is not minimal
g2 <- ug(~a:b:e:f+b:c:d:e)
x<-minimalTriang(g1, TuG=g2)

## An adjacency matrix
g1m <- ug(~a:b+b:c+c:d+d:e+e:f+a:f+b:e, result="matrix")
x<-minimalTriangMAT(g1m)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
