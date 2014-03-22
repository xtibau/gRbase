\name{vpar}
\alias{vpar}
\alias{vpar.matrix}
\alias{vpar.Matrix}
\alias{vpar.graphNEL}
\alias{vparMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  List of vertices and their parents for graph.
}
\description{
  Get list of vertices and their parents for graph.
}
\usage{
vpar(object, getv = TRUE, forceCheck = TRUE)
\method{vpar}{graphNEL}(object, getv = TRUE, forceCheck = TRUE)
\method{vpar}{matrix}(object, getv = TRUE, forceCheck = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object representing a graph. Valid objects are an
    adjacency matrix or as a graphNEL.
}
\item{getv}{
  The result is by default a list of vectors of the form
  \code{(v, pa1, pa2, ... paN)} where \code{pa1, pa2, ... paN} are the
  parents of \code{v}. If \code{getv} is \code{FALSE} then the vectors
  will have the form \code{(pa1, pa2, ... paN)}.
}
\item{forceCheck}{
  Logical indicating if it should be checked that the object is a DAG.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  A list of vectors where each vector will have the form \code{(v, pa1,
    pa2, ... paN)} where \code{pa1, pa2, ... paN} are the parents of \code{v}.
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{dag}}
  \code{\link{ug}}
}
\examples{

## DAGs
dagMAT <- dag(~a:b:c+c:d:e, result="matrix")
dagNEL <- dag(~a:b:c+c:d:e, result="NEL")

vpar(dagMAT)
vpar(dagNEL)
vpar(dagMAT, getv=FALSE)
vpar(dagNEL, getv=FALSE)

## Undirected graphs
ugMAT <- ug(~a:b:c+c:d:e, result="matrix")
ugNEL <- ug(~a:b:c+c:d:e, result="NEL")

\dontrun{
## This will fail because the adjacency matrix is symmetric and the
## graphNEL has undirected edges
vpar(ugMAT)
vpar(ugNEL)
}

## When forceCheck is FALSE, it will not be detected that the graphs are undirected.
vpar(ugMAT, forceCheck=FALSE)
vpar(ugNEL, forceCheck=FALSE)

## Bidirected graphs
## This is, for graphNEL's, the same as working with bidirected edges:
if (require(graph)){
graph::edgemode(ugNEL)
graph::edgemode(ugNEL) <- "directed"
graph::edgemode(ugNEL)
vpar(ugNEL,FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
