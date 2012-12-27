\name{is.DAG}
\alias{is.DAG}
\alias{is.DAG.matrix}
\alias{is.DAG.Matrix}
\alias{is.DAGMAT}
\alias{is.DAG.graphNEL}

\alias{is.UG}
\alias{is.UG.matrix}
\alias{is.UG.Matrix}
\alias{is.UGMAT}
\alias{is.UG.graphNEL}

\alias{is.TUG}
\alias{is.TUG.matrix}
\alias{is.TUG.Matrix}
\alias{is.TUGMAT}
\alias{is.TUG.graphNEL}

\alias{is.adjMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Check properties of graphs.
}
\description{
  Check if a graph is 1) a directed acyclic graph (DAG), 2) an undirected
  graph (UG), 3) a triangulated (chordal) undirected graph (TUG). A
  graph represented as 1) graphNEL (from the graph  package), 2) an adjacency
  matrix, 3) a sparse adjacency matrix (a dgCMatrix from the Matrix
  package).  
}
\usage{
is.DAG(object)
is.UG(object)
is.TUG(object)
is.adjMAT(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A graph represented as 1) graphNEL (from the graph
    package), 2) an adjacency
    matrix, 3) a sparse adjacency matrix (a dgCMatrix from the Matrix
    package). }
  \item{x}{Any object. If the object is a quadratic matrix with 0's on
    the diagonal then it is an adjacency matrix.}
}

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{dag}},
  \code{\link{ug}}
}
\examples{

## DAGs
dagNEL  <- dag(~a:b:c+c:d:e, result="NEL")
dagMAT  <- dag(~a:b:c+c:d:e, result="matrix")
dagMATS <- dag(~a:b:c+c:d:e, result="Matrix")

## Undirected graphs
ugNEL  <- ug(~a:b:c+c:d:e, result="NEL")
ugMAT  <- ug(~a:b:c+c:d:e, result="matrix")
ugMATS <- ug(~a:b:c+c:d:e, result="Matrix")

## Is it a DAG?
is.DAG(dagNEL)
is.DAG(dagMAT)
is.DAG(dagMATS)

is.DAG(ugNEL)
is.DAG(ugMAT)
is.DAG(ugMATS)

## Is it an undirected graph
is.UG(dagNEL)
is.UG(dagMAT)
is.UG(dagMATS)

is.UG(ugNEL)
is.UG(ugMAT)
is.UG(ugMATS)

## Is it a triangulated (i.e. chordal)  undirected graph
is.TUG(dagNEL)
is.TUG(dagMAT)
is.TUG(dagMATS)

is.TUG(ugNEL)
is.TUG(ugMAT)
is.TUG(ugMATS)

## Example where the graph is not triangulated
ug2NEL  <- ug(~a:b+b:c+c:d+d:a, result="NEL")
ug2MAT  <- ug(~a:b+b:c+c:d+d:a, result="matrix")
ug2MATS <- ug(~a:b+b:c+c:d+d:a, result="Matrix")

is.TUG(ug2NEL)
is.TUG(ug2MAT)
is.TUG(ug2MATS)

## Bidirected graphs
edgemode(ugNEL)
edgemode(ugNEL) <- "directed"
edgemode(ugNEL)
is.DAG(ugNEL)
is.UG(ugNEL)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
