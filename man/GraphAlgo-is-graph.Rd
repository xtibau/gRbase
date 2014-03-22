\name{is.DAG}
\alias{is.DAG}
\alias{is.DAG.matrix}
\alias{is.DAG.Matrix}
\alias{is.DAGMAT}
\alias{is.DAG.graphNEL}

\alias{is.DG}
\alias{is.DG.matrix}
\alias{is.DG.Matrix}
\alias{is.DGMAT}
\alias{is.DG.graphNEL}

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
  Check if a graph is 1) a directed acyclic graph (DAG), 2) a directed
  graph (DG), 3) an undirected
  graph (UG), 4) a triangulated (chordal) undirected graph (TUG). This
  is done for
  graphs represented as 1) graphNEL (from the graph  package), 2) an adjacency
  matrix, 3) a sparse adjacency matrix (a dgCMatrix from the Matrix
  package).
}
\usage{
is.DAG(object)
is.DG(object)
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

\details{
  A non-zero value at entry (i,j) in an adjacency matrix A for a graph
  means that there is an edge from i to j. If also (j,i) is non-zero
  there is also an edge from j to i. In this case we may think of a
  bidirected edge between i and j or we may think of the edge as being
  undirected.  We do not distinguish between undirected and bidirected
  edges in the gRbase package. On the other hand, graphNEL objects from
  the graph package makes such a distinction (the function
  \code{edgemode()} will tell if edges are "directed" or "undirected" in
  a graphNEL object).

  The function \code{is.UG()} checks if the adjacency
  matrix is symmetric (If applied to a graphNEL, the adjacency matrix is
  created and checked for symmetry.)

  The function \code{is.TUG()} checks if the graph is undirected and
  triangulated (also called chordal) by checking if the adjacency matrix is
  symmetric and the vertices can be given a perfect ordering using
  maximum cardinality seach.

  The function \code{is.DG()} checks if a graph is directed, i.e., that
  there are no undirected edges. This is done by computing the
  elementwise product of A and the transpose of A; if there are no
  non--zero entries in this product then the graph is directed.

  The function \code{is.DAG()} will
  return \code{TRUE} if all edges are directed and if there are no
  cycles in the graph. (This is checked by checking if the vertices in
  the graph can be given a topological ordering which is based on
  identifying an undirected edge with a bidrected edge).

  There is a special case, namely if the graph has no edges at all (such
  that the adjacency matrix consists only of zeros). Such a graph is
  both undirected, triangulated, directed and directed acyclic.
}

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
graph::edgemode(ugNEL)
graph::edgemode(ugNEL) <- "directed"
graph::edgemode(ugNEL)
is.DAG(ugNEL)
is.UG(ugNEL)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
