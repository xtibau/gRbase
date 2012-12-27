\name{edgeList}
\alias{edgeList}
\alias{edgeList.graphNEL}
\alias{edgeList.matrix}
\alias{edgeListMAT}
\alias{nonEdgeList}
\alias{nonEdgeList.graphNEL}
\alias{nonEdgeList.matrix}
\alias{nonEdgeListMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Find edges in a graph and edges not in a graph.
}
\description{
  Returns the edges of a graph (or edges not in a graph) where the graph
  can be either a graphNEL object or an adjacency matrix.
}
\usage{
edgeList(object, matrix = FALSE)
\method{edgeList}{graphNEL}(object, matrix = FALSE)
\method{edgeList}{matrix}(object, matrix = FALSE)
nonEdgeList(object, matrix = FALSE)
\method{nonEdgeList}{graphNEL}(object, matrix = FALSE)
\method{nonEdgeList}{matrix}(object, matrix = FALSE)
edgeListMAT(adjmat, matrix = FALSE, edgemode="undirected")
nonEdgeListMAT(adjmat, matrix = FALSE)
}
\arguments{
  \item{object}{
    A graphNEL object or an adjacency matrix.
  }
  \item{adjmat}{
    An adjacency matrix.
  }
  \item{matrix}{
    If TRUE the result is a matrix; otherwise the result is a list.
  }
  \item{edgemode}{
    Can be "directed" or "undirected".
  }
  
}
\details{
  If object is a matrix, then edgeList() checks if object is
  symmetrical. If so it is assumed that the graph is undirected;
  otherwise the graph is assumed to be directed.

  The workhorse is edgeListMAT.
}

\value{
  A list or a matrix with edges.
}
\author{  Søren Højsgaard, \email{sorenh@math.aau.dk} }
  

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %\code{\link{edgeList}}
  %\code{\link{nonEdgeList}}
  \code{\link{as.adjMAT}}
  \code{\link{mcs}}
  \code{\link{rip}}
  \code{\link{moralize}}
  \code{\link{jTree}}

}

\examples{
## A graph with edges
g  <- ug(~a:b+b:c+c:d)
gm <- as.adjMAT(g)

edgeList(g)
edgeList(gm)
edgeListMAT(gm)

edgeList(g, matrix=TRUE)
edgeList(gm, matrix=TRUE)
edgeListMAT(gm, matrix=TRUE)

nonEdgeList(g)
nonEdgeList(gm)
nonEdgeListMAT(gm)

## A graph without edges
g  <- ug(~a+b+c)
gm <- as.adjMAT(g)

edgeList(g)
edgeList(gm)
edgeListMAT(gm)

edgeList(g, matrix=TRUE)
edgeList(gm, matrix=TRUE)
edgeListMAT(gm, matrix=TRUE)

nonEdgeList(g)
nonEdgeList(gm)
nonEdgeListMAT(gm)
}
\keyword{utilities}

