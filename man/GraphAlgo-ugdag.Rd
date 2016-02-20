\name{ug}

\alias{ug}
\alias{dag}
\alias{ugList}
\alias{dagList}

\alias{ugList2matrix}
\alias{ugList2dgCMatrix}
\alias{dagList2matrix}
\alias{dagList2dgCMatrix}

% \alias{ugMAT}
% \alias{ugListMAT}
% \alias{dagMAT}
% \alias{dagListMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create undirected and directed graphs}
\description{
  These functions are wrappers for creation of graphs as implemented by
  graphNEL objects in the \code{graph} package.
}
\usage{
ug(..., result="NEL")
dag(..., result="NEL", forceCheck=FALSE)
ugList(x, result="NEL")
dagList(x, result="NEL", forceCheck=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{A generating class for a graph, see examples below}
  \item{x}{A list containing a generating class for a graph, see
    examples below}
  \item{forceCheck}{Logical determining if it should be checked if the
    graph is acyclical. Yes, one can specify graphs with cycles using
    the \code{dag()} function.}
  \item{result}{The format of the graph. The possible choices are "NEL"
    (for a graphNEL object), "matrix" (for an adjacency matrix),
    "igraph" (for an igraph object), "Matrix" (for a sparse matrix).}
}
\value{
  Functions \code{ug()}, \code{dag()}, \code{ugList()} and
  \code{dagList()} can  return a \code{graphNEL}
  object, a sparse or dense adjacency matrix or an \code{igraph} object.
  %Functions ugMAT() and ugListMAT() return an adjacency matrix.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
% \note{ ~~further notes~~

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

## The following specifications of undirected graphs are equivalent:
uG1 <- ug(~a:b, ~a:c, ~b:c ~c:d)
uG2 <- ug(c("a","b"), c("a","c"), c("b","c"), c("c","d"))
uG3 <- ug(~a:b:c, ~c:d)
uG4 <- ug(c("a","b","c"), c("c","d"))
uG5 <- ug(~a:b:c + c:d)

graph::edges(uG1)
graph::nodes(uG1)

## The following specifications of directed acyclig graphs are equivalent:
daG1 <- dag(~a:b:c, ~b:c, ~c:d)
daG2 <- dag(c("a","b","c"), c("b","c"), c("c","d"))
daG3 <- dag(~a:b:c + b:c + c:d)

graph::edges(daG1)
graph::nodes(daG2)

## dag() allows to specify directed graphs with cycles:
daG4 <- dag(~a:b+b:c+c:a) # A directed graph but with cycles
## A check for acyclicity can be done with
## daG5 <- dag(~a:b+b:c+c:a, forceCheck=TRUE) 

## A check for acyclicity is provided by topoSort
topoSort( daG3 )
topoSort( daG4 )

## Different representations
uG6 <- ug(~a:b:c + c:d, result="graphNEL")  # default
uG6
uG7 <- ug(~a:b:c + c:d, result="NEL")       # same
uG7
uG8 <- ug(~a:b:c + c:d, result="matrix")    # dense matrix
uG8
uG9 <- ug(~a:b:c + c:d, result="dgCMatrix") # sparse matrix
uG9
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
