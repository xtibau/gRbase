\name{mcs}
\alias{mcs}
\alias{mcs.default}
%% \alias{mcs.graphNEL}
%% \alias{mcs.matrix}
%% \alias{mcs.Matrix}
\alias{mcsMAT}

\alias{mcsmarked}
\alias{mcsmarked.default}
%% \alias{mcsmarked.graphNEL}
%% \alias{mcsmarked.matrix}
\alias{mcsmarkedMAT}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Maximum cardinality search on undirected graph.
}
\description{
  Returns (if it exists) a perfect ordering of the vertices in an
  undirected graph.
}
\usage{
mcs(object, root = NULL, index = FALSE)
\method{mcs}{default}(object, root = NULL, index = FALSE)
%% \method{mcs}{graphNEL}(object, root = NULL, index = FALSE)
%% \method{mcs}{matrix}(object, root = NULL, index = FALSE)
mcsMAT(amat, vn = colnames(amat), root = NULL, index = FALSE)

mcsmarked(object, discrete = NULL, index = FALSE)
\method{mcsmarked}{default}(object, discrete = NULL, index = FALSE)
%% \method{mcsmarked}{graphNEL}(object, discrete = NULL, index = FALSE)
%% \method{mcsmarked}{matrix}(object, discrete = NULL, index = FALSE)
mcsmarkedMAT(amat, vn = colnames(amat), discrete = NULL, index = FALSE)
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

  \item{discrete}{
    A vector indicating which of the nodes are discrete. See 'details'
    for more information.
  }

  \item{index}{
    If TRUE, then a permutation is returned
  }
  \item{amat}{Adjacency matrix}
  \item{vn}{Nodes in the graph given by adjacency matrix}
}
\details{
  An undirected graph is decomposable iff there exists a perfect
    ordering of the vertices. The maximum cardinality search algorithm returns a
    perfect ordering of the vertices if it exists and hence this
    algorithm provides a check for decomposability. The \code{mcs()}
    functions finds such an ordering if it exists.

    The notion of strong decomposability is used in connection with
    e.g. mixed interaction models where some vertices represent discrete
    variables and some represent continuous variables. Such graphs are
    said to be marked. The \code{mcsmarked()} function will return a
    perfect ordering iff the graph is strongly decomposable. As graphs
    do not know about whether vertices represent discrete or
    continuous variables, this information is supplied in the
    \code{discrete} argument.
}
\value{
  A vector with a linear ordering
  (obtained by maximum cardinality search) of the variables or
  character(0) if such an ordering can not be created.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\note{
  The workhorse is the \code{mcsMAT} function.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{moralize}}
  \code{\link{jTree}}
  \code{\link{rip}}
  \code{\link{ug}},
  \code{\link{dag}}
}
\examples{
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
mcs(uG)
mcsMAT(as.adjMAT(uG))
## Same as
uG <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st,result="matrix")
mcsMAT(uG)

## Marked graphs
uG1 <- ug(~a:b+b:c+c:d)
uG2 <- ug(~a:b+a:d+c:d)
## Not strongly decomposable:
mcsmarked(uG1, discrete=c("a","d"))
## Strongly decomposable:
mcsmarked(uG2, discrete=c("a","d"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

