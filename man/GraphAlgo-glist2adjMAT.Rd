\name{glist2adjMAT}
\alias{glist2adjMAT}
\alias{vpaL2adjMAT}
\alias{vpaL2tfM}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Creates adjacency matrix for a graph from a list of generators
}
\description{
  Creates adjacency matrix for a graph from a list of generators. The
  graph is assumed to be undirected.
}
\usage{
glist2adjMAT(glist, vn = uniquePrim(c(glist, recursive = TRUE)), result="matrix")
vpaL2adjMAT(vpaL, vn=uniquePrim(c(vpaL, recursive=TRUE)), result="matrix")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glist}{
    A list of generators for an undirected graph.
  }
  \item{vpaL}{
    A list of the form (v,pa(v)) specifying a directed acyclic graph. 
  }  
  \item{vn}{
    The names of the vertices in the graphs. These will be the row and
    column names of the matrix
  }
  \item{result}{Either \code{"matrix"} or \code{"Matrix"} (for a sparse
    matrix representation)}
}
%\details{  
%}
\value{
  An adjacency matrix (or \code{NULL} if \code{glist} has length 0) 
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
  Søren Højsgaard, \email{sorenh@math.aau.dk}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{ug}}
  \code{\link{dag}}
}
\examples{
glist <- list(1:3,2:4,4:5)
am1 <- glist2adjMAT(glist)
am2 <- vpaL2adjMAT(glist)
## plot(coerceGraph(am1, "igraph"))
## plot(coerceGraph(am2, "igraph"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

