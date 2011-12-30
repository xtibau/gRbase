\name{glist2adjMAT}
\alias{glist2adjMAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Creates adjacency matrix for a graph from a list of generators
}
\description{
  Creates adjacency matrix for a graph from a list of generators. The
  graph is assumed to be undirected.
}
\usage{
glist2adjMAT(glist, vn = uniquePrim(c(glist, recursive = TRUE)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glist}{
    A list of generators.
}
  \item{vn}{
    The names of the vertices in the graphs. These will be the row and
    column names of the matrix
}
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
  Søren Højsgaard, sorenh at mail dot dk
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
am <- glist2adjMAT(glist)
## plot(coerceGraph(am, "igraph"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

