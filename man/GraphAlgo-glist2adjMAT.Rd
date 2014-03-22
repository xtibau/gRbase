\name{glist2adjMAT}
\alias{glist2adjMAT}
\alias{vpaList2adjMAT}
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
glist2adjMAT(glist, vn = unique(unlist(glist)), result="matrix")
vpaList2adjMAT(glist, vn = unique(unlist(glist)), result="matrix")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glist}{
    A list of generators where a generator is a vector of nodes. For
    \code{glist2adjMAT} a vector (v1,...,vn) means that there are
    undirected edges between all nodes. For \code{vpaList2adjMAT},
    (v1,...,vn) means that there will be arrows from v2,...,vn to v1.
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
am2 <- vpaList2adjMAT(glist)
if (interactive()){
 plot(as(am1, "graphNEL"))
 plot(as(am2, "graphNEL"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

