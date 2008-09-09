\name{triang}
\alias{triang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Triangulation of an undirected graph}
\description{
  This function will triangulate an undirected graph by adding fillins. 
}
\usage{
triang(object, vn = nodes(object), nLevels = rep(2, length(vn)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An undirected graph (of class 'graphNEL')}
  \item{vn}{Names of nodes in the graph}
  \item{nLevels}{Typically, the number of levels of the variables
    (nodes) when these are discrete. Used in determining the
    triangulation using a "minimum clique weight heuristic". See section
  'details'.}
}
\details{
  The triangulation is made so as the total state space is kept low.
}
\value{
  A triangulated graph (an object of class 'graphNEL').
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{MCS}}
  \code{\link{RIP}}
}
\examples{
ug <- newUG(~a:b+b:c+c:d+d:e+e:f+f:a)
triang(ug)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
