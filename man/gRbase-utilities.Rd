\name{gRbase-utilities}

\alias{colSumsPrim}
\alias{intersectPrim}
\alias{matchPrim}
\alias{outerPrim}
\alias{setdiffPrim}
\alias{uniquePrim}
\alias{unlistPrim}
\alias{rowSumsPrim}
\alias{colwiseProd}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Utility functions for gRbase}
\description{
  Utility functions for gRbase package. Includes 'faster versions' of
  certain standard R functions. 
}
\usage{
uniquePrim(x)
setdiffPrim(x,y)
intersectPrim(x,y)
unlistPrim(l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{Vectors}
  \item{l}{A list (of vectors)}
%  \item{recursive}{logical. Should unlisting be applied to list components of x?}
%  \item{use.names}{logical. Should names be preserved?}
}
\details{
  \code{colwiseProd} multiplies a vector and a matrix columnwise (as
  opposed to rowwise which is achieved by \code{v*M}). Hence
  \code{colwiseProd} does the same as \code{t(v*t(M))} - but it does so
  faster for numeric values.  
}
\value{
  A vector or a logical.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
\note{
  Use the xxxxPrim functions with caution!

 %~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{\code{\link{unique}}, \code{\link{setdiff}}, \code{\link{unlist}}
}
\examples{
uniquePrim(c(1,2,3,2,1,2))
setdiffPrim(c(1,3,2), c(2,3,4,5))
unlistPrim(list(c(1,2),c(2,3)))

## colwiseProd
M <- matrix(1:16, nrow=4)
v <- 1:4

t(v*t(M))
colwiseProd(v,M)

system.time(for (ii in 1:100000)  t(v*t(M)))
system.time(for (ii in 1:100000)  colwiseProd(v,M))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
