\name{gRbase-utilities}
\alias{uniquePrim}
\alias{setdiffPrim}
\alias{unlistPrim}
\alias{subsetof}
\alias{subsetofList}
\alias{removeRedundant}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Utility functions for gRbase}
\description{
  Utility functions for gRbase package. Includes 'faster versions' of
  certain standard R functions. 
}
\usage{
uniquePrim(x)
setdiffPrim(x,y)
subsetof(x, y)
subsetofList(x, l)
unlistPrim(l, recursive = TRUE, use.names = TRUE)
removeRedundant(l, maximal=TRUE, index=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{Vectors}
  \item{l}{A list (of vectors)}
  \item{recursive}{logical. Should unlisting be applied to list components of x?}
  \item{use.names}{logical. Should names be preserved?}
  \item{maximal}{If TRUE the maximal terms are returned, if FALSE
    the minimal terms are returned}
  \item{index}{If TRUE the the index of the maximal/minimal elements of
    l are returned rather than the subset of maximal or minimal subset
    of l}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
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
subsetof(c(1,2),c(1,2,3))
subsetof(c(1,2,3), c(1,2))

l <- list(c(1,2),c(1,2,3),c(2,4),c(5,6), 5)

subsetofList(c(1,2), l)
subsetofList(c(1,2,3,4), l)

removeRedundant(l)
removeRedundant(l, maximal=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
