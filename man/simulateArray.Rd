\name{simulateArray}
\alias{simulateArray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate data from array}
\description{
  Simulate data (slice of) an array.
}
\usage{
simulateArray(x, nsim = 1, margin, value.margin) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An array}
  \item{nsim}{Number of cases to simulate}
  \item{margin, value.margin}{Specification of slice of array to
    simulate from}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A matrix
}
% \references{
%   Søren Højsgaard (2012). Graphical Independence Networks with the
%   gRain Package for R. Journal of Statistical Software, 46(10), 1-26.
%   \url{http://www.jstatsoft.org/v46/i10/}.
% }

\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
\note{
  The current implementation is fragile in the sense that it is not
  checked that the input argument \code{x} is an array.
}  
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}  
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

## 2x2 array
x <- parray(c("a","b"), levels=c(2,2), values=1:4)

## Simulate from entire array
s <-simulateArray(x,1000)
xtabs(~., as.data.frame(s))

## Simulate from slice defined by that dimension 1 is fixed at level 2
s <-simulateArray(x,6000,1,2)
xtabs(~., as.data.frame(s))

## 2x2x2 array
x <- parray(c("a","b","c"), levels=c(2,2,2), values=1:8)

## Simulate from entire array
s <-simulateArray(x,36000)
xtabs(~., as.data.frame(s))

## Simulate from slice defined by that dimension 3 is fixed at level 1
s <-simulateArray(x,10000,3,1)
xtabs(~., as.data.frame(s))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

