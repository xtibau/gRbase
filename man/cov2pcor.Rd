\name{cov2pcor}
\alias{cov2pcor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Partial correlation (matrix)}
\description{
 \code{cov2pcor} calculates the partial correlation matrix from an
 (empirical) covariance matrix
}
\usage{
cov2pcor(V)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{Covariance matrix}
}
% \details{
%   ~~ If necessary, more details than the description above ~~
% }
\value{
  A matrix with the same dimension as V.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(math)
S <- cov.wt(math)$cov
cov2pcor(S)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

