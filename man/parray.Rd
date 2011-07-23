\name{parray}

\alias{parray}
\alias{as.parray}

\alias{varNames.parray}
\alias{nLevels.parray}
\alias{valueLabels.parray}

\alias{varNames.array}
\alias{nLevels.array}
\alias{valueLabels.array}

\alias{print.parray}
\alias{tableMarginPrim}



%- Also NEED an '\alias' for EACH other topic documented here.
\title{Representation of and operations on multidimensional tables}
\description{General representation of multidimensional 
  tables (by \code{parray} objects).
}

\usage{
parray(varNames, levels, values = 1, normalize = c("none", "first", "all"), smooth = 0)
as.parray(values, normalize=c("none","first","all"), smooth=0)
\method{varNames}{parray}(x)
\method{nLevels}{parray}(x)
\method{valueLabels}{parray}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varNames}{Names of variables defining table}
  \item{levels}{Either vector with number of levels of the factors in
    varNames or list vith specification of the levels of the factors in
    varNames. See 'examples' below.}
  \item{values}{Table values}
  \item{x}{Objects of class "parray"}
  \item{normalize}{Should result be normalized, see 'Details' below.}
  \item{smooth}{Should values be smoothed, see 'Details' below.}
}
\details{
  A parray object represents a table defined by a set of variables and
  their levels, together with the values of the table. E.g. f(a,b,c) can
  be a table with a,b,c representing levels of binary variable
  
  If \code{normalize="first"} then for each configuration of the parents,
  "pa", the probabilities are normalized to sum to one. Thus f(a,b,c)
  becomes a conditional probability table of the form p(a|b,c). If
  \code{normalize="all"} then the sum over all entries of f(a,b,c) is
  one. 
  
  If \code{smooth} is positive then \code{smooth} is added to 
  \code{values}  before normalization takes place.

  \code{as.parray} can be used for coercing a an array to a parray
  object.

}
\value{
  An object of class \code{parray}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}

% \seealso{
%   \code{\link{tableOp}}
%   \code{\link{tableOp}}
%   \code{\link{tableOp}}
%   \code{\link{tableOp}}
% }

\examples{
t1 <- parray(c("gender","answer"),list(c('male','female'),c('yes','no')), values=1:4)
t1 <- parray(~gender:answer,list(c('male','female'),c('yes','no')), values=1:4)
t1 <- parray(~gender:answer,c(2,2), values=1:4)

t2 <- parray(c("answer","category"), list(c('yes','no'),c(1,2)), values=1:4+10)
t3 <- parray(c("category","foo"), c(2,2), values=1:4+100)

varNames(t1)
nLevels(t1)
valueLabels(t1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
