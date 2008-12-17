\name{ptable}

\alias{ptable}
\alias{as.ptable}

\alias{varNames.ptable}
\alias{nLevels.ptable}
\alias{valueLabels.ptable}

\alias{varNames.array}
\alias{nLevels.array}
\alias{valueLabels.array}

\alias{print.ptable}
\alias{tableMarginPrim}



%- Also NEED an '\alias' for EACH other topic documented here.
\title{Representation of and operations on multidimensional tables}
\description{General representation of multidimensional 
  tables (by \code{ptable} objects).
  %Operations on these include multiplication, division and
  %marginalization.
}

\usage{
ptable(varNames, nLevels, values = 1, normalize = c("none", "first", "all"), smooth = 0)
tableMarginPrim(t1, margin, normalize=FALSE)

\method{varNames}{ptable}(x)
\method{nLevels}{ptable}(x)
\method{valueLabels}{ptable}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varNames}{Names of variables defining table}
  \item{nLevels}{Levels of variables}
  \item{values}{Table values}
  \item{x}{Objects of class "ptable"}
  \item{t1}{Objects of class "ptable"}
  %\item{op}{Operation to perform. Either "*" or "/" (where 0/0=0)}
  \item{margin}{Set of nodes to marginalize onto}
  \item{normalize}{Should result be normalized, see 'Details' below.}
  \item{smooth}{Should values be smoothed, see 'Details' below.}
}
\details{
  A ptable object represents a table defined by a set of variables and
  their levels, together with the values of the table. E.g. f(a,b,c) can
  be a table with a,b,c representing levels of binary variable
  
  If \code{normalize="first"} then for each configuration of the parents,
  "pa", the probabilities are normalized to sum to one. Thus f(a,b,c)
  becomes a conditional probability table of the form p(a|b,c). If
  \code{normalize="all"} then the sum over all entries of f(a,b,c) is
  one. 
  
  If \code{smooth} is positive then \code{smooth} is added to 
  \code{values}  before normalization takes place.

}
\value{
  An object of class \code{ptable}.
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
t1 <- ptable(c("gender","answer"),list(c('male','female'),c('yes','no')), values=1:4)
t2 <- ptable(c("answer","category"), list(c('yes','no'),c(1,2)), values=1:4+10)
t3 <- ptable(c("category","foo"), c(2,2), values=1:4+100)

varNames(t1)
nLevels(t1)
valueLabels(t1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
