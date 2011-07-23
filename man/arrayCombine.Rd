\name{arrayCombine}
\alias{arrayCombine}
\alias{arrayExtendDomain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Combine arrays
}
\description{
  Combine arrays into a new array with higher dimension.
}
\usage{
arrayCombine(aa.list, aux)
arrayExtendDomain(aa, bb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aa.list}{
    List of arrays.
  }
  \item{aux}{
    A list with one element. The element must be a vector and the
    element must be named, e.g. \code{list(Z=c(1,2))}.
  }
  \item{aa}{An array}
  \item{bb}{A list with additional dimensions to be added, e.g.
    \code{list(Z=c(1,2), U=c("a","b"))}
  }
  }
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  An array
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Søren Højsgaard <sorenh at agrsci dot dk>
}
\note{
  For two arrays with a common variable, it is not checked that the
  levels of that variable match. They must match, but it is the users
  responsibility to check that they do.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%  \code{\link{help}}
%}
\examples{
## Case 1: t1 and t2 are arrays defined over identical sets of variables:
t1 <- parray(c("y","x1"), c(2,2), 1:4)
t2 <- parray(c("y","x1"), c(2,2), c(-11,12,-13,14))
tc <- arrayCombine(list(t1,t2), aux=list(Z=c(1,2)))
as.data.frame.table(tc)
## The "auxilary" variable Z adds a new dimension to the table

## Case 2: t1 and t2 are arrays defined over non-identical sets of variables:
t1 <- parray(c("y","x1"), c(2,2), 1:4)
t2 <- parray(c("y","x2"), c(2,2), c(-11,12,-13,14))
tc <- arrayCombine(list(t1,t2), aux=list(Z=c(1,2)))
as.data.frame.table(tc)
## The "auxilary" variable Z adds a new dimension to the table
## When Z=Z1, tc is constant as a function of x2
## When Z=Z2, tc is constant as a function of x1

## Case 3: t1 and t2 are arrays defined over non-identical sets of variables,
## but the variables for t1 is a subset of the variables for t2:
t1 <- parray(c("y","x1"), c(2,2), 1:4)
t2 <- parray(c("y","x1","x2"), c(2,2,2), 11:18)
tc <- arrayCombine(list(t1,t2), aux=list(Z=c(1,2)))
as.data.frame.table(tc)
## The "auxilary" variable Z adds a new dimension to the table
## When Z=Z1, tc is constant as a function of x2

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

