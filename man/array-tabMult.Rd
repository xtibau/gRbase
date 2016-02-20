\name{tableOpertations}

\alias{tabAdd}
\alias{tabSubt}
\alias{tabMult}
\alias{tabDiv}
\alias{tabDiv0}
\alias{tabEqual}
\alias{tabMarg}
\alias{tabCondProb}
\alias{tabListAdd}
\alias{tabListMult}
\alias{tabAlign}
\alias{tabExpand}
\alias{tabSliceMult}
\alias{tabSlice2Entries}
\alias{tabSlice2Entries_}
\alias{tabPerm} %% could be a good name!!!

\alias{tabAdd__}
\alias{tabSubt__}
\alias{tabMult__}
\alias{tabDiv__}
\alias{tabDiv0__}
\alias{tabOp__}

\alias{aperm__}
%% \alias{apermc__}
%% \alias{apermi__}

\alias{tabMarg__}
%% \alias{tabMargc__}
%% \alias{tabMargi__}

\alias{tabEqual__}
\alias{tabMarg__}

\alias{tabAlign__}
\alias{tabExpand__}


\alias{tabListAdd__}
\alias{tabListMult__}

\alias{tabMarg}
\alias{tabSlice}
%% \alias{tabSlice2}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Operations on multidimensional tables (arrays).
}
\description{
  A multidimensional table (an array) is here a vector with a dim and a
  dimnames attribute.
}
\usage{
tabPerm(tab, perm)
tabMarg(tab, marg)

tabAdd(tab1, tab2)
tabSubt(tab1, tab2)
tabMult(tab1, tab2)
tabDiv(tab1, tab2)
tabDiv0(tab1, tab2)

tabEqual(tab1, tab2, eps=1e-12)
tabMarg(tab, marg)
tabAlign(tab1, tab2)
tabExpand(tab1, tab2)
tabListAdd(lst)
tabListMult(lst)
aperm__(tab, perm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab, tab1, tab2}{
	Multidimensional arrays.
  }
  \item{lst}{List of multidimensional arrays (no checking is done)}
  \item{perm}{A vector of indices or dimnames giving the desired
	permutiation. For \code{tabPerm}, it can also be a right hand sided formula.}
  \item{marg}{A vector of indices or dimnames defining the
	desired marginal. For \code{tabMarg} it can also be a right hand
	sided formula. }
  \item{eps}{
	Criterion for checking equality of two arrays.
  }
}
\details{
  \code{perm} in \code{aperm__()} can be a vector of indices (as in Rs
  own \code{aperm()}) but also a vector of dimnames. Currently there is
  no checking that the dimnames are actually in the array, so please
  take care.

  \code{tabDiv0} divides two tables with the convention that 0/0=0.
}
\value{
  Most functions here return a multidimensional array.
}

%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
  Soren Hojsgaard, \email{sorenh@math.aau.dk}
}
%% \note{
%% %%  ~~further notes~~
%% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{aperm}}
}
\examples{
tab1 <- array(1:8, dim=c(2,2,2), dimnames=list("a"=1:2,"b"=1:2,"c"=1:2))
tab2 <- array(1:8, dim=c(2,2,2), dimnames=list("b"=1:2,"c"=1:2,"d"=1:2))

## ## tabMarg ##
## Marginalize down to the bc-array
tabMarg(tab1, 2:3)
tabMarg(tab1, c("b","c"))
tabMarg(tab1, ~b+c)

## This gives an error
## tabMarg(tab1, c(2,5))
## tabMarg(tab1, c("b","w"))
## tabMarg(tab1, ~b+w)


## ## tabPerm ##
tabPerm(tab1, 1:3)      ## No change - an abc-table
tabPerm(tab1, c(2,3,1)) ## A bca-table
tabPerm(tab1, ~b+c+a)

## This gives error
## tabPerm(tab1, c(2,1))
## tabPerm(tab1, c(2,1,5))
## tabPerm(tab1, c(2,1,NA))

## ## tabMult etc ##
## Multiply two arrays
out <- tabMult(tab1, tab2)
out <- tabPerm(out, ~a+b+c+d) ## Just for comparison below
ftable(out)
## Alternative approch
df1 <- as.data.frame.table(tab1)
df2 <- as.data.frame.table(tab2)
df3 <- merge(df1, df2, by=c("b","c"))
df3 <- transform(df3, Freq=Freq.x*Freq.y)
tab3 <- xtabs(Freq ~ a+b+c+d, data=df3)
ftable(tab3)

## ## tabExpand ##
tab1.e <- tabExpand(tab1, tab2)
## tab1.e has dimnames b,c,d,a; values are simply replicated for each
## level of d.
dimnames(tab1.e)
ftable(tab1.e, row.vars="d")

## ## tabAlign ##
tab2.e <- tabExpand(tab2, tab1)
names(dimnames(tab2.e))
names(dimnames(tab1.e))
out <- tabAlign(tab1.e, tab2.e)
names(dimnames(out)) ## Same as tab2.e

## ## tabListAdd, tabListMult ##
lst <- list(tab1, tab2)
tabListAdd( lst )
tabListMult( lst )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
