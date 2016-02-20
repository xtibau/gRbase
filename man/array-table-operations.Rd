\name{table-operations}
\alias{tableSlice}
\alias{tableSlicePrim}
\alias{tableMargin}
\alias{tableOp}
\alias{tableMult}
\alias{tableDiv}
\alias{tableOp2}
\alias{tablePerm}
\alias{tableSetSliceValue}
\alias{tableGetSliceIndex}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute table margin or table slice}
\description{
  For a contingency table in array form, compute the sum of table
  entries for a given index (i.e. a marginal table) or find the slice of
  the table defined by specific margins being at a specific level.
}
\usage{
tableOp(t1, t2, op = "*")
tableMult(t1, t2)
tableDiv(t1, t2)
tableMargin(x, margin, keep.class=FALSE)
tableSlice(x, margin, level, impose)
tablePerm(x, perm, resize = TRUE, keep.class=FALSE)
tableSetSliceValue(x, margin, level, complement=FALSE, value=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, t1, t2}{An array (a vector with a dim and a dimnames attribute)}
  \item{margin}{An index, either numerical  or character}
  \item{keep.class}{If TRUE the result will be forced to have the same
    class as the input; otherwise the result will be an array.}
  \item{level}{A value, either numerical or character}
  \item{impose}{Possible value used to fill up a slice to give it full
    dimension}
  \item{complement}{If TRUE then the entries of the complement of the
    slice is set to some value.}
  \item{value}{The value to put into the entries given by \code{margin} and
    \code{level} - or their complement if \code{complement=TRUE}}
  \item{op}{Either "*" or "/"}

  \item{perm}{The subscript permutation vector, which must be a
    permutation of the integers 1:n, where n is the number of dimensions of
    a OR a permutation of the dimension names of a. The default is to
    reverse the order of the dimensions. A permutation of the dimensions of
    a.}
  \item{resize}{A flag indicating whether the vector should be resized as
    well as having its elements reordered. }

}

\details{
  \code{tableMargin}: tableMargin is analogous to margin.table except that margin can be given
  both as array indices or as variable names

  \code{tableSlice}: If the table x has dimensions Z,U,V where V has levels 1 and 2
  then tableSlice can extract the
  slice of x (in this case a 2-way table) defined by e.g. U=2. Setting
  impose=1000 implies that a 3-way table is returned with the U=2 slice
  in the right place and the U=1-slice consisting of 1000 in each cell.

  \code{tableOp}: If t1 has dimnames A and B and t2 has dimnames B and C then
  \code{tableOp(t1,t2)} will return a table (an array) with dimnames A, B and C
  containing the product.

  \code{tableMult(t1,t2)} is a wrapper for \code{tableOp(t1,t2, op="*")}
  and \code{tableMult(t1,t2)} is a wrapper for \code{tableDiv(t1,t2, op="/")}

  \code{tablePerm}: A wrapper for aperm, but tablePerm accepts dimnames in
  addition to indices.

  See examples below.
}
\value{
  An array.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard}
% \note{ ~~further notes~~

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{\link{margin.table}}
\examples{

data(HairEyeColor)

tableMargin(HairEyeColor, "Hair")           ## same as:
tableMargin(HairEyeColor, 1)
tableMargin(HairEyeColor, c("Hair", "Eye")) ## same as:
tableMargin(HairEyeColor, c(1, 2))


tableSlice(HairEyeColor, "Sex", "Male")              ## same as:
tableSlice(HairEyeColor, 3, 1)
tableSlice(HairEyeColor, "Sex", "Male", impose=1000) ## same as:
tableSlice(HairEyeColor, 3, 1, impose=1000)

t1 <- array(1:4, dim=c(2,2), dimnames=list(gender=c('male','female'),
                                           answer=c('yes','no')))
t2 <- array(5:8, dim=c(2,2), dimnames=list(answer=c('yes','no'),
                                           category=c(1,2)))

tableOp(t1, t2, "*")
tableOp(t1, t2, "/")

data(reinis)

t1 <- tableMargin(reinis, c(6, 5, 2, 1))
t2 <- tableMargin(reinis, c(6, 5, 3, 4))
tt1 <- tableOp(t1, t2)

t1 <- tableMargin(reinis, c(6, 5, 2, 4, 1))
t2 <- tableMargin(reinis, c(6, 5, 4))
tt1 <- tableOp2(t1, t2)

tableSetSliceValue(HairEyeColor, "Eye", "Brown")
tableSetSliceValue(HairEyeColor, "Eye", "Brown", complement=TRUE)

tableSetSliceValue(HairEyeColor, c("Eye","Hair"), c("Brown","Black"))
tableSetSliceValue(HairEyeColor, c("Eye","Hair"), c("Brown","Black"), complement=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

