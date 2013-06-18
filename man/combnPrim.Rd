\name{combnPrim}
\Rdversion{1.1}
\alias{combnPrim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate All Combinations of n Elements Taken m at a Time
}
\description{
  Generate all combinations of the elements of x taken m at a time.
  If x is a positive integer, returns all combinations of the elements
  of seq(x) taken m at a time.  
}
\usage{
combnPrim(x, m, simplify = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    vector source for combinations, or integer n for x <- seq(n).
}
  \item{m}{
    number of elements to choose.
}
  \item{simplify}{
    logical indicating if the result should be simplified to a matrix;
    if FALSE, the function returns a list. 
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
  A matrix or a list.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
  P.T.Wallace and Søren Højsgaard
}
\note{
  The combnPrim function is a simplified version of the combn
  function. However, combnPrim is implemented in C and is considerably
  faster than combn.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{combn}}
}
\examples{

x <- letters[1:20]
m <- 3

combn(x,m)
combnPrim(x,m)

combn(m,m)
combnPrim(m,m)

combn(x,m, simplify=FALSE)
combnPrim(x,m, simplify=FALSE)

system.time({ for (ii in 1:100) { combnPrim(x,m) }})
system.time({ for (ii in 1:100) { combn(x,m) }})

system.time({ for (ii in 1:100) { combnPrim(x,m, simplify=FALSE) }})
system.time({ for (ii in 1:100) { combn(x,m, simplify=FALSE) }})

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

