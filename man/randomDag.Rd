\name{random_dag}
\alias{random_dag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Random directed acyclic graph%%  ~~function to do ... ~~
}
\description{
  Generate a random directed acyclic graph (DAG)
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
random_dag(V, maxpar = 3, wgt = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
    The set of vertices.
  }
  \item{maxpar}{
    The maximum number of parents each node can have
}
  \item{wgt}{
    A parameter controlling how likely it is for a node to have a
    certain number of parents; see 'Details'%%     ~~Describe \code{wgt} here~~
}
}
\details{
If the maximum number of parents for a node is, say 3 and wgt=0.1, then
the probability of the node ending up with 0,1,2,3 parents is
proportional to 0.1^0, 0.1^1, 0.1^2, 0.1^3.
}
\value{
  A graphNEL object.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
dg   <- random_dag(1:1000, maxpar=5, wgt=.9)
table(sapply(vpar(dg),length))

dg   <- random_dag(1:1000, maxpar=5, wgt=.5)
table(sapply(vpar(dg),length))

dg   <- random_dag(1:1000, maxpar=5, wgt=.1)
table(sapply(vpar(dg),length))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

