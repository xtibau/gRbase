\name{isGraphical}
\alias{isGraphical}
\alias{isGraphical.default}
%\alias{isGraphical.list}
\alias{isDecomposable}
\alias{isDecomposable.default}
%\alias{isDecomposable.list}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Properties of a generating class (for defining a graph)
}

\description{A set of generators define an undirected graph, here
  called a dependence graph. Given a set of generators it is checked 1)
  if the dependence dependence graph is in 1-1-correspondance with the
  genrators (such that the corresponding model is graphical) and 2) if
  the dependence graph is chordal (triangulated) (such that the
  corresponding model is decomposable).
}

\usage{
isGraphical(x)
isDecomposable(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A generating class given as right hand sided formula or a list; see 'examples' below
  }
}

\details{
  A set of sets of variables, say A_1, A_2, ... A_K is
  called a generating class for a graph with vertices V and edges E. If
  two variables a,b are in the same generator, say A_j, then a and b
  are vertices in the graph and there is an undirected edge between a
  and b. 

  The graph induced by \code{g1 <- ~a:b + a:c + b:c + c:d} has edges
  \code{ab,ac,bc,cd}. The cliques of this graph are \code{abc,cd}. Hence
  there is not a 1-1-correspondance between the graph and the
  generators.

  On the other hand, \code{g2 <- ~a:b:c + c:d} induces the same
  graph in this case there is a 1-1-correspondance.

  The graph induced by
  \code{g3 <- ~a:b + b:c + c:d + d:a}
  is in 1-1-correspondance with its dependence graph, but the graph is
  not chordal.
}

\value{
  TRUE or FALSE
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{mcs}},
  \code{\link{rip}}
}
\examples{

g1 <- ~a:b+a:c+b:c+c:d
g2 <- ~a:b:c+c:d
g3 <- ~a:b + b:c + c:d + d:a

isGraphical( g1 ) # FALSE
isGraphical( g2 ) # TRUE
isGraphical( g3 ) # TRUE

isDecomposable( g1 ) # FALSE
isDecomposable( g2 ) # TRUE
isDecomposable( g3 ) # TRUE

## A generating class can be given as a list:
f <- list(c("a","b"), c("b","c"), c("a","c"))
isGraphical( f )
isDecomposable( f )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{models}
