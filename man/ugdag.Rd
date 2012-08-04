\name{ug}

\alias{ug}
\alias{dag}
\alias{ugList}
\alias{dagList}

% \alias{ugMAT}
% \alias{ugListMAT}
% \alias{dagMAT}
% \alias{dagListMAT}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create undirected and directed graphs}
\description{
  These functions are wrappers for creation of graphs as implemented by
  graphNEL objects in the \code{graph} package.
}
\usage{
ug(..., result="NEL")
dag(..., result="NEL", forceCheck=FALSE)
ugList(x, result="NEL")
dagList(x, result="NEL", forceCheck=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{A generating class for a graph, see examples below}
  \item{x}{A list containing a generating class for a graph, see
    examples below}
  \item{forceCheck}{Logical determining if it should be checked if the
    graph is acyclical}
  \item{result}{The format of the graph. The possible choices are "NEL"
    (for a graphNEL object), "matrix" (for an adjacency matrix),
    "igraph" (for an igraph object), "Matrix" (for a sparse matrix).}
}
% \details{
%   Functions dag() and ug() were originally called newDAG() and
%   newUG(). The latter functions are retained in the package for backward
%   compatibility, but they will be removed at some point of time in the future. 
% }
\value{
  Functions ug(), dag(), ugList() and dagList() return a 'graphNEL'
  object, an adjacency matrix or an 'igraph' object.
  %Functions ugMAT() and ugListMAT() return an adjacency matrix. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
ugr <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)

ugr <- ug(~me:ve:al,~al:an:st)

ugr <- ug(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st")) 

ugr <- ug(~me:ve:al, c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))


dagr <- dag(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))


dagr <- dag(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)


dagr <- dag(~me:ve:al,~ve:al:an)

edges(ugr)
nodes(ugr)

edges(dagr)
nodes(dagr)

ugList(list(~me:ve:al,~al:an:st))
dagList(list(~me:ve:al,~ve:al:an))



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
