\name{graph-operations}

\alias{ancestors}
\alias{ancestralGraph}
\alias{ancestralSet}
\alias{children}
\alias{closure}

\alias{is.complete}
\alias{is.decomposition}
\alias{is.simplicial}
\alias{parents}
\alias{simplicialNodes}

\title{Simple operations on undirected and directed acyclic graphs.}
\description{
  Make operations on undirected and directed acyclic graphs (which are
  represented as \code{graphNEL} objects.
}

\usage{
ancestors(set, object)
ancestralGraph(set, object)
ancestralSet(set, object)
children(set, object)
closure(set, object)
is.complete(object, set)
is.decomposition(set, set2, set3, object)
is.simplicial(set, object)
parents(set, object)
simplicialNodes(object)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set, set2, set3}{Vectors of sets}
  \item{object}{A graphNEL object}
%  \item{result}{Either \code{"matrix"} of \code{"Matrix"} (for a sparse
%    matrix representation).}
}
\details{
  Notice: \code{graphNEL2adjMAT(g, result="matrix")} and
  \code{graphNEL2adjMAT(g, result="Matrix")} do the same as
  \code{as(g, "matrix")} and \code{as(g, "Matrix")} but
  considerably faster.
}
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
% \note{ ~~further notes~~

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{
  \code{\link{edgeList}}
  \code{\link{nonEdgeList}}
  \code{\link{mcs}}
  \code{\link{rip}}
  \code{\link{moralize}}
  \code{\link{jTree}}
}
\examples{
uG <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)
closure("me", uG)
getCliques( uG )

amat1 <- graphNEL2M( uG )
getCliques( amat1 )

amat1 <- graphNEL2M( uG, result="Matrix" )
getCliques( amat1 )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
