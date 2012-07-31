\name{graph-operations1}

\alias{ancestors}
\alias{ancestralGraph}
\alias{ancestralSet}
\alias{children}
\alias{closure}

\alias{vpar}
\alias{is.complete}
\alias{is.decomposition}
\alias{is.simplicial}
\alias{parents}
\alias{simplicialNodes}

\alias{as.adjMAT}
\alias{graphNEL2ftM}
\alias{maxCliqueMAT}

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
vpar(object, forceCheck=TRUE)
is.complete(object, set)
is.decomposition(set, set2, set3, object)
is.simplicial(set, object)
parents(set, object)
simplicialNodes(object)
as.adjMAT(object, result="matrix")
graphNEL2ftM(object)
maxCliqueMAT(amat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{set, set2, set3}{Vectors of sets}
  \item{object}{A graphNEL object}
  %   \item{matrix}{If TRUE the result is returned as a p x 2 matrix; otherwise as a
  %     matrix list. }
  \item{forceCheck}{Logical indicating if it should be checked that the
    object is a DAG}
  \item{result}{Either \code{"matrix"} of \code{"Matrix"} (for a sparse
    matrix representation).}
  \item{amat}{Adjacency matrix}
}
 \details{
   Notice: \code{as.adjMAT(g)} does the same as \code{as(g, "matrix")} but
   considerably faster.

   \code{maxCliqueMAT} applied to an adjacency matrix does the same as
   \code{maxClique} applied to a graphNEL; but maxCliqueMAT is is
   considerably faster. If the matrix is sparse, it will be coerced to a
   non-sparse matrix.
   
  
 }
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at mail dot dk}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
\seealso{
  \code{\link{edgeList}}
  \code{\link{nonEdgeList}}
  %\code{\link{as.adjMAT}}
  \code{\link{mcs}}
  \code{\link{rip}}
  \code{\link{moralize}}
  \code{\link{jTree}}
}
\examples{
uG <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)
closure("me", uG)

maxClique(uG)
amat1 <- as.adjMAT(uG)
maxCliqueMAT(amat1)

amat1 <- as.adjMAT(uG, result="Matrix")
maxCliqueMAT(amat1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
