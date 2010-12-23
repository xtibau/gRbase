\name{graph-operations1}

\alias{as.adjMAT}

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
  
\title{Simple operations on undirected and directed acyclic graphs.}
\description{
  Make operations on undirected and directed acyclic graphs.
}


\usage{
as.adjMAT(object)
ancestors(set, object)
ancestralGraph(set, object)
ancestralSet(set, object)
children(set, object)
closure(set, object)
vpar(object)
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
  %   \item{matrix}{If TRUE the result is returned as a p x 2 matrix; otherwise as a
  %     matrix list. }
}
 \details{
   Notice that as.adjMAT(g) does the same as as(g, "matrix") but
   as.adjMAT() is considerably faster.
 }
% \value{
%   ~Describe the value returned
%   If it is a LIST, use
%   \item{comp1 }{Description of 'comp1'}
%   \item{comp2 }{Description of 'comp2'}
%   ...
% }
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
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
ugr <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)
closure("me", ugr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
