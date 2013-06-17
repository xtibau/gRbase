\name{graph-coercion}

\alias{graphNEL2ftM}
\alias{as.adjMAT}
\alias{graphNEL2adjMAT}
\alias{graphNEL2matrix}
\alias{graphNEL2dgCMatrix}

\title{Coercion of graphs}
\description{
  Coercion of graphs from graphNEL format to adjacency matrix format and
  to other formats
}

\usage{
as.adjMAT(object, result="matrix")
graphNEL2adjMAT(object, result="matrix")
graphNEL2ftM(object)
graphNEL2matrix(object)
graphNEL2dgCMatrix(object)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A graphNEL object}
  \item{result}{Either \code{"matrix"} of \code{"Matrix"} (for a sparse
    matrix representation).}
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
