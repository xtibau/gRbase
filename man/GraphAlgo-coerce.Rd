\name{graph-coercion}

\alias{M2adjList}
\alias{M2dagList}
\alias{M2graphNEL}
\alias{M2ugList}

\alias{adjList2M}
\alias{dagList2M}
\alias{ugList2M}

\alias{graphNEL2M}
\alias{graphNEL2matrix}
\alias{graphNEL2dgCMatrix}
\alias{graphNEL2MAT}
\alias{graphNEL2adjMAT}

\alias{dagList2graphNEL}
\alias{graphNEL2igraph}
\alias{matrix2igraph}
\alias{ugList2graphNEL}

\alias{graphNEL2ftM}
\alias{graphNEL2tfM}
\alias{as.adjMAT}

%%\alias{NEL2MAT}

\title{Coercion of graphs}
\description{
  Coercion of graphs from \code{graphNEL} format to dense or sparse
  adjacency matrix formats etc.
}

\usage{
as.adjMAT(object, result="matrix")
graphNEL2M(object, result="matrix")
graphNEL2MAT(object, limit=100)
graphNEL2ftM(object)
graphNEL2matrix(object)
graphNEL2dgCMatrix(object)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A graphNEL object}
  \item{result}{Either \code{"matrix"} of \code{"Matrix"} (or
    equivalenty \code{"dgCMatrix"}) for a sparse
    matrix representation.}
  \item{limit}{If the number of nodes is larger than \code{limit} the
    matrix will be sparse; otherwise dense.}
}
\details{
  Notice: \code{graphNEL2M(g, result="matrix")} and
  \code{graphNEL2M(g, result="Matrix")} do the same as
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
  \code{\link{getCliques}}
  \code{\link{nonEdgeList}}
  \code{\link{mcs}}
  \code{\link{rip}}
  \code{\link{moralize}}
  \code{\link{jTree}}
}
\examples{
uG <- ug(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)
getCliques(uG)

uG.m <- as.adjMAT(uG)
getCliques( uG.m )

ug.M <- as.adjMAT(uG, result="Matrix")
getCliques( ug.M )

graphNEL2ftM( uG )
graphNEL2tfM( uG )

graphNEL2M( uG )
graphNEL2matrix( uG )
graphNEL2dgCMatrix( uG )
graphNEL2MAT( uG )
graphNEL2MAT( uG, limit=4 )

}
\keyword{utilities}
