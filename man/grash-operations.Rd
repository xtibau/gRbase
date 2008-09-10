\name{graph-operations}

\alias{MCS}
\alias{RIP}
\alias{MCSMAT}
\alias{RIPMAT}
\alias{as.adjMAT}
\alias{moralize}
\alias{print.ripOrder}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{Operations on undirected and directed acyclic graphs.}
\description{
  Make operations on undirected and directed acyclic graphs.
}

\usage{
%mcs(ug,amat=as.adjmat(ug), vn=colnames(amat), root=NULL, index=FALSE)
MCS(object, root=NULL, index=FALSE)
RIP(object,root=NULL,nLevels=NULL)
MCSMAT(amat, vn = colnames(amat), root = NULL, index = FALSE) 
RIPMAT(amat, root = NULL, nLevels = NULL) 
as.adjMAT(object)
moralize(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An undirected graph (of class "graphNEL")}
  \item{root}{Variables to be considered first when doing the orderings}
  \item{index}{If TRUE, then a permutation is returned}
  \item{nLevels}{Number of levels of the nodes in the graph}
  \item{amat}{Adjacency matrix (symmetrical) for undirected graph}
  \item{vn}{Nodes in the graph given by adjacency matrix}
  
%   \item{ug}{An undirected graph (of class "ugsh")}
%   \item{method}{The triangulation method, \code{"standard"} is a C
%     implementation of a minimum clique weight heuristic,
%     \code{"mcwh"} is a Fortran implementation of the same (kindly
%     provided by Peter Green) and \code{"R"} is a corresponding R
%     implementation. }
%   \item{matrix}{Should the triangulated graph be returned as an
%     adjacency matrix}
%   \item{amat}{The graph represented as an adjancecy matrix}
%   \item{vn}{The node names as they appear in the adjancecy matrix}
%   \item{control}{Currently not used}
%   \item{\dots}{Currently not used.}
}
%\details{
  %The jTree (for "junction tree") is just a wrapper for a call of
  %\code{triangulate} followed by a call of \code{ripOrder}.
%}
%\value{
%   \code{moralize} and   \code{triangulate} returns objects of class
%   "graphsh".

%   \code{mcs} returns a vector or NULL.

%   \code{ripOrder} returns a list with cliques, separators etc.
%}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{newUG}}, 
  \code{\link{newDAG}}
}
\examples{

## Undirected graphs
##
ug <- newUG(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
edges(ug)
nodes(ug)
MCS(ug)
MCSMAT(as.adjMAT(ug))
RIP(ug)
RIPMAT(as.adjMAT(ug))

## Directed graphs
##
#dag <- newdag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
#edges(dag)
#nodes(dag)
#moralize(dag)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}


%triangulate(ug,method="standard", nLevels, matrix=FALSE)
%jTree(ug, method="standard", vn=nodes(ug),nLevels, control=list())


% \alias{triangulate}
% \alias{ripOrder}
% \alias{jTree}

%\alias{moralize}
