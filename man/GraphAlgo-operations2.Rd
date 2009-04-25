\name{graph-operations2}

\alias{mcs}
\alias{mcsMAT}

\alias{rip}
\alias{ripMAT}
\alias{moralize}
\alias{moralizeMAT}
\alias{maxCliqueMAT}
\alias{jTree}
\alias{print.ripOrder}

%- Also NEED an '\alias' for EACH other topic documented here.

\title{More advanced operations on undirected and directed acyclic graphs.}
\description{
  Make operations on undirected and directed acyclic graphs.
}

\usage{
%mcs(ug,amat=as.adjmat(ug), vn=colnames(amat), root=NULL, index=FALSE)
mcs(object, root=NULL, index=FALSE)
mcsMAT(amat, vn = colnames(amat), root = NULL, index = FALSE) 
rip(object,root=NULL,nLevels=NULL)
ripMAT(amat, root = NULL, nLevels = NULL) 
moralize(object)
moralizeMAT(amat)
maxCliqueMAT(amat)
jTree(object, method="mcwh", nLevels=rep(2,length(nodes(object))),control=list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An undirected graph (of class "graphNEL")}
  \item{root}{Variables to be considered first when doing the orderings}
  \item{index}{If TRUE, then a permutation is returned}
  \item{nLevels}{Number of levels of the nodes in the graph}
  \item{amat}{Adjacency matrix}
  \item{vn}{Nodes in the graph given by adjacency matrix}
  \item{method}{The triangulation method, \code{"mcwh"} is a C
    implementation of a minimum clique weight heuristic,
    \code{"R"} is a corresponding R
    implementation (experimental) }
  \item{control}{Currently not used}
}
\details{
  The \code{RIP} and \code{RIPMAT} functions return a RIP ordering of
  the cliques. This is obtained by first ordering the variables linearly
  with maximum cardinality search (by \code{MCS}). The root argument is
  transfered to \code{MCS} as a way of controlling which clique will be
  the first in the RIP ordering. 
  
  The jTree (for "junction tree") is just a wrapper for a call of
  \code{triangulate} followed by a call of \code{RIP}.  
}
\value{
   \code{moralize} and   \code{triangulate} returns objects of class
   "graphsh".

   \code{MCS} and \code{MCSMAT} return a vector with a linear ordering
   (obtained by maximum cardinality search) of the variables or
   character(0) if such an ordering can not be created. 

   \code{RIP} and \code{RIPMAT} returns a list with cliques, separators
   etc. if a RIP ordering can be made and an empty list if not. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh@agrsci.dk}
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{
  \code{\link{ug}}, 
  \code{\link{dag}}
}
\examples{

## Undirected graphs
##
ugr <- ug(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
ugm <- as.adjMAT(ugr)
edges(ugr)
nodes(ugr)
mcs(ugr)
mcsMAT(ugm)
rip(ugr)
ripMAT(ugm)

maxClique(ugr)
maxCliqueMAT(ugm)

## Directed graphs
##
dagr <- dag(~me+ve,~me+al,~ve+al,~al+an,~al+st,~an+st)
edges(dagr)
nodes(dagr)
moralize(dagr)
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
