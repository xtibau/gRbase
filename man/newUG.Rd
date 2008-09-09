\name{newUG}

\alias{newUG}
\alias{newDAG}
\alias{newUGlist}
\alias{newDAGlist}
\alias{edgePairs}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create undirected and directed graphs}
\description{
  These functions are wrappers for creation of graphs as implemented by
  graphNEL objects in the \code{graph} package.
}
\usage{
newUG(..., short = FALSE)
newDAG(..., short = FALSE)
edgePairs(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{A generating class for a graph, see examples below}
  \item{short}{Not used}
  \item{object}{A graph of class 'graphNEL'}
}
%\details{
%  
%}
\value{
  A 'graphNEL' object.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, sorenh at agrsci.dk}
% \note{ ~~further notes~~ 

%  ~Make other sections like Warning with \section{Warning }{....} ~
% }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
ug <- newUG(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)

ug <- newUG(~me:ve:al,~al:an:st)

ug <- newUG(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))

ug <- newUG(~me:ve:al, c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))

dag <- newDAG(c("me","ve"),c("me","al"),c("ve","al"),c("al","an"),c("al","st"),c("an","st"))
dag <- newDAG(~me:ve,~me:al,~ve:al,~al:an,~al:st,~an:st)

dag2 <- newDAG(~me:ve:al,~ve:al:an)

edges(ug)
nodes(ug)

edges(dag)
nodes(dag)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
