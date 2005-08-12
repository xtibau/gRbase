\name{gModel}
\alias{gModel}
\alias{gModel-class}

\alias{formula}
\alias{formula.gModel}
\alias{formula<-}
\alias{formula<-.gModel}

\alias{gmData}
\alias{gmData<-}
\alias{gmData.gModel}
\alias{gmData<-.gModel}

\alias{print.gModel}

\alias{addEdge}
\alias{addEdge.gModel}
\alias{addVertex}
\alias{addVertex.gModel}
\alias{dropEdge}
\alias{dropEdge.gModel}
\alias{dropVertex}
\alias{dropVertex.gModel}

\title{Class "gModel" - graphical models}
\description{
  The general class \code{gModel} contains a formula object and a
  \code{gmData} object. Implementations of different
  specific graphical model classes can inherit from this class and
  provide methods for parsing the formula. This is illustrated in the
  implementation of a class for hierarchical log--linear models,
  \code{\link{hllm}}.
}
\usage{
gModel(formula, gmData)
addEdge(object, name.1, name.2)
dropEdge(object, name.1, name.2)
addVertex(object, name)
dropVertex(object, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an object of class \code{\link{formula}}.}
  \item{gmData}{an object of class \code{\link{gmData}}.}
  \item{object}{an object of class \code{gModel}.}
  \item{name}{a string with a name of a variable available in
    \code{gmData} to be added or removed from the model formula.}
  \item{name.1}{a string with a name of a variable available in
    \code{gmData} to mark the first node in the edge to be added or
    dropped from the model.}
  \item{name.2}{a string with a name of a variable available in
    \code{gmData} to mark the second node in the edge to be added or
    dropped from the model..}
}
%\details{
%}
\value{
  \code{gModel} creates an object of class \code{gModel} with the two
  components \code{formula} and \code{gmData}. These components can be
  retrieved or replaced using the accessor functions of the same
  names. Also, a \code{gModel} object may be manipulated using the
  \code{dynamicGraph} interface.
}
%\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@agrsci.dk}, \cr Claus Dethlefsen, \email{aas.claus.dethlefsen@nja.dk}}
%\note{ ~~further notes~~ }

\seealso{\code{\link{gmData}}, \code{\link{gRfit}}, \code{\link{hllm}}, \code{\link{dynamic.Graph}}.}

\examples{
data(rats)
rats <- as.gmData(rats)

m1 <- gModel(~.^. , rats)
m1.form <- formula(m1)
m1.data <- gmData(m1)
observations(gmData(m1)) <- observations(rats)[1:10,]
}
\keyword{models}% at least one, from doc/KEYWORDS

