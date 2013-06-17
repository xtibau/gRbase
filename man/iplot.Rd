\name{iplot}
\alias{iplot}
\alias{iplot.graphNEL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Function for plotting graphs using the 'igraph' package.
}
\description{
  Generic function for plotting graphs using the 'igraph' package and a
  plot method for graphNEL objects.
  
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
iplot(x, ...)
\method{iplot}{graphNEL}(x,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A graph object to be plotted.
  }
  \item{\dots}{
    Additional arguments
  }
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{Søren Højsgaard, \email{sorenh@math.aau.dk}}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
UG <- ug(~a:b+b:c:d)
iplot(UG)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graphics}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
