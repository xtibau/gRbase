\name{wine}
\alias{wine}
\docType{data}
\title{
  Chemical composition of wine
}
\description{
  Using chemical analysis determine the origin of wines
}
\usage{data(wine)}
\format{
  A data frame with 178 observations on the following 14 variables.
  \describe{
    \item{\code{Cult}}{a factor with levels \code{v1} \code{v2}
      \code{v3}: 3 different graph varieties}
    \item{\code{Alch}}{Alcohol}
    \item{\code{Mlca}}{Malic acid}
    \item{\code{Ash}}{Ash}
    \item{\code{Aloa}}{Alcalinity of ash}
    \item{\code{Mgns}}{Magnesium}
    \item{\code{Ttlp}}{Total phenols}
    \item{\code{Flvn}}{Flavanoids}
    \item{\code{Nnfp}}{Nonflavanoid phenols}
    \item{\code{Prnt}}{Proanthocyanins}
    \item{\code{Clri}}{Color intensity}
    \item{\code{Hue}}{Hue}
    \item{\code{Oodw}}{OD280/OD315 of diluted wines}
    \item{\code{Prln}}{Proline}
  }
}

\details{
  Data comes from the UCI Machine Learning Repository. The grape variety
  \code{Cult} is the class identifier. 
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
  Frank, A. & Asuncion, A. (2010). UCI Machine Learning Repository
  [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California,
  School of Information and Computer Science.  
}
\references{
  See references at \url{http://archive.ics.uci.edu/ml/datasets/Wine}
}
\examples{
data(wine)
## maybe str(wine) ; plot(wine) ...
}
\keyword{datasets}
