\name{chestSim}
\alias{chestSim}
\alias{chestSim500}
\alias{chestSim1000}
\alias{chestSim10000}
\alias{chestSim50000}
\alias{chestSim100000}

\docType{data}
\title{Simulated data from the Chest Clinic example}
\description{
  Simulated data from the Chest Clinic example (also known as the Asia example) from Lauritzen and
  Spiegelhalter, 1988. 
}
\usage{data(chestSim500)}
\format{
  A data frame with 500 observations on the following 8 variables.
  \describe{
    \item{\code{asia}}{a factor with levels \code{yes} \code{no}}
    \item{\code{tub}}{a factor with levels \code{yes} \code{no}}
    \item{\code{smoke}}{a factor with levels \code{yes} \code{no}}
    \item{\code{lung}}{a factor with levels \code{yes} \code{no}}
    \item{\code{bronc}}{a factor with levels \code{yes} \code{no}}
    \item{\code{either}}{a factor with levels \code{yes} \code{no}}
    \item{\code{xray}}{a factor with levels \code{yes} \code{no}}
    \item{\code{dysp}}{a factor with levels \code{yes} \code{no}}
  }
}
% \details{
%   ~~ If necessary, more details than the __description__ above ~~
% }
% \source{
%   ~~ reference to a publication or URL from which the data were obtained ~~
% }
\references{
  Lauritzen and Spiegelhalter (1988) Local Computations
 with Probabilities on Graphical Structures and
 their Application to Expert Systems (with
 Discussion). J. Roy. Stat. Soc. 50, p. 157-224.
}

\examples{
data(chestSim500)
## maybe str(chestSim500) ; plot(chestSim500) ...
}
\keyword{datasets}
