\name{gmData-class}
\docType{class}
\alias{gmData-class}
\alias{coerce,data.frame,gmData-method}
\alias{coerce,table,gmData-method}
\alias{coerce,array,gmData-method}
\alias{description,gmData-method}
\alias{description<-,gmData-method}
\alias{dynamic.Graph,gmData-method}
\alias{initialize,gmData-method}
\alias{latent,gmData-method}
\alias{latent<-,gmData-method}
\alias{numberLevels,gmData-method}
\alias{numberLevels<-,gmData-method}
\alias{observations,gmData-method}
\alias{observations<-,gmData-method}
\alias{show,gmData-method}
\alias{valueLabels,gmData-method}
\alias{valueLabels<-,gmData-method}
\alias{varNames,gmData-method}
\alias{varNames<-,gmData-method}
\alias{varTypes,gmData-method}
\alias{varTypes<-,gmData-method}

\title{Class "gmData" graphical meta data}
\description{A common class for representing data. No matter the actual
  representation of data, the important characteristics are contained in
  a graphical metadata object. }
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("gmData",
    varNames, varTypes, numberLevels, latent, valueLabels,
    observations)}.

  
  A \code{gmData} object contains the abstraction of data into a
  meta data object including variable names and types etc. However, the
  actual data might not be present or may be represented by a reference
  to data, such as a database file. Also, it may be possible to work
  without data, which may be valuable if the point of interest is in the
  model alone. Separating the specification of the
  variables from data has the benefit, that some properties of a model can be
  investigated without any reference to data, for example
  decomposability and collapsibility.
}

  
\section{Slots}{
  \describe{
    \item{\code{description}:}{Object of class \code{"data.frame"}
      containing one row for each variable and a column for each
      property of the variables. The properties include the variable
      name, the number of levels (for discrete variables), whether the
      variable is latent or not, and the variable type. The description
      may be extended by adding columns. } 
    \item{\code{valueLabels}:}{Object of class \code{"vector"} giving
      the names of the levels of the discrete variables. }
    \item{\code{observations}:}{Object of class \code{"dataOrNULL"}
      containing the data, a pointer to data or NULL if no data are used. }
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "data.frame", to = "gmData")}:
      Create gmData object from a data.frame. }
    \item{coerce}{\code{signature(from = "table", to = "gmData")}: Create gmData object from a table. }
    \item{coerce}{\code{signature(from = "array", to = "gmData")}: Create gmData object from an array. }
    \item{description}{\code{signature(x = "gmData")}: Extract the
      description slot from a gmData object. }
    \item{description<-}{\code{signature(x = "gmData")}: Replace the
      description slot from a gmData object. }
    \item{dynamic.Graph}{\code{signature(object = "gmData")}: ... }
    \item{initialize}{\code{signature(.Object = "gmData")}: Creation of
      a gmData object. }
    \item{latent}{\code{signature(x = "gmData")}: Extract the
      column "latent" from the description slot from a gmData object. }
    \item{latent<-}{\code{signature(x = "gmData")}: Replace the
      column "latent" from the description slot from a gmData object. }
    \item{numberLevels}{\code{signature(x = "gmData")}: Extract the
      column "numberLevels" from the description slot from a gmData object. }
    \item{numberLevels<-}{\code{signature(x = "gmData")}: Replace the
      column "numberLevels" from the description slot from a gmData object. }
    \item{observations}{\code{signature(x = "gmData")}: Extract the
      observations slot from a gmData object. }
    \item{observations<-}{\code{signature(x = "gmData")}: Replace the
      observations slot from a gmData object. }
    \item{show}{\code{signature(object = "gmData")}: Print method for
      gmData object. }
    \item{valueLabels}{\code{signature(x = "gmData")}: Extract the
      valueLabels slot from a gmData object. }
    \item{valueLabels<-}{\code{signature(x = "gmData")}: Replace the
      valueLabels slot from a gmData object. }
    \item{varNames}{\code{signature(x = "gmData")}: Extract the
      column "varNames" from the description slot from a gmData object.  }
    \item{varNames<-}{\code{signature(x = "gmData")}: Replace the
      column "varNames" from the description slot from a gmData object. }
    \item{varTypes}{\code{signature(x = "gmData")}: Extract the
      column "varTypes" from the description slot from a gmData object. }
    \item{varTypes<-}{\code{signature(x = "gmData")}: Replace the
      column "varTypes" from the description slot from a gmData object. }
  }
}
\references{ ~put references to the literature/web site here ~ }
\author{Søren Højsgaard, \email{sorenh@agrsci.dk}, \cr Claus Dethlefsen, \email{dethlef@math.aau.dk}}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{demo(gmData)}}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%}
\keyword{classes}
