\name{gModel-class}
\docType{class}
\alias{gModel-class}

\title{Class "gModel" - graphical models }
\description{The general class \code{gModel} contains a formula object and a
\code{gmData} object. Implementations of different
specific graphical model classes can inherit from this class and
provide methods for parsing the formula. This is illustrated in the
implementation of a class for hierarchical log--linear models, \code{hllm}.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("gModel", ...)}.
    ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{formula}:}{Object of class \code{"formula"} holding the
      formal definition of the model in a format that is understood by
      the specific graphical model class. }
    \item{\code{gmData}:}{Object of class \code{"gmData"} holding meta
      data information about variables. }
  }
}
\section{Methods}{
No methods defined with class "gModel" in the signature.
}
\author{Søren Højsgaard, \email{sorenh@agrsci.dk}, \cr Claus Dethlefsen, \email{dethlef@math.aau.dk}}

\seealso{\code{\link{gmData-class}}}
\keyword{classes}
