\name{gRbase}
\alias{gRbase}
\title{
  The package `gRbase': summary information
}
\description{
  This package provides a basis for graphical modelling in R and in
  particular for other graphical modelling packages, most notably
  \pkg{gRim} and \pkg{gRain}.   The package is a contribution to the gR-project 
  described by Lauritzen (2002).
}
\section{Details}{
  \pkg{gRbase} provides the following:
  \itemize{
    \item Implementation of various graph algorithms, including maximum
    cardinality search, maximal prime subgraph decomposition,
    triangulation. See the vignette \code{graphs}.

    \item Implementation of various "high level" array operations, including
    multiplication/division, marginalization, slicing, permutation. See
    the vignette \code{ArrayOps}.

    \item Implementation of various "low level" array operations. See
    the vignette \code{ArrayOpsPrim}. 
    
    \item A collection of datasets
    
    \item A general framework for setting up data and model structures
    and provide examples for fitting hierarchical log linear 
    models for contingency tables and graphical Gaussian models for the
    multivariate normal distribution.

    Notice: This last part is not maintained / developed further.
    
  }
}
\section{Authors}{
  Søren Højsgaard, Aarhus University, DK-8830 Tjele, Denmark
  
  Claus Dethlefsen, Center for Cardiovascular Research, Aalborg Hospital,
  Århus University Hospital, DK-9000 Aalborg, Denmark

  Code for maximal prime subgraph decomposition has been provided by
  Clive Bowsher.
}
\section{Acknowledgements}{
  Thanks to the other members of the gR initiative, in particular to
  David Edwards for providing functions for formula-manipulation.
}
\references{
  Lauritzen, S. L. (2002). gRaphical Models in R.
  \emph{R News}, 3(2)39.
}
\keyword{multivariate}
\keyword{models}
\keyword{graphs}
