\name{mildew}
\alias{mildew}
\docType{data}
\title{Mildew fungus}
\description{
 The data stem from a cross between two isolates of the barley powdery mildew fungus. For each offspring
 6 binary characteristics, each corresponding to a single locus, were recorded. The object of the analysis
 is to determine the order of the loci along the chromosome.
}
\usage{data(mildew)}
\format{
  The format is:
 table [1:2, 1:2, 1:2, 1:2, 1:2, 1:2] 0 0 0 0 3 0 1 0 0 1 ...
 - attr(*, "dimnames")=List of 6
  ..$ la10: chr [1:2] "1" "2"
  ..$ locc: chr [1:2] "1" "2"
  ..$ mp58: chr [1:2] "1" "2"
  ..$ c365: chr [1:2] "1" "2"
  ..$ p53a: chr [1:2] "1" "2"
  ..$ a367: chr [1:2] "1" "2"
}
% \details{
%   ~~ If necessary, more details than the __description__ above ~~
% }
% \source{
%   ~~ reference to a publication or URL from which the data were obtained ~~
% }
\references{
Christiansen, S.K., Giese, H (1991) Genetic analysis of obligate barley
powdery mildew fungus based on RFLP and virulence
loci. Theor. Appl. Genet. 79:705-712
}
\examples{
data(mildew)
## maybe str(mildew) ; plot(mildew) ...
}
\keyword{datasets}
