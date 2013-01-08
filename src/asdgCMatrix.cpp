#include <RcppEigen.h>
#include <Rcpp.h>

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

// sparse matrix
RcppExport SEXP C_asdgCMatrix_st ( SEXP XX_ ){
  typedef Eigen::SparseMatrix<double> SpMat;
  typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
  MapMatd X(Rcpp::as<MapMatd>(XX_));
  SpMat ans = X.sparseView();
  return(wrap(ans));
}



