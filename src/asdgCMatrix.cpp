#include <RcppEigen.h>
#include <Rcpp.h>

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

RcppExport SEXP C_asdgCMatrix_st ( SEXP XX_ ){
  typedef Eigen::SparseMatrix<double> SpMat;
  typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
  MapMatd X(Rcpp::as<MapMatd>(XX_));
  SpMat Xsparse = X.sparseView();
  S4 Xout(wrap(Xsparse));
  NumericMatrix Xin(XX_);
  List dn = clone(List(Xin.attr("dimnames")));
  if (dn.length()>0){
    Xout.slot("Dimnames") = dn;
  }
  return(Xout);
}



