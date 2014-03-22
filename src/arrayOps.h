#ifndef ARRAYOPS1_H
#define ARRAYOPS1_H

#include <Rcpp.h>
using namespace Rcpp;

List extendArrays_cpp(NumericVector a1, NumericVector a2);
List alignArrays_cpp(NumericVector a1, NumericVector a2);

SEXP arrayMargin_cpp( SEXP a1, SEXP marg );
NumericVector arrayMargin0_cpp( NumericVector a1, IntegerVector marg );
NumericVector arrayMarginChr_cpp( NumericVector a1, CharacterVector marg );

NumericVector arrayOp_cpp(NumericVector a1, NumericVector a2, const char op='*');
NumericVector arrayOp2_cpp(const NumericVector& a1, const NumericVector& a2, const char op='*');

#endif
