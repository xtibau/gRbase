#include "arrayOps.h"

using namespace Rcpp;

// //[[Rcpp::export]]
List get_marg_arrays(NumericVector x, List glist){
  int ng = glist.size();
  List out(ng);
  for (int i=0; i<ng;++i){
    out[ i ] =arrayMargin_cpp( x , glist[ i ]);
  }
  return out;
}


inline double get_pearson(NumericVector x, NumericVector f){
  return sum( pow(x-f,2)/f ) ;
  //return sum( (x-f)*(x-f)/f ) ;
}


//[[Rcpp::export(fitLoglin)]]
List fitLoglin_cpp( NumericVector x, List glist, double peps=1e-3){
  NumericVector fitted = clone(x);
  std::fill(fitted.begin(), fitted.end(), 1 );

  NumericVector adj, fitmarg;

  List SS   = get_marg_arrays( x, glist );
  int i, ng = glist.size(), itcount=0;
  double p0 = get_pearson( x, fitted ), p1, dp;

  while(1){
    itcount++;
    for (i=0; i<ng; ++i){
      fitmarg = arrayMarginChr_cpp( fitted, glist[i] );
      adj     = arrayOp2_cpp( SS[i], fitmarg, '/');
      fitted  = arrayOp2_cpp( fitted, adj, '*');
      //std::cout << "iteration=" << itcount << " inner=" << i <<std::endl;
      //Rf_PrintValue(fitted);
    }
    p1 = get_pearson( x, fitted );
    dp = fabs( (p0-p1) / p0);
    //std::cout << " p0=" << p0 << " p1=" << p1 << " dp=" << dp << std::endl;
    if (dp<peps)
      break;
    p0=p1;
  }

  return List::create(Named("fit")=fitted,
		      Named("pearson")=p1,
		      Named("iterations")=itcount);
}
