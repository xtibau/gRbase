/*
  Implements
  allSubsets_cpp(x)  - 'any' vector x
  allSubsets0_cpp(x) - integer vector x

 */


#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::interfaces(r,cpp)]]

template <const int RTYPE>
inline Vector<RTYPE> do_subsetter_idx_ (const Vector<RTYPE>& x, const IntegerVector& idx){
  Vector<RTYPE> out=no_init( idx.size() );
  for (int i=0; i<idx.size(); ++i){
    out[ i ] = x[ idx[ i ] - 1 ];
  }
  return out;
}

inline IntegerVector conc_int(const IntegerVector& x, int y){
  int nx=x.size(), i;
  IntegerVector out=no_init(nx+1);
  for (i=0; i<nx; ++i) out[ i ] = x[ i ];
  out[ nx ] = y;
  return out;
}


// Works for integer input vector
//[[Rcpp::export(allSubsets0)]]
List allSubsets0_cpp( IntegerVector x ){
  int nx = x.length(), nout=pow(2., nx), i, k, ny=1;
  List out( nout );
  out[0]=-1;
  double z;
  IntegerVector tmp;
  for (i=0; i<nx; ++i){
    z = x[i];
    for (k=0; k<ny; ++k){
      tmp = conc_int(out[k], z);
      out[ny+k] = tmp;
    }
    ny = 2*ny;
  }

  for (i=1; i<nout; ++i){
    IntegerVector aa=out[i];
    int M = aa.length();
    IntegerVector vv = no_init( M-1 );
    for (k=1; k<M; ++k){
      vv[k-1]=aa[k];
    }
    out[i-1] = vv;
  }
  out.erase(out.end()-1, out.end());
  return out;
}

template <int RTYPE>
List do_allSubsets (Vector<RTYPE> vn){
  IntegerVector sq = seq_len( vn.size() );
  List lst = allSubsets0_cpp( sq );
  int N=lst.size(), i;
  for (i=0; i<N; ++i){
    lst[i] = do_subsetter_idx_<RTYPE>( vn, lst[i] );
  }
  return lst;
}


// Works for any type of input vector
// [[Rcpp::export(allSubsets)]]
SEXP allSubsets_cpp( SEXP& XX_){
  int type = TYPEOF(XX_) ;
  switch( type ){
  case INTSXP  : return allSubsets0_cpp( XX_ ) ;
  case REALSXP : return do_allSubsets<REALSXP>( XX_ ) ;
  case STRSXP  : return do_allSubsets<STRSXP> ( XX_ ) ;
  }
  return R_NilValue ;
}





/*** R

allSubsets0_R <- function(x) {
        y <- list(vector(mode(x), length = 0))
        for (i in seq_along(x)) {
            y <- c(y, lapply(y, "c", x[i]))
        }
        y[-1L]
    }

allSubsets1_R <- compiler::cmpfun(
function(x){
    out <- vector("list", length=2^length(x))
    ny = 1 # filled elements of out
    for (i in seq_along(x)){
        z=x[i]
        for (k in 1:ny){
            out[[ny + k]] = c(out[[k]],z)
        }
        ny = 2 * ny
    }
    out[-1]
})


library(microbenchmark)
x <- 1:4
microbenchmark(allSubsets0_R( x ), allSubsets1_R( x ), allSubsets_cpp(x))

x <- 1:10
microbenchmark(allSubsets0_R( x ), allSubsets1_R( x ), allSubsets_cpp(x))

x <- 1:15
microbenchmark(allSubsets0_R( x ), allSubsets_cpp(x),times=10)

 */
