/*
  Implements
  arrayPerm(x,p)  - p can be numeric (index) or dimnames
  arrayPerm0(x,p) - p must be integer vector
 */

#include <Rcpp.h>
using namespace Rcpp;

inline IntegerVector make_prod( int ndim, IntegerVector adim ){
  IntegerVector plevels = no_init( ndim );
  plevels[0] = 1;
  for (int i=1; i<ndim; i++){
    plevels[i] =  adim[i-1] * plevels[i-1];
  }
  return plevels;
}


IntegerVector get_perm_order3(IntegerVector perm, int N){
  IntegerVector vv ( N ), out = no_init(N);
  int psize=perm.size(), i, k=-1;
  for (i=0; i<psize; ++i){
    vv[ perm[i]-1 ] = -1;
    out[i] = perm[i];
  }
  k = psize-1;
  for (i=0; i<N; ++i){
    if (vv[i]>-1){
      out[++k] = i+1;
    }
  }
  //Rf_PrintValue( out );
  return out;
}


IntegerVector get_perm_char( const NumericVector& Xin, const CharacterVector& perm){
  List dn = Xin.attr("dimnames");
  CharacterVector nn = dn.names();
  IntegerVector mm=match( perm, nn );
  return get_perm_order3(mm, nn.length());
}


IntegerVector get_perm_num( const NumericVector& Xin, const IntegerVector& perm){
  // FIXME: Can be speeded up
  IntegerVector di = Xin.attr("dim");
  return get_perm_order3(perm, di.length());
}


// [[Rcpp::export]]
IntegerVector getPerm(const SEXP& XX_, const SEXP& perm){
  int ptype = TYPEOF( perm ) ;
  switch( ptype ){
  case INTSXP   :
  case REALSXP  : return get_perm_num (XX_, perm) ;
  case STRSXP   : return get_perm_char(XX_, perm) ;
  }
  return R_NilValue ;
}

// ------------------------------------------------------------------------------

#define DO_CELL                                                         \
  for (k=0, cell_number=0; k<ndim; k++){				\
    cell_number += pvec_perm[k] * (cell[k] - 1);			\
  }									\
  for (k=0; k<ndim; k++){						\
    if (cell[k] == adim_new[k])						\
      cell[k] = 1;							\
    else{								\
      cell[k]++;							\
      break;								\
    }									\
  };

template <int RTYPE>
Vector<RTYPE> aperm_internal_cpp(const Vector<RTYPE>& AA, const IntegerVector& adim, const IntegerVector& perm){
  int NN = AA.length(), ndim = adim.length(), i, k, cell_number, n;
  Vector<RTYPE> out  = no_init( NN );
  NumericVector cell = no_init( ndim );  // FIXME: Why not integer??

  IntegerVector pvec  = make_prod( ndim, adim );
  IntegerVector pvec_perm =no_init( ndim ), perm0 = no_init( ndim ), adim_new=no_init( ndim );

  for (i=0; i<ndim; i++){
    cell[i]      = 1;
    n            = perm[i]-1;
    perm0[i]     = n;
    pvec_perm[i] = pvec[n];
    adim_new[i]  = adim[n];
  }

  for (i=0; i<NN; i++){
    DO_CELL;
    out[i] = AA[cell_number];
  } // for

  return out ;
}

template <int RTYPE>
Vector<RTYPE> do_arrayPerm0_cpp(const Vector<RTYPE>& Xin, const IntegerVector& perm)
{
  //Rprintf("do_arrayPerm0_cpp\n");
  //Rf_PrintValue( Xin ); Rf_PrintValue( perm );
  IntegerVector di = Xin.attr("dim");
  int ndim = di.length();
  IntegerVector di2 = no_init( ndim );
  Vector<RTYPE> out=aperm_internal_cpp<RTYPE>(Xin, di, perm);

  for (int ii=0; ii<ndim; ii++){
    di2[ii]=di[perm[ii]-1];
  }
  out.attr("dim") = di2;

  List dn = clone(List(Xin.attr("dimnames")));
  if (dn.length()>0){
    List dn2( dn.length() );
    CharacterVector nn = dn.names();
    CharacterVector nn2 =no_init( ndim );

    for (int ii=0; ii<ndim; ii++){
      dn2[ii]=dn[perm[ii]-1];
      nn2[ii]=nn[perm[ii]-1];
    }
    dn2.names()=nn2;
    out.attr("dimnames") = dn2;
  }
  return out;
}

// [[Rcpp::export(arrayPerm0)]]
SEXP arrayPerm0_cpp ( SEXP XX_, SEXP perm ){
  int xtype = TYPEOF(XX_), ptype = TYPEOF( perm ) ;
  switch( ptype ){
  case INTSXP  :
  case REALSXP : break;
  default:     stop("'perm' must be numeric or integer vector");
  }

  switch( xtype ){
  case INTSXP : return do_arrayPerm0_cpp<INTSXP> ( XX_, perm ) ;
  case REALSXP: return do_arrayPerm0_cpp<REALSXP>( XX_, perm ) ;
  case STRSXP : return do_arrayPerm0_cpp<STRSXP> ( XX_, perm ) ;
  }
  return R_NilValue ;
}


// [[Rcpp::export(arrayPerm)]]
SEXP arrayPerm_cpp ( SEXP XX_, SEXP perm ){
  return arrayPerm0_cpp( XX_, getPerm( XX_, perm ) );
}



/*** R

x  <- HairEyeColor
storage.mode(x) <- "integer"
pp <- c(2,3,1)
arrayPerm0_cpp(x, pp)

##pp <- c("Eye","Sex","Hair")
##arrayPerm0_cpp(x, pp)

library(microbenchmark)
library(gRbase)

pp <- c(2,3,1)
microbenchmark(aperm(x, pp), tablePerm(x, pp), arrayPerm0_cpp(x,pp), aperm_cpp(x,pp))

pp <- as.integer(c(2,3,1))
microbenchmark(aperm(x, pp), tablePerm(x, pp), arrayPerm0_cpp(x,pp), aperm_cpp(x,pp), aperm3_cpp(x,pp))

pp <- c("Eye","Sex","Hair")
microbenchmark(tablePerm(x, pp), aperm_cpp(x,pp), aperm3_cpp(x,pp))

 */

// //[[Rcpp::export]]
// IntegerVector get_perm_order(IntegerVector perm, int N){
//   IntegerVector perm2 = na_omit(perm);
//   IntegerVector vv = seq(1, N);
//   IntegerVector ww = setdiff(vv, perm);
//   IntegerVector out = no_init(N);
//   int k=-1,i;
//   for (i=0; i<perm2.size(); ++i){
//     out[++k] = perm2[i];
//   }
//   for (i=0; i<ww.size(); ++i){
//     out[++k] = ww[i];
//   }
//   return out;
// }

// //[[Rcpp::export]]
// IntegerVector get_perm_order2(IntegerVector perm, int N){
//   //IntegerVector perm2 = na_omit(perm);
//   IntegerVector vv = seq(1, N);
//   int k=-1,i;
//   IntegerVector out = no_init(N);
//   for (i=0; i<perm.size(); ++i){
//     vv[ perm[i]-1 ] = -1;
//     out[i] = perm[i];
//   }
//   k = perm.size()-1;
//   for (i=0; i<N; ++i){
//     if (vv[i]>0){
//       out[++k] = vv[i];
//     }
//   }
//   return out;
// }
