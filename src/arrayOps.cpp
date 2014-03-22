#include "arrayPerm.h"

using namespace Rcpp;

// --- UTILITIES ---
inline int prod(Rcpp::IntegerVector x){
  return std::accumulate(x.begin(), x.end(), 1, std::multiplies<double>());
}


inline IntegerVector make_prod( int ndim, IntegerVector adim ){
  IntegerVector plevels = no_init( ndim );
  plevels[0] = 1;
  for (int i=1; i<ndim; i++){
    plevels[i] =  adim[i-1] * plevels[i-1];
  }
  return plevels;
}

template <const int RTYPE>
Vector<RTYPE> do_concat_(const Vector<RTYPE> x, const Vector<RTYPE> y){
  int nx=x.size(), n=x.size()+y.size(),i,j;
  Vector<RTYPE> out=no_init(n);
  for (i=0; i<nx; ++i){ out[ i ] = x[ i ];}
  for (j=i, i=0; j<n; ++j, ++i){ out[ j ] = y[i] ;}
  return out;
}

template <const int RTYPE>
inline Vector<RTYPE> do_subsetter_idx_ (const Vector<RTYPE>& x, const IntegerVector& idx){
  Vector<RTYPE> out=no_init( idx.size() );
  for (int i=0; i<idx.size(); ++i){
    out[ i ] = x[ idx[ i ] - 1 ];
  }
  return out;
}

inline bool isna_( double x) {  return std::isnan(x) ; }

inline bool isinf_( double x){  return std::isinf(x) ; }

inline NumericVector colSums_(const NumericMatrix& tmp){
  int NC=tmp.ncol();
  NumericVector out = no_init(NC);
  for (int j=0; j<NC; ++j){
    out[j] = sum( tmp( _, j ) );
  }
  return out;
}

inline NumericVector rowSums_(const NumericMatrix& tmp){
  int NC=tmp.nrow(), M=tmp.ncol();
  double sum;
  NumericVector out = no_init(NC);
  for (int j=0; j<NC; ++j){
    sum = 0;
    for (int k=0; k<M; ++k)
      sum += tmp( j, k);
    out[j] = sum;
  }
  return out;
}


// --- UTILITIES --- END -------------------------------------------

//FIXME: get_array_info: Kunne godt have et indikator-slot, der siger om
// det er en tabel, der kan permuteres og laves andre operationer på

// //[[Rcpp::export]]
inline List get_array_info(const NumericVector& a1){
  List dn = a1.attr("dimnames");
  IntegerVector di=a1.attr("dim");
  CharacterVector vn=dn.names();
  return List::create(_["di"]=di, _["dn"]=dn, _["vn"]= vn );
}

// //[[Rcpp::export]]
inline NumericVector set_array_info(NumericVector x, IntegerVector di, List dn, CharacterVector vn){
  dn.names()=vn;
  x.attr("dim")=di;
  x.attr("dimnames")=dn;
  return x;
}

// //[[Rcpp::export]]
// inline NumericVector set_array_info(NumericVector x, IntegerVector di, List dn, CharacterVector vn){
//   NumericVector x2 = clone(x);
//   dn.names()=vn;
//   x2.attr("dim")=di;
//   x2.attr("dimnames")=dn;
//   return x2;
// }

// //[[Rcpp::export]]
inline List marg_array_info(const List& x, const IntegerVector& idx){
  IntegerVector di=x["di"], di2=no_init(idx.length());
  List dn=x["dn"], dn2=no_init(idx.length());
  CharacterVector vn=x["vn"], vn2=no_init(idx.length());
  //std::cout << "marg_array_info \n";  Rf_PrintValue( x );
  for (int i=0; i<idx.length(); ++i){
    di2[i]=di[ idx[i]-1 ];
    dn2[i]=dn[ idx[i]-1 ];
    vn2[i]=vn[ idx[i]-1 ];
  }
  dn2.names()=vn2;
  return List::create(_["di"]=di2, _["dn"]=dn2, _["vn"]=vn2);
}


// //[[Rcpp::export]]
inline IntegerVector get_perm_order(IntegerVector perm, int N){
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

/*
  vn1_idx  = 1,2,3,...,N
  marg_idx = 2,4,5
  comp_idx = 1,3,6,7,..
  vn2_idx  = 1,3,6,7,...,2,4,5 = c(comp_idx, marg_idx)

 */


//[[Rcpp::export(arrayMargin0)]]
NumericVector  arrayMargin0_cpp(NumericVector a1, IntegerVector marg_idx )
{
  List a1info = get_array_info( a1 );
  CharacterVector vn1 = a1info["vn"];
  IntegerVector   di1 = a1info["di"];
  IntegerVector pp = get_perm_order(marg_idx, vn1.size());

  //Rf_PrintValue(pp);
  NumericVector a1p = arrayPerm0_cpp(a1, pp);

  IntegerVector comp_idx = no_init(vn1.size()-marg_idx.size());
  std::copy(pp.begin()+marg_idx.size(), pp.end(), comp_idx.begin());
  //Rf_PrintValue( comp_idx );

  IntegerVector di_comp = do_subsetter_idx_( di1, comp_idx );
  IntegerVector di_marg = do_subsetter_idx_( di1, marg_idx );
  //Rf_PrintValue(di_comp); Rf_PrintValue(di_marg);

  int NC=prod( di_marg ), NR=prod( di_comp );
  NumericMatrix tmp ( NC, NR );
  std::copy(a1p.begin(), a1p.end(), tmp.begin());
  //Rf_PrintValue(tmp);

  List marginfo = marg_array_info( a1info, marg_idx );
  //  Rf_PrintValue( marginfo );
  NumericVector out=rowSums_( tmp );
  //Rf_PrintValue(out);
  return set_array_info( out, marginfo["di"], marginfo["dn"], marginfo["vn"]);
  //return a1;
}

//[[Rcpp::export]]
NumericVector arrayMarginChr_cpp(NumericVector a1, CharacterVector marg )
{
  List              a1info = get_array_info( a1 );
  CharacterVector      vn1 = a1info["vn"];
  IntegerVector   marg_idx = match( marg, vn1 );
  return arrayMargin0_cpp( a1, marg_idx );
}


//[[Rcpp::export(arrayMargin)]]
SEXP arrayMargin_cpp ( SEXP a1, SEXP marg ){
  int type = TYPEOF( marg ) ;
  switch( type ){
  case INTSXP  :
  case REALSXP : return arrayMargin0_cpp( a1, marg );
  case STRSXP  : return arrayMarginChr_cpp( a1, marg );
  }
  return R_NilValue ;
}





//[[Rcpp::export(extendArrays)]]
List extendArrays_cpp(const NumericVector& a1, const NumericVector& a2)
{
  //std::cout << "extendArrays_cpp..." << std::endl;
  typedef CharacterVector charVec;
  typedef IntegerVector   intVec;
  typedef NumericVector   numVec;

  int i, j, k, l1 = a1.size(), l2 = a2.size();

  List a1info = get_array_info( a1 );
  List a2info = get_array_info( a2 );

  intVec  di1 = a1info["di"], di2 = a2info["di"];
  List    dn1 = a1info["dn"], dn2 = a2info["dn"];
  charVec vn1 = a1info["vn"], vn2 = a2info["vn"];

  List out(2);
  charVec r1 = setdiff( vn2, vn1 );
  charVec r2 = setdiff( vn1, vn2 );

  // handle a1
  if (r1.length() > 0){
    intVec  idx_r1 = match( r1, vn2 );
    intVec  di_r1  = do_subsetter_idx_<INTSXP>(di2, idx_r1);

    int pdi_r1 = prod( di_r1 );
    numVec a1ext = no_init( l1 * pdi_r1 );
    for (k=0, i=0; i<l1; ++i){
      for (j=0; j<pdi_r1; ++j) a1ext[k++] = a1[ i ];
    }

    intVec  di1ext  = do_concat_<INTSXP>( di_r1, di1 );
    charVec vn1ext  = do_concat_<STRSXP>( r1, vn1 );

    List dn1ext( vn1ext.length() );
    for (i=0; i<idx_r1.size(); i++)
      dn1ext[i] = dn2[idx_r1[i]-1];

    for (j=i, i=0; i<dn1.size(); i++, j++)
      dn1ext[j] = dn1[i];

    a1ext = set_array_info( a1ext, di1ext, dn1ext, vn1ext);
    out[0] = a1ext;
  } else{
    out[0] = a1;
  }

  // handle a2
  if (r2.length()>0){
    intVec  idx_r2 = match( r2, vn1 );
    intVec di_r2   = do_subsetter_idx_<INTSXP>(di1, idx_r2);

    int pdi_r2 = prod( di_r2 );
    numVec a2ext = no_init( l2 * pdi_r2 );

    for (k=0, i=0; i<l2; ++i){
      for (j=0; j<pdi_r2; ++j) a2ext[k++] = a2[ i ];
    }

    intVec  di2ext  = do_concat_<INTSXP>( di_r2, di2 );
    charVec vn2ext  = do_concat_<STRSXP>( r2, vn2 );

    List dn2ext( vn2ext.length() );
    for (i=0; i<idx_r2.size(); i++)
      dn2ext[i] = dn1[idx_r2[i]-1];

    for (j=i, i=0; i<dn2.size(); i++, j++)
      dn2ext[j] = dn2[i];

    a2ext = set_array_info( a2ext, di2ext, dn2ext, vn2ext);
    out[1] = a2ext;
  } else{
    out[1] = a2;
  }
  //std::cout << "... exit" << std::endl;

  return out;
}


// // New versions of extendAlong
//[[Rcpp::export(extendArrayAlong)]]
NumericVector extendArrayAlong_cpp(NumericVector a1, NumericVector a2)
{
  // Extends a2 so that its domain contains the domain of a1

  List a1info = get_array_info( a1 );
  List a2info = get_array_info( a2 );
  List    dn1 = a1info["dn"], dn2 = a2info["dn"];
  IntegerVector   di1 = a1info["di"], di2 = a2info["di"];
  CharacterVector vn1 = a1info["vn"], vn2 = a2info["vn"];

  CharacterVector r2  = setdiff( vn1, vn2 ); // what should be added to a2

  if (r2.length()>0){
    int i, j, k, l2 = a2.size();
    IntegerVector r2_idx = match( r2, vn1 );
    IntegerVector di_r2  = do_subsetter_idx_<INTSXP>( di1, r2_idx );
    int pdi_r2 = prod( di_r2 );
    NumericVector a2ext = no_init( l2 * pdi_r2 );

    for (k=0, i=0; i<l2; ++i)
      for (j=0; j<pdi_r2; ++j)
	a2ext[k++] = a2[ i ];

    IntegerVector   di2ext  = do_concat_<INTSXP>( di_r2, di2 );
    CharacterVector vn2ext  = do_concat_<STRSXP>( r2,    vn2 );

    List dn2ext( vn2ext.length() );
    for (i=0; i<r2_idx.size(); ++i)
      dn2ext[i] = dn1[ r2_idx[i]-1 ];
    for (j=i, i=0; i<dn2.size(); i++, j++)
      dn2ext[j] = dn2[i];

    a2ext = set_array_info( a2ext, di2ext, dn2ext, vn2ext);

    return a2ext;
  } else {
    return a2;
  }
}







//[[Rcpp::export(alignArrays)]]
List alignArrays_cpp(NumericVector a1, NumericVector a2){
  //std::cout << "alignArrays_cpp..." << std::endl;
  List align  = extendArrays_cpp( a1, a2 );
  List a1info = get_array_info( align[0] );
  List a2info = get_array_info( align[1] );

  CharacterVector vn1 = a1info["vn"], vn2=a2info["vn"] ;

  IntegerVector   idx2in1 = match( vn1, vn2 );
  NumericVector   a2perm  = arrayPerm0_cpp( align[1], idx2in1 );
  //std::cout << "alignArrays_cpp... exit..." << std::endl;
  return List::create(align[0], a2perm);
}

//[[Rcpp::export(alignArrays2)]]
List alignArrays2_cpp(NumericVector a1, NumericVector a2){
  // assumes domain(a1)>=domain(a2) FIXME: not checked!
  NumericVector a2e = extendArrayAlong_cpp(a1, a2);
  List a1info = get_array_info( a1  );
  List a2info = get_array_info( a2e );

  CharacterVector vn1 = a1info["vn"], vn2=a2info["vn"] ;
  IntegerVector   idx2in1 = match( vn1, vn2 );
  // a2e = arrayPerm0_cpp( a2e, idx2in1 );
  // return List::create(a1, a2e);

  NumericVector a2perm = arrayPerm0_cpp( a2e, idx2in1 );
  return List::create(a1, a2perm);

}




// ------------------------------------------------------------------------------


#define DO_CELL                                                         \
  for (k=0, cell1_idx=0; k<ndim1; k++){				\
    cell1_idx += pvec1[k] * (cell1[k] - 1);				\
  }									\
  for (k=0, cell2_idx=0; k<ndim2; k++){				\
    cell2_idx += pvec2[k] * (cell2[k] - 1);				\
  }									\
  for (k=0; k<ndim1; k++){						\
    if (cell1[k] == di1[k])						\
      cell1[k] = 1;							\
    else{								\
      cell1[k]++;							\
      break;								\
    }									\
  };

// This fn is way faster than arrayOp2 !!!
// Needs thorough checking
//[[Rcpp::export(arrayOp2)]]
NumericVector arrayOp2_cpp(const NumericVector& a1, const NumericVector& a2, const char op='*'){
  NumericVector out=clone( a1 );

  List a1info = get_array_info( a1 );
  CharacterVector vn1 = a1info["vn"];
  IntegerVector   di1 = a1info["di"];

  List a2info = get_array_info( a2 );
  CharacterVector vn2 = a2info["vn"];
  IntegerVector   di2 = a2info["di"];

  IntegerVector mm = match( vn2, vn1 );
  //Rf_PrintValue( mm );

  int N1 = a1.length(), ndim1 = di1.length(), ndim2 = di2.length(), i, j, k,
    cell1_idx, cell2_idx;
  NumericVector cell1 = no_init( ndim1 ), cell2=no_init(ndim2);  // FIXME: Why not integer??

  IntegerVector pvec1  = make_prod( ndim1, di1 );
  IntegerVector pvec2  = make_prod( ndim2, di2 );
  //Rf_PrintValue( pvec1 ); Rf_PrintValue( pvec2 );

  for (i=0; i<ndim1; i++) cell1[i] = 1;
  for (i=0; i<ndim2; i++) cell2[i] = 1;

  for (i=0; i<N1; i++){
    //Rprintf("cell1:"); Rf_PrintValue( cell1 );
    for (j=0; j<ndim2; ++j){
      cell2[ j ] = cell1[ mm[j] - 1 ];
    }
    //Rprintf("cell2:"); Rf_PrintValue( cell2 );
    DO_CELL;
    //Rprintf("cell1_idx=%d, cell2_idx=%d\n", cell1_idx, cell2_idx);
    //out[i] = a1[ cell1_idx ] * a2[ cell2_idx ];

    switch( op ){
    case '/' : {
      out[i] = a1[ cell1_idx ] / a2[ cell2_idx ];
      if (( isna_( out[i] ) ) | ( isinf_( out[i] ))) out[i]=0;
      break;
    }
    case '*' : {   out[i] = a1[ cell1_idx ] * a2[ cell2_idx ]; break;}
    case '+' : {   out[i] = a1[ cell1_idx ] + a2[ cell2_idx ]; break;}
    case '-' : {   out[i] = a1[ cell1_idx ] - a2[ cell2_idx ]; break;}
    default : stop("'op' is an undefined operation");
    }
  } // for

  return out ;
}

//[[Rcpp::export(arrayOp)]]
NumericVector arrayOp_cpp(const NumericVector& a1, const NumericVector& a2, const char op='*'){
  NumericVector a1c = extendArrayAlong_cpp( a2, a1 );
  return arrayOp2_cpp( a1c, a2, op);
}





// -----------------------------------------------------------------
// OBSOLETE: extendArrayAlong
// This version is obsolete; a faster version has been implemented
// -----------------------------------------------------------------

//[[Rcpp::export(.extendArrayAlongOLD)]]
NumericVector extendArrayAlongOLD_cpp(const NumericVector& a1, const NumericVector& a2)
{
  // extends a2 to have the same domain as a1;
  // FIXME: does not check that domain(a2) is subset of domain(a1)
  typedef CharacterVector charVec;
  typedef IntegerVector   intVec;
  typedef NumericVector   numVec;

  int i, j, k, l2 = a2.size();

  List a1info = get_array_info( a1 );
  List a2info = get_array_info( a2 );
  intVec  di1 = a1info["di"], di2 = a2info["di"];
  List    dn1 = a1info["dn"], dn2 = a2info["dn"];
  charVec vn1 = a1info["vn"], vn2 = a2info["vn"];

  charVec r2     = setdiff( vn1, vn2 );

  if (r2.length()>0){
    intVec  idx_r2 = match( r2, vn1 );
    intVec  di_r2  = do_subsetter_idx_<INTSXP>(di1, idx_r2);

    int pdi_r2 = prod( di_r2 );
    numVec a2ext = no_init( l2 * pdi_r2 );

    for (k=0, i=0; i<l2; ++i){
      for (j=0; j<pdi_r2; ++j) a2ext[k++] = a2[ i ];
    }

    intVec  di2ext  = do_concat_<INTSXP>( di_r2, di2 );
    charVec vn2ext  = do_concat_<STRSXP>( r2,    vn2 );

    List dn2ext( vn1.length() );
    for (i=0; i<idx_r2.size(); i++)
      dn2ext[i] = dn1[idx_r2[i]-1];

    for (j=i, i=0; i<dn2.size(); i++, j++)
      dn2ext[j] = dn2[i];

    a2ext = set_array_info( a2ext, di2ext, dn2ext, vn2ext);

    return a2ext;
  } else {
    return a2;
  }
}

// -----------------------------------------------------------------
// End OBSOLETE
// -----------------------------------------------------------------


// -----------------------------------------------------------------
// OBSOLETE: arrayOp
// This version is obsolete; a faster version has been implemented
// -----------------------------------------------------------------

inline NumericVector do_arrayOp_cpp(List align, const char op='*')
{
  //input: a list of aligned arrays (same domains in same order)
  NumericVector a1e = align[0], a2e=align[1];
  NumericVector res = no_init(a1e.size());

  switch( op ){
  case '/' : {
    res = a1e / a2e;
    int n=res.size(), i;
    for (i=0; i<n; ++i){
      if (( isna_( res[i] ) ) | ( isinf_( res[i] ))) res[i]=0;
    }
    break;
  }
  case '*' : {   res = a1e * a2e; break;}
  case '+' : {   res = a1e + a2e; break;}
  case '-' : {   res = a1e - a2e; break;}
  default : stop("'op' is an undefined operation");
  }

  IntegerVector di  = a1e.attr("dim");
  List          dn  = a1e.attr("dimnames");
  res.attr("dim") = di;
  res.attr("dimnames") = dn;

  return res;
}

//[[Rcpp::export(.arrayOpOLD)]]
NumericVector arrayOpOLD_cpp(NumericVector a1, NumericVector a2, const char op='*'){
  List align = alignArrays_cpp( a1, a2 );
  return do_arrayOp_cpp( align, op );
}

//[[Rcpp::export(.arrayOp2OLD)]]
NumericVector arrayOp2OLD_cpp(const NumericVector& a1, const NumericVector& a2, const char op='*')
{
  List align = alignArrays2_cpp( a1, a2 );
  return do_arrayOp_cpp( align, op );
}

// -----------------------------------------------------------------
// End OBSOLETE
// -----------------------------------------------------------------
























/*** R

library(microbenchmark)

microbenchmark( propLS(pp, rr), propagateLS(pp, rr))



data(lizard, package="gRbase")
t1 <- arrayMargin_cpp(lizard, c("species","diam"))
t2 <- arrayMargin_cpp(lizard, c("species","height"))

arrayMargin_cpp(t1, "species") #rigtig

load_all("mypack"); arrayMargin_cpp(t1, "diam") #forkert dimnames; sum er ok


library(microbenchmark)
library(gRbase)

microbenchmark( arrayMargin(lizard, c("species","diam")), arrayMargin_cpp(lizard, c("species","diam")), arrayMargin2_cpp(lizard, c("species","diam")))


glist<-list(c("species","diam"),c("species","height"),c("diam","height"))

SS=get_marg_arrays(lizard, glist)

loglin_cpp( lizard, glist )

x <- lizard; fitted=x; x[]=1;

fitmarg <- arrayMargin_cpp(fitted, glist[[1]])
adj     <- arrayOp_cpp( SS[[1]], fitmarg, "/")
arrayOp_cpp(fitted, adj, "*")






fit <- lizard




#arrayMargin(lizard, c("species","height"))
#arrayMargin_cpp(lizard, c("species","height"))

*/



// myips <- function(dat, glist){
//     fit<-dat
//     fit[] <- 1
//     md1  <-tableMargin_cpp(dat, glist[[1]])
//     md2  <-tableMargin_cpp(dat, glist[[2]])
//     md3  <-tableMargin_cpp(dat, glist[[3]])

//     for (i in 1:4){
//         mf  <-tableMargin_cpp(fit, glist[[1]])
//         adj <- tableOp_cpp( md1, mf, op="/" )
//         fit <- tableOp_cpp( fit, adj, op="*" )

//         mf  <-tableMargin_cpp(fit, glist[[2]])
//         adj <- tableOp_cpp( md2, mf, op="/" )
//         fit <- tableOp_cpp( fit, adj, op="*" )

//         mf  <-tableMargin_cpp(fit, glist[[3]])
//         adj <- tableOp_cpp( md3, mf, op="/" )
//         fit <- tableOp_cpp( fit, adj, op="*" )

//     }
//     sum( (fit-dat)^2 / fit)
// }

// myips(lizard, glist)
// loglin(lizard, glist)

// microbenchmark(myips(lizard, glist), loglin(lizard, glist,print=FALSE))




// a1 <- gRbase::parray(c("a","b","c"), c(3,2,2), 0:8)
// a2 <- gRbase::parray(c("b","c","e"), c(2,2,3), 0:8)

// w1 <- tableOp(a1, a2, "*")
// w2 <- tableOp_cpp(a1, a2, "*")

// ww <- lapply(alignTables_cpp(w1,w2), as.data.frame.table)
// names(ww[[2]])[5] <- "Freq2"
// merge(ww$a1, ww$a2)


// #tableMargin_cpp(a1, c("a","b"))
// #gRbase::tableMargin(a1, c("a","b"))


// library(microbenchmark)
// microbenchmark( tableOp(a1,a2), tableOp_cpp(a1,a2))

// microbenchmark( tableMargin(a1, c("a","b")), tableMargin_cpp(a1, c("a","b")) )


// #a1 <- gRbase::parray(c("a","b","c"), c(3,2,2), 0:8)
// #a2 <- gRbase::parray(c("b","c"), c(2,2), 0:4)



// inline NumericVector rowSums(NumericMatrix tmp){
//   int NR=tmp.nrow(), NC=tmp.ncol();
//   NumericVector out = no_init(NR);
//   double sum=0;
//   for (int i=0; i<NR; ++i){
//     sum=0;
//     for (int j=0; j<NC; ++j){
//       sum+=tmp(i,j);
//     }
//     out[i] = sum;
//   }
//   return out;
// }




// //[[Rcpp::export]]
// List extendTables0_cpp(NumericVector a1, NumericVector a2)
// {
//   typedef CharacterVector charVec;
//   typedef IntegerVector   intVec;
//   typedef NumericVector   numVec;

//   int i, j, k;
//   int l1  = a1.size(), l2 = a2.size();

//   List a1info = get_table_info( a1 );
//   intVec  di1 = a1info["di"];
//   List    dn1 = a1info["dn"];
//   charVec vn1 = a1info["vn"];

//   List a2info = get_table_info( a2 );
//   intVec  di2 = a2info["di"];
//   List    dn2 = a2info["dn"];
//   charVec vn2 = a2info["vn"];

//   charVec isec = intersect( vn1, vn2 );

//   charVec r1     = setdiff( vn2, isec );
//   intVec  idx_r1 = match( r1, vn2 );
//   intVec  di_r1  = do_subsetter_idx_<INTSXP>(di2, idx_r1);

//   charVec r2     = setdiff( vn1, isec );
//   intVec  idx_r2 = match( r2, vn1 );
//   intVec di_r2   = do_subsetter_idx_<INTSXP>(di1, idx_r2);

//   // intVec idx_sec = match( isec, vn1 );
//   // intVec di_sec  = do_subsetter_idx_<INTSXP>( di1, idx_sec );

//   int pdi_r1 = prod( di_r1 );
//   int pdi_r2 = prod( di_r2 );

//   numVec a1ext = no_init( l1 * pdi_r1 ), a2ext = no_init( l2 * pdi_r2 );

//   for (k=0, i=0; i<l1; ++i){
//     for (j=0; j<pdi_r1; ++j) a1ext[k++] = a1[ i ];
//   }
//   for (k=0, i=0; i<l2; ++i){
//     for (j=0; j<pdi_r2; ++j) a2ext[k++] = a2[ i ];
//   }

//   intVec  di1ext  = do_concat_<INTSXP>( di_r1, di1 );
//   intVec  di2ext  = do_concat_<INTSXP>( di_r2, di2 );
//   charVec vn1ext  = do_concat_<STRSXP>( r1, vn1 );
//   charVec vn2ext  = do_concat_<STRSXP>( r2, vn2 );

//   List dn1ext( vn1ext.length() ), dn2ext( vn1ext.length() );
//   for (i=0; i<idx_r1.size(); i++)
//     dn1ext[i] = dn2[idx_r1[i]-1];

//   for (j=i, i=0; i<dn1.size(); i++, j++)
//     dn1ext[j] = dn1[i];

//   for (i=0; i<idx_r2.size(); i++)
//     dn2ext[i] = dn1[idx_r2[i]-1];

//   for (j=i, i=0; i<dn2.size(); i++, j++)
//     dn2ext[j] = dn2[i];

//   a1ext = set_table_info( a1ext, di1ext, dn1ext, vn1ext);
//   a2ext = set_table_info( a2ext, di2ext, dn2ext, vn2ext);

//   return List::create(Named("a1")=a1ext, Named("a2")=a2ext);
// }


// //[[Rcpp::export]]
// NumericVector _tableMargin_cpp( NumericVector a1, CharacterVector marg )
// {
//   typedef CharacterVector charVec;
//   typedef IntegerVector   intVec;
//   typedef NumericVector   numVec;

//   List a1info = get_table_info( a1 );
//   charVec vn1 = a1info["vn"];
//   charVec r1  = setdiff( vn1, marg );
//   charVec vn2 = do_concat_<STRSXP>( r1, marg );
//   intVec  pp  = match( vn2, vn1 );
//   a1 = arrayPerm0_cpp(a1, pp);

//   a1info = get_table_info( a1 );
//   intVec di1 = a1info["di"];
//   List   dn1 = a1info["dn"];
//   vn1 = a1info["vn"];

//   intVec idx_marg=match( marg, vn1 ), idx_r1=match( r1, vn1 );
//   intVec di_marg = do_subsetter_idx_<INTSXP>( di1, idx_marg );
//   intVec di_r1   = do_subsetter_idx_<INTSXP>( di1, idx_r1 );

//   int NC=prod( di_marg ), NR=prod( di_r1 );
//   NumericMatrix tmp( NR, NC );
//   std::copy(a1.begin(), a1.end(), tmp.begin());
//   List marginfo = marg_table_info( a1info, idx_marg );
//   numVec out=colSums_( tmp );

//   out = set_table_info( out, marginfo["di"], marginfo["dn"], marginfo["vn"]);
//   return(out);
// }


// // [[Rcpp::export]]
// SEXP concat_( SEXP& XX_, SEXP& YY_){
//   int type = TYPEOF(XX_) ;
//   switch( type ){
//   case INTSXP  : return do_concat_<INTSXP> ( XX_, YY_ ) ;
//   case REALSXP : return do_concat_<REALSXP>( XX_, YY_ ) ;
//   case STRSXP  : return do_concat_<STRSXP> ( XX_, YY_ ) ;
//   case VECSXP  : return do_concat_<VECSXP> ( XX_, YY_ ) ;
//   }
//   return R_NilValue ;
// }




// //[[Rcpp::export(arrayMargin)]]
// NumericVector arrayMargin_cpp( NumericVector a1, CharacterVector marg )
// {
//   List a1info = get_array_info( a1 );
//   CharacterVector vn1 = a1info["vn"];
//   CharacterVector r1  = setdiff( vn1, marg );
//   CharacterVector vn2 = do_concat_( r1, marg );
//   IntegerVector   pp  = match( vn2, vn1 ); //vn2 only used here
//   IntegerVector   di1 = a1info["di"];
//   //Rprintf("a1info:\n"); Rf_PrintValue( a1info );

//   //Rf_PrintValue(pp);
//   NumericVector a1p = arrayPerm0_cpp(a1, pp);

//   IntegerVector idx_marg(marg.length());
//   IntegerVector idx_r1(r1.length());

//   std::copy( pp.begin(), pp.begin() + r1.length(), idx_r1.begin() );
//   std::copy( pp.begin() + r1.length(), pp.end(), idx_marg.begin() );

//   IntegerVector di_r1   = do_subsetter_idx_( di1, idx_r1 );
//   IntegerVector di_marg = do_subsetter_idx_( di1, idx_marg );

//   int NC=prod( di_marg ), NR=prod( di_r1 );
//   NumericMatrix tmp ( NR, NC );
//   std::copy(a1p.begin(), a1p.end(), tmp.begin());
//   //Rf_PrintValue(tmp);
//   List marginfo = marg_array_info( a1info, idx_marg ); //  Rf_PrintValue( marginfo );
//   NumericVector out=colSums_( tmp );
//   //Rf_PrintValue(out);
//   return set_array_info( out, marginfo["di"], marginfo["dn"], marginfo["vn"]);
// }
