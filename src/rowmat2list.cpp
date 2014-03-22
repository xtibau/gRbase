#include <Rcpp.h>

using namespace Rcpp;

template <typename TT>
SEXP do_rowmat2list( SEXP XX_ ){
  TT X(XX_);
  int nr=X.nrow();
  List out(nr);
  for (int ii=0; ii<nr; ii++){
    out(ii) = X(ii,_);
  }
  return(out);
}

template <typename TT>
SEXP do_colmat2list( SEXP XX_ ){
  TT X(XX_);
  int nc=X.ncol();
  List out(nc);
  for (int ii=0; ii<nc; ii++){
    out(ii) =  X( _, ii);
  }
  return(out);
}

// [[Rcpp::export]]
SEXP rowmat2list ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_rowmat2list<IntegerMatrix>(XX_); 
  case REALSXP : return do_rowmat2list<NumericMatrix>(XX_); 
  case STRSXP  : return do_rowmat2list<CharacterMatrix>(XX_); 
  }                                    
  return List(0);
}

// [[Rcpp::export]]
List do_colmat2list_str(SEXP XX_)
{
  CharacterMatrix X(XX_);
  int nc=X.ncol(), nr=X.nrow(), k;
  List out(nc);
  for (int j=0; j<nc; j++){
    CharacterVector v(nr);
    for (k=0; k<nr; ++k){
      v[ k ] = X( k, j);
    }
    out(j) =  v ; 
  }
  return out;
}


// [[Rcpp::export]]
SEXP colmat2list ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_colmat2list<IntegerMatrix>(XX_); 
  case REALSXP : return do_colmat2list<NumericMatrix>(XX_); 
    //case STRSXP  : return do_colmat2list<CharacterMatrix>(XX_); 
  case STRSXP  : return do_colmat2list_str(XX_); // FIXME bug in Rcpp???
  }                                    
  return List(0) ;
}



// [[Rcpp::export]]
SEXP which_matrix_index( SEXP XX_ ){
  NumericMatrix X(XX_);
  double sum=0;
  for(int ii=0; ii<X.nrow(); ii++){
    for (int jj=0; jj<X.ncol(); jj++){
      sum += (X(ii,jj)!=0);
    }
  }
  
  int kk=0;
  NumericMatrix out(sum, 2);
  for(int ii=0; ii<X.nrow(); ii++){
    for (int jj=0; jj<X.ncol(); jj++){
      if (X(ii,jj)!=0){
	out(kk,0)=ii+1; 
	out(kk++,1)=jj+1;
      }
    }
  }

  return wrap(out);
}


/*** R

m1 <- matrix(1:16,4)
m2 <- matrix(letters[1:16], 4)

m2 <- matrix(as.character(1:10000), 100)


imat <- matrix(1:90, nrow=10)
dmat <- 1.0*imat
cmat <- matrix(as.character(1:90), nrow=10)

ilist <- rowmat2list(imat); #sapply(ilist, class)
dlist <- rowmat2list(dmat); #sapply(dlist, class)
clist <- rowmat2list(cmat); #sapply(clist, class)

#library(microbenchmark)
#microbenchmark(gRbase::rowmat2list(imat), rowmat2list(imat))
#microbenchmark(gRbase::colmat2list(imat), colmat2list(imat))
#microbenchmark(gRbase::rowmat2list(cmat), rowmat2list(cmat))
#microbenchmark(gRbase::colmat2list(cmat), colmat2list(cmat))

 */


// m0 <-matrix(NA, nrow=0, ncol=2)
// #rowmat2list(m0); colmat2list(m0)

// m0 <- matrix(c(NA,NA,2,3),nrow=2)
// #rowmat2list(m0); colmat2list(m0)

// m <- matrix(c(1,0,1,0,0,1,1,1,1e-16),nr=3)
// gRbase::which.arr.ind(m)

// which_matrix_index(m)
