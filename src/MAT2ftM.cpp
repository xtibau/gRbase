/*
  MAT2ftM and SYMMAT2ftM : Coerces dense and sparse matrices to from-to-matrix 

  is_sym_MAT : Checks for symmetry
*/
  

#include <RcppEigen.h>
#include <math.h>       /* fabs */

//[[Rcpp::depends(RcppEigen)]]

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::MappedSparseMatrix<double> MSpMat;

template <typename TT>
SEXP do_MAT2ftM ( SEXP XX_ ){
  using Eigen::MatrixXd;
  const TT X(as<TT>(XX_));

  int ii, jj, kk=0, sum=0;
  int nrX(X.rows()), ncX(X.cols());
  for (ii=0; ii<nrX; ii++){
    for (jj=00; jj<ncX; jj++){
      if (X.coeff(ii,jj))
	sum++;
    }
  }

  NumericMatrix out(sum,2);
  for (ii=0; ii<nrX; ii++){
    for (jj=0; jj<ncX; jj++){
      if(X.coeff(ii,jj)){
	out(kk,0) = ii+1;
	out(kk,1) = jj+1;
	kk++;
      }
    }
  }
  return out;  
}



//[[Rcpp::export]]
SEXP MAT2ftM ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_MAT2ftM<MapMati>(XX_); // matrix - integer 
  case REALSXP : return do_MAT2ftM<MapMatd>(XX_); // matrix - double
  case S4SXP   : return do_MAT2ftM<MSpMat>(XX_);  // dgCMatrix
  }
  return R_NilValue ;
}


template <typename TT>
SEXP do_SYMMAT2ftM ( SEXP XX_ ){
  using Eigen::MatrixXd;
  const TT X(as<TT>(XX_));

  int ii, jj, kk=0, sum=0;
  int nrX(X.rows()), ncX(X.cols());
  for (ii=0; ii<nrX-1; ii++){
    for (jj=ii+1; jj<ncX; jj++){
      if (X.coeff(ii,jj))
	sum++;
    }
  }

  NumericMatrix out(sum,2);
  for (ii=0;ii<nrX-1;ii++){
    for (jj=ii+1;jj<ncX;jj++){
      if(X.coeff(ii,jj)){
	out(kk,0) = ii+1;
	out(kk,1) = jj+1;
	kk++;
      }
    }
  }
  return out;  
}



//[[Rcpp::export]]
SEXP SYMMAT2ftM ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_SYMMAT2ftM<MapMati>(XX_); // matrix - integer 
  case REALSXP : return do_SYMMAT2ftM<MapMatd>(XX_); // matrix - double
  case S4SXP   : return do_SYMMAT2ftM<MSpMat>(XX_);  // dgCMatrix
  }
  return R_NilValue ;
}


template <typename TT>
SEXP do_is_sym_MAT ( SEXP XX_ ){
  using Eigen::MatrixXd;
  const TT X(as<TT>(XX_));

  int nrX(X.rows()), ncX(X.cols());
  double dif = 0, dd;
  for (int ii=0; ii<nrX-1; ii++){
    for (int jj=ii+1; jj<ncX; jj++){
      dd = X.coeff(ii,jj)-X.coeff(jj,ii);
      //Rf_PrintValue(wrap(dd));
	
      dif += fabs( dd );
      //Rf_PrintValue(wrap(dif));
    }
  }
  LogicalVector out(1);
  //Rf_PrintValue(out);
  //Rf_PrintValue(wrap(dif));
  if (dif>1e-6)
    out[0]=0;
  else
    out[0]=1;
    
  return out;  
}



//[[Rcpp::export]]
SEXP is_sym_MAT ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_is_sym_MAT<MapMati>(XX_); // matrix - integer 
  case REALSXP : return do_is_sym_MAT<MapMatd>(XX_); // matrix - double
  case S4SXP   : return do_is_sym_MAT<MSpMat>(XX_);  // dgCMatrix
  }
  return R_NilValue ;
}



/*** R


amat<-structure(c(0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 
1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "f", "g"), c("a", "b", "c", "d", 
    "e", "f", "g")))

#matrix2ftM(amat)

amat2 <- structure(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "g", "f"), c("a", "b", "c", "d", 
    "e", "g", "f")))

amat3<-structure(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "g", "f"), c("a", "b", "c", "d", 
    "e", "g", "f")))

#MAT2ftM(as(amat,"dgCMatrix"))
#MAT2ftM(amat)
#storage.mode(amat)<-"integer"
#MAT2ftM(amat)

amat <- structure(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0,
0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "g", "f"), c("a", "b", "c", "d",
    "e", "g", "f")))

mm<-MAT2ftM(amat)

vn<-c("a", "b", "c", "d", "e", "g", "f")
mm2<-vn[mm]
dim(mm2) <- dim(mm)
mm2

em<-structure(c("a", "a", "b", "c", "e", "a", "f", "b", "c", "c",
"d", "d", "e", "g"), .Dim = c(7L, 2L))
em


is_sym_MAT(amat)


library(microbenchmark)
microbenchmark(
isSymmetric(amat),
is_sym_MAT(amat)   )

library(Matrix)
amat <- as(amat, "dgCMatrix")

library(microbenchmark)
microbenchmark(
isSymmetric(amat),
is_sym_MAT(amat)   )



*/


// //[[Rcpp::export]]
// SEXP dgCMatrix2ftM ( SEXP XX_ ){
//   using namespace Rcpp;
//   using Eigen::MatrixXd;
//   //   Only difference is here:
//   typedef Eigen::MappedSparseMatrix<double> MSpMat;
//   const MSpMat X(as<MSpMat>(XX_));
//   //   end !!!
//   int ii, jj, kk=0, sum=0;
//   int nrX(X.rows());
//   int ncX(X.cols());
//   for (ii=0;ii<nrX-1;ii++){
//     for (jj=ii+1;jj<ncX;jj++){
//       if (X.coeff(ii,jj) || X.coeff(jj,ii))
// 	sum++;
//     }
//   }
//   MatrixXd ans(sum,2);
//   for (ii=0;ii<nrX-1;ii++){
//     for (jj=ii+1;jj<ncX;jj++){
//       if(X.coeff(ii,jj) || X.coeff(jj,ii)){
// 	ans(kk,0) = ii+1;
// 	ans(kk++,1) = jj+1;
//       }
//     }
//   }
//   return(wrap(ans));
// }

// //[[Rcpp::export]]
// SEXP matrix2ftM ( SEXP XX_ ){
//   using namespace Rcpp;
//   using Eigen::MatrixXd;
//   //   Only difference is here:

//   const MapMatd X(as<MapMatd>(XX_));
//   //   end !!!
//   int ii, jj, kk=0, sum=0;
//   int nrX(X.rows());
//   int ncX(X.cols());
//   for (ii=0;ii<nrX-1;ii++){
//     for (jj=ii+1;jj<ncX;jj++){
//       if (X.coeff(ii,jj) || X.coeff(jj,ii))
// 	sum++;
//     }
//   }
//   MatrixXd ans(sum,2);
//   for (ii=0;ii<nrX-1;ii++){
//     for (jj=ii+1;jj<ncX;jj++){
//       if(X.coeff(ii,jj) || X.coeff(jj,ii)){
// 	ans(kk,0)   = ii+1;
// 	ans(kk++,1) = jj+1;
//       }
//     }
//   }

//   return wrap(ans);  
// }
