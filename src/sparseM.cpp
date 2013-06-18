# include <RcppEigen.h>
# include <Rcpp.h>

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

RcppExport SEXP C_getXij ( SEXP XX_, SEXP ii_, SEXP jj_){
  using namespace Rcpp;
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  const MSpMat X(as<MSpMat>(XX_));
  int i = as<int>(ii_)-1;
  int j = as<int>(jj_)-1;
  double ans = X.coeff(i,j);
  return(wrap(ans));
}

RcppExport SEXP C_setXij1 ( SEXP XX_, SEXP ii_, SEXP jj_){
  using namespace Rcpp;
  typedef Eigen::SparseMatrix<double> MSpMat;
  MSpMat   X(as<MSpMat>(XX_));
  int ii = as<int>(ii_)-1;
  int jj = as<int>(jj_)-1;
  X.coeffRef(ii,jj) = 1;
  X.makeCompressed();
  return(wrap(X));
}

RcppExport SEXP C_getXtf ( SEXP XX_, SEXP TF_){
  using Eigen::Map;
  using Eigen::MatrixXi;
  using namespace Rcpp;
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  MSpMat   X(as<MSpMat>(XX_));
  const Map<MatrixXi> TF(as<Map<MatrixXi> >(TF_));
  int nrTF(TF.rows());
  Eigen::VectorXi ans(nrTF);
  int rr, ii, jj;
  for (rr=0; rr<nrTF; rr++){
    jj = (TF.coeff(rr,0))-1;
    ii = (TF.coeff(rr,1))-1;
    ans[rr] = X.coeff(ii,jj);
  }
  return(wrap(ans));
}

RcppExport SEXP C_getXM ( SEXP XX_, SEXP MM_){
  using Eigen::Map;
  using Eigen::MatrixXi;
  using namespace Rcpp;
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  MSpMat   X(as<MSpMat>(XX_));
  const Map<MatrixXi> M(as<Map<MatrixXi> >(MM_));
  int nrM(M.rows());
  Eigen::VectorXi ans(nrM);
  int rr, ii, jj;
  for (rr=0; rr<nrM; rr++){
    jj = (M.coeff(rr,1))-1;
    ii = (M.coeff(rr,0))-1;
    ans[rr] = X.coeff(ii,jj);
  }
  return(wrap(ans));
}

RcppExport SEXP C_setXtf1 ( SEXP XX_, SEXP TF_){
  using Eigen::Map;
  using Eigen::MatrixXi;
  using namespace Rcpp;
  typedef Eigen::SparseMatrix<double> MSpMat;
  MSpMat   X(as<MSpMat>(XX_));
  const Map<MatrixXi> TF(as<Map<MatrixXi> >(TF_));
  int nrTF(TF.rows());
  int rr, ii, jj;
  for (rr=0; rr<nrTF; rr++){
    jj = (TF.coeff(rr,0))-1;
    ii = (TF.coeff(rr,1))-1;
    X.coeffRef(ii,jj) = 1;
  }
  X.makeCompressed();
  return(wrap(X));
}


RcppExport SEXP C_setXM1 ( SEXP XX_, SEXP MM_){
  using Eigen::Map;
  using Eigen::MatrixXi;
  using namespace Rcpp;
  typedef Eigen::SparseMatrix<double> MSpMat;
  MSpMat   X(as<MSpMat>(XX_));
  const Map<MatrixXi> M(as<Map<MatrixXi> >(MM_));
  int nrM(M.rows());
  int rr, ii, jj;
  for (rr=0; rr<nrM; rr++){
    jj = (M.coeff(rr,1))-1;
    ii = (M.coeff(rr,0))-1;
    X.coeffRef(ii,jj) = 1;
  }
  X.makeCompressed();
  return(wrap(X));
}

RcppExport SEXP C_getXi ( SEXP XX_, SEXP ii_){
  using Eigen::Map;
  using Eigen::MatrixXi;
  using namespace Rcpp;
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  MSpMat   X(as<MSpMat>(XX_));
  int ncX(X.cols());
  Eigen::VectorXi ans(ncX);
  int jj;
  int ii = as<int>(ii_)-1;
  for (jj=0; jj<ncX; jj++){
    ans[jj] = X.coeff(ii,jj);
  }
  return(wrap(ans));
}


RcppExport SEXP C_getXj ( SEXP XX_, SEXP jj_){
  using Eigen::Map;
  using Eigen::MatrixXi;
  using namespace Rcpp;
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  MSpMat   X(as<MSpMat>(XX_));
  int nrX(X.rows());
  Eigen::VectorXi ans(nrX);
  int ii;
  int jj = as<int>(jj_)-1;
  for (ii=0; ii<nrX; ii++){
    ans[ii] = X.coeff(ii,jj);
  }
  return(wrap(ans));
}

// RcppExport SEXP C_moralizeM ( SEXP XX_){
//   using Eigen::Map;
//   using Eigen::MatrixXi;
//   using namespace Rcpp;
//   typedef Eigen::MappedSparseMatrix<double> MSpMat;
//   typedef Eigen::SparseMatrix<double> SpMat;
//   MSpMat   X(as<MSpMat>(XX_));
//   SpMat   X2(X.rows(), X.cols());
//   SpMat   TMP(X.rows(), X.cols());
//   SpMat   ANS(X.rows(), X.cols());
//   X2 = X;
//   int nrX(X.rows());
//   Eigen::VectorXi bb(nrX);
//   int kk, ll, jj;
//   for (jj=0;jj<nrX;jj++){
//     bb = X.col(jj);
//     for (kk=0;kk<nrX;kk++){
//       if (X.coeff(kk,jj) != 0){
// 	for (ll=kk+1;ll<nrX; ll++){
// 	  if (X.coeff(ll,jj) != 0){
// 	    if (X2.coeff(kk,ll)==0){
// 	      X2.insert(kk,ll) = 1;
// 	      X2.insert(ll,kk) = 1;
// 	    }
// 	  }
// 	}
//       }
//     }
//   }
//   TMP = (X2.adjoint() + X.adjoint())/2 ;
//   ANS = TMP + (X2 + X)/2;
//   ANS.makeCompressed();
//   return(wrap(ANS));
// }



RcppExport SEXP C_moralizeM ( SEXP XX_){
  using Eigen::Map;
  using namespace Rcpp;
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  typedef Eigen::SparseMatrix<double> SpMat;
  SpMat   X(as<MSpMat>(XX_));
  
  typedef Eigen::Triplet<double> T;
  std::vector<T> triplets;
  triplets.reserve(X.nonZeros() * 2);
  
  int nrX(X.rows());
  int kk, ll, vv;
  for (vv=0; vv<nrX; vv++){ /* consider vertex vv */
    for (kk=0; kk<nrX; kk++){
      if (X.coeff(kk, vv) != 0){     /* yes, kk->vv */
	for (ll=kk+1; ll<nrX; ll++){
	  if (X.coeff(ll, vv) != 0){ /* yes, ll->vv */
	    if ((X.coeff(kk, ll)==0) && (X.coeff(ll, kk)==0)){ /* kk not~ ll */
	      triplets.push_back(T(kk, ll, 1));
	      triplets.push_back(T(ll, kk, 1));
	    }
	  }
	}
      }
    }
  }
  
  SpMat ans(X.rows(), X.cols());
  ans.setFromTriplets(triplets.begin(), triplets.end());
  SpMat Xt(X.transpose());
  ans = ans + Xt + X;
  
  for (kk=0; kk<nrX; kk++){
    for (ll=kk+1; ll<nrX; ll++){
      if (ans.coeff(kk,ll)!=0){
	ans.coeffRef(kk,ll)=1;
	ans.coeffRef(ll,kk)=1;
      }
    }
  }
  ans.makeCompressed();
  return(wrap(ans));
}

RcppExport SEXP C_topoSortM ( SEXP XX_){
  using Eigen::Map;
  using Eigen::MatrixXi;
  using namespace Rcpp;
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  typedef Eigen::SparseMatrix<double> SpMat;
  MSpMat   X(as<MSpMat>(XX_));
  int ii, jj, kk=0, count=0, ll=0, flagsum=0;
  int ncX(X.rows());
  Eigen::VectorXi indegree(ncX);
  Eigen::VectorXi flag(ncX);
  Eigen::VectorXi ans(ncX);
  
  for (ii = 0; ii < ncX; ii++) {
    indegree[ii] = 0; flag[ii] = 0; ans[ii] = 0;
  }
  for (jj = 0; jj < ncX; jj++)
    for (ii = 0; ii < ncX; ii++)
      indegree[jj] = indegree[jj] +  X.coeff(ii,jj); 
  
  //   Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;
  //   Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;
  while (count < ncX){
    // Rcout << "count=" << count << std::endl;
    for (kk = 0; kk < ncX; kk++){
      // Rcout <<" kk="<<kk<<" indeg="<<indegree[kk]<<" flag="<<flag[kk] << std::endl;
      if ((indegree[kk] == 0) && (flag[kk] == 0)){
	//Rcout << "   no incomming:" << kk << std::endl;
	ans[ll++] = kk+1;
	flag[kk]  = 1;
	flagsum++;
	for (jj = 0; jj < ncX; jj++){
	  /*  Rcout <<"kk,jj="<<kk<<","<<jj<<" entry=" << X.coeff(kk,jj) << std::endl;*/
	  if (X.coeff(kk,jj) == 1){
	    indegree[jj]--;
	    // Rcout <<" updating indegree at entry="<<jj<<std::endl;
	  }
	}
      }      
      // Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;	
    }
    if (flagsum==ncX)
      break;
    count++;
    // Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;	
  }  
  if (flagsum<ncX)
    ans[0] = -1;
  return(wrap(ans));
}

RcppExport SEXP C_fromtoM ( SEXP XX_ ){
  using namespace Rcpp;
  using Eigen::MatrixXi;
  //   Only difference is here:
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  const MSpMat X(as<MSpMat>(XX_));
  //   end !!!
  int ii, jj, kk=0, sum=0;
  int nrX(X.rows());
  int ncX(X.cols());
  for (ii=0;ii<nrX;ii++){
    for (jj=0;jj<ncX;jj++){
      if (X.coeff(ii,jj))
	sum++;
    }
  }
  MatrixXi ans(sum,2);
  for (ii=0;ii<nrX;ii++){
    for (jj=0;jj<ncX;jj++){
      if(X.coeff(ii,jj)){
	ans(kk,0) = ii+1;
	ans(kk++,1) = jj+1;
      }
    }
  }
  return(wrap(ans));
}

RcppExport SEXP C_fromtoS ( SEXP XX_ ){
  using namespace Rcpp;
  using Eigen::MatrixXi;
  //   Only difference is here:
  typedef Eigen::Map<MatrixXi> MapMati;  
  const MapMati X(as<MapMati>(XX_));
  //   end !!!
  int ii, jj, kk=0, sum=0;
  int nrX(X.rows());
  int ncX(X.cols());
  for (ii=0;ii<nrX;ii++){
    for (jj=0;jj<ncX;jj++){
      if (X.coeff(ii,jj))
	sum++;
    }
  }
  MatrixXi ans(sum,2);
  for (ii=0;ii<nrX;ii++){
    for (jj=0;jj<ncX;jj++){
      if(X.coeff(ii,jj)){
	ans(kk,0) = ii+1;
	ans(kk++,1) = jj+1;
      }
    }
  }
  return(wrap(ans));
}
