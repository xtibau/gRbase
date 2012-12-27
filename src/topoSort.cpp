# include <RcppEigen.h>
# include <Rcpp.h>

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

// standard matrix
RcppExport SEXP C_topoSort_st ( SEXP XX_ ){
  typedef Eigen::Map<Eigen::MatrixXi> MapMati;
  const MapMati X(Rcpp::as<MapMati>(XX_));
  //typedef Eigen::MappedSparseMatrix<double> MSpMat;
  //const MSpMat   X(as<MSpMat>(XX_));
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
  
  /*   Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;*/
  /*   Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;*/
  while (count < ncX){
    /* Rcout << "count=" << count << std::endl;*/
    for (kk = 0; kk < ncX; kk++){
      /* Rcout <<" kk="<<kk<<" indeg="<<indegree[kk]<<" flag="<<flag[kk] << std::endl;*/
      if ((indegree[kk] == 0) && (flag[kk] == 0)){
	/*Rcout << "   no incomming:" << kk << std::endl;*/
	ans[ll++] = kk+1;
	flag[kk]  = 1;
	flagsum++;
	for (jj = 0; jj < ncX; jj++){
	  /*  Rcout <<"kk,jj="<<kk<<","<<jj<<" entry=" << X.coeff(kk,jj) << std::endl;*/
	  if (X.coeff(kk,jj) == 1){
	    indegree[jj]--;
	    /* Rcout <<" updating indegree at entry="<<jj<<std::endl;*/
	  }
	}
      }      
      /* Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;	*/
    }
    if (flagsum==ncX)
      break;
    count++;
    /* Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;	*/
  }  
  if (flagsum<ncX)
    ans[0] = -1;
  return(wrap(ans));
}

// sparse matrix
RcppExport SEXP C_topoSort_sp ( SEXP XX_ ){
  //   typedef Eigen::Map<Eigen::MatrixXi> MapMati;
  //   const MapMati X(Rcpp::as<MapMati>(XX_));
  typedef Eigen::MappedSparseMatrix<double> MSpMat;
  const MSpMat   X(as<MSpMat>(XX_));
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
  
  /*   Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;*/
  /*   Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;*/
  while (count < ncX){
    /* Rcout << "count=" << count << std::endl;*/
    for (kk = 0; kk < ncX; kk++){
      /* Rcout <<" kk="<<kk<<" indeg="<<indegree[kk]<<" flag="<<flag[kk] << std::endl;*/
      if ((indegree[kk] == 0) && (flag[kk] == 0)){
	/*Rcout << "   no incomming:" << kk << std::endl;*/
	ans[ll++] = kk+1;
	flag[kk]  = 1;
	flagsum++;
	for (jj = 0; jj < ncX; jj++){
	  /*  Rcout <<"kk,jj="<<kk<<","<<jj<<" entry=" << X.coeff(kk,jj) << std::endl;*/
	  if (X.coeff(kk,jj) == 1){
	    indegree[jj]--;
	    /* Rcout <<" updating indegree at entry="<<jj<<std::endl;*/
	  }
	}
      }      
      /* Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;	*/
    }
    if (flagsum==ncX)
      break;
    count++;
    /* Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;	*/
  }  
  if (flagsum<ncX)
    ans[0] = -1;
  return(wrap(ans));
}
























// RcppExport SEXP C_topoSort ( SEXP XX_ ){
  
//   using Eigen::Map;
//   using Eigen::MatrixXi;
//   typedef Eigen::MappedSparseMatrix<double> MSpMat;
//   typedef Eigen::SparseMatrix<double> SpMat;
//   const MSpMat   X(as<MSpMat>(XX_));
//   int ii, ll=0, flagsum=0;
//   int ncX(X.rows());
//   Eigen::VectorXi indegree(ncX);
//   Eigen::VectorXi flag(ncX);
//   Eigen::VectorXi ans(ncX);
  
//   for (int jj = 0; jj < X.cols(); jj++) {
//     indegree[jj] = flag[jj] = ans[jj] = 0;
//     for (SpMat::InnerIterator it(X, jj); it; ++it) indegree[jj] += it.value();
//   }
//   Rcout<<"indegree: " << indegree.adjoint() << std::endl;
//   Rcout<<"flag    : " << flag.adjoint() << std::endl;
//   for (int count = 0; count < ncX; ++count) {
//     Rcout << "count=" << count << std::endl;
//     for (int kk = 0; kk < ncX; kk++) {
//       Rcout <<" kk="<<kk<<" indeg="<<indegree[kk]<<" flag="<<flag[kk] << std::endl;
//       if ((indegree[kk] == 0) && (flag[kk] == 0)) {
// 	Rcout << "   no incoming:" << kk << std::endl;
// 	ans[ll++] = kk+1;
// 	flag[kk]  = 1;
// 	flagsum++;
// 	for (int jj = 0; jj < ncX; jj++){
// 	  //Rcout <<"kk,jj="<<kk<<","<<jj<<" entry=" << X.coeff(kk,jj) << std::endl;
// 	  if (X.coeff(kk,jj) == 1) {
// 	    indegree[jj]--;
// 	    Rcout <<" updating indegree at entry="<<jj<<std::endl;
// 	  }
// 	}
//       }      
//       Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; 
//       Rcout << std::endl;     
//     }
//     if (flagsum == ncX)
//       break;
//     Rcout<<"flag    : " << flag.adjoint() <<" " ; Rcout << std::endl;
//   }  
//   if (flagsum < ncX)
//     ans[0] = -1;
//   return Rcpp::wrap(ans);
// }

