#include <RcppEigen.h>
#include <Rcpp.h>

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

// sparse matrix
RcppExport SEXP C_mcsMAT_sp ( SEXP XX_, SEXP OO_, SEXP dd_, SEXP ddd_ ){
/*
  act / pas : booleans telling whether a given vertex is active/passive. 
  There is a clearly a redundancy here.
  nbr : short for neighbour
*/

using namespace Eigen;
using namespace Rcpp;
using namespace std;
using Eigen::Map;

typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::SparseVector<double> SpVec;
typedef SpVec::InnerIterator InIter;
SpMat   X(as<SpMat>(XX_));
int nrX(X.rows());
int count=1;

Eigen::VectorXi O(as<Eigen::VectorXi>(OO_));
Eigen::VectorXi res(nrX);

// debugging info
int dd(as<int>(dd_)), ddd(as<int>(ddd_));

Eigen::VectorXi n_pas_nbr(nrX);
Eigen::VectorXd pas(nrX), act(nrX);
SpVec pas_s(nrX), act_s(nrX), vec1_s(nrX), vec2_s(nrX);
SpVec pas_nbr_s(nrX);

pas.setZero(); 
act.setOnes(); 
pas_s = pas.sparseView();
act_s = act.sparseView();
n_pas_nbr.setZero();

int ii_mark, max_pas, n_nbr_req, n_nbr_obs, npasnbr, is_perfect=1;

if(dd)Rcout << "*INITIALIZATION" << endl;

ii_mark = O[0];
pas[ii_mark] = 1;
act[ii_mark] = 0;
pas_s = pas.sparseView();
act_s = act.sparseView();

if(dd)Rcout << " ** ii_mark=" << ii_mark << endl;
if(ddd)Rcout << "   pas  : " << pas.transpose() << endl;
if(ddd)Rcout << "   act  : " << act.transpose() << endl;

res[0] = ii_mark;

vec1_s = X.col(ii_mark);
for (InIter itjj(vec1_s); itjj; ++itjj){
  vec2_s    = X.col(itjj.index());            // Rcout << "vec2_s " << vec2_s.transpose() << endl;
  pas_nbr_s = vec2_s.cwiseProduct(pas_s); // Rcout << "pas_nbr_s " << pas_nbr_s.transpose() << endl;
  n_pas_nbr[itjj.index()] = pas_nbr_s.sum();
 }

if(nrX>1){
  if(dd)Rcout << "*ITERATION" << endl;
  while(count<nrX){
    if(ddd)Rcout << "   n_pas_nbr: " << n_pas_nbr.transpose() << endl;
    max_pas=-1;
    for (InIter jj_(act_s); jj_; ++jj_){
      if (n_pas_nbr[jj_.index()]>max_pas){
	ii_mark = jj_.index();
	max_pas = n_pas_nbr[jj_.index()];
      }
    }
    if(dd)Rcout << " **ii_mark=" << ii_mark << " max_pas=" << max_pas << endl;
    //Rcout << "count=" << count << endl;
    if ((n_pas_nbr[O[count]]==max_pas) & (act[O[count]]!=0)){
      if (ddd)Rcout << "   setting ii_mark to 'follow order'"<< endl;
      ii_mark=O[count];
    }
    if(dd)Rcout << " **ii_mark=" << ii_mark << " max_pas=" << max_pas << endl;

    res[count] = ii_mark;
    pas[ii_mark] = 1;
    act[ii_mark] = 0;
    pas_s = pas.sparseView();
    act_s  = act.sparseView();

    if(ddd)Rcout << "   pas  : " << pas.transpose() << endl;
    if(ddd)Rcout << "   act  : " << act.transpose() << endl;

    pas_nbr_s  = X.col(ii_mark).cwiseProduct(pas_s);
    npasnbr  = pas_nbr_s.sum();
    if(ddd)Rcout << "   npasnbr="<<npasnbr << " pas_nbr_s: " << pas_nbr_s.transpose();
    
    //Rcout << "pas_nbr_s " << pas_nbr_s.transpose() << endl;
    n_nbr_obs = 0; 
    n_nbr_req = 0;
    for (InIter it2(pas_nbr_s); it2; ++it2){
      for (InIter it3(pas_nbr_s); it3; ++it3){
	n_nbr_req++;
	n_nbr_obs += X.coeff(it2.index(), it3.index());
      }
    }
    if (npasnbr==0)
      n_nbr_req = 0;
    else
      n_nbr_req = npasnbr*(npasnbr-1)/2;
    n_nbr_obs /= 2;
    
    if(ddd)Rcout << "   n_nbr_req=" << n_nbr_req << " n_nbr_obs=" << n_nbr_obs << endl;

    if (n_nbr_req != n_nbr_obs){
      is_perfect=0;
      if(dd)Rcout << "   not perfect" << endl;
      break;
    }
    vec1_s = X.col(ii_mark);
    for (InIter itjj(vec1_s); itjj; ++itjj){
      vec2_s    = X.col(itjj.index());            // Rcout << "vec2_s " << vec2_s.transpose() << endl;
      pas_nbr_s = vec2_s.cwiseProduct(pas_s); // Rcout << "pas_nbr_s " << pas_nbr_s.transpose() << endl;
      n_pas_nbr[itjj.index()] = pas_nbr_s.sum();
    }
    count++;
  }
 }

if(dd)Rcout << "*FINALIZE" << endl;

if (is_perfect==0)
  res[0]=-1;
return(wrap(res));




}
