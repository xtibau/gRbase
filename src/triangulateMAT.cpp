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
RcppExport SEXP C_triangulateMAT_sp ( SEXP XX_, SEXP LL_, SEXP dd_, SEXP ddd_ ){
/*
Triangulation of undirected graph with k vertices.

1) Start with all vertices marked as active (coded as 1); set i:=k

2) While there are still active vertices {
   - Select an active vertex v that optimizes some criterion c(v)
   - Label v with the number i; i.e. set v_i = v
   - For the set C_i consisting of v_i and its active neigbours.
   - Fill in edges where none exist between all pairs of vertices in C_i
   - Mark v_i as non-active (coded as 0) and set i:=i-1
}
*/

using namespace Rcpp;
using namespace std;
using Eigen::Map;

typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::SparseVector<double> SpVec;

// For sparse matrices:
SpMat   X(as<SpMat>(XX_));
// For dense matrices:
//MapMati X(Rcpp::as<MapMati>(XX_));

Eigen::VectorXd L(as<Eigen::VectorXd>(LL_));
//int dd=1, ddd=0; // debugging info
int dd(as<int>(dd_));
int ddd(as<int>(ddd_));

int ii, n_act_nbr, ii_mark, n_nbr_need, n_nbr_obs, eli=0, nrX(X.rows()), n_active(X.rows());
double spsize, min_spsize, max_size;

Eigen::VectorXi active(nrX), act_nbr(nrX), eli_vec(nrX), n_act_nbr_vec(nrX);
Eigen::VectorXd spsize_vec(nrX);

SpVec active_s(nrX), active2_s(nrX);
SpVec act_nbr_s(X.cols()), act_nbr2_s(X.cols());
SpMat fill(X.rows(), X.cols());

typedef Eigen::Triplet<double> T;
std::vector<T> triplets;
triplets.reserve(X.nonZeros() * 2);
//triplets.reserve(X.rows()*X.cols());

active.setOnes();
active_s = active.sparseView();
max_size = L.sum();
if(dd)Rcout << "* Initialization\n";
if(ddd)Rcout << "   active   :  " << active.transpose() << endl;
if(ddd)Rcout << "   active_s :  " << active_s.transpose();
if(ddd)Rcout << "   max_size :  " << max_size << endl;

if(dd)Rcout << "* Find clique sizes \n";
for (ii=0; ii<nrX; ii++){ 
  spsize = L[ii];
  act_nbr_s  = X.col(ii).cwiseProduct(active_s);
  n_act_nbr  = act_nbr_s.sum();
  n_act_nbr_vec[ii] = n_act_nbr;

  for (SpVec::InnerIterator inner2_jj(act_nbr_s); inner2_jj; ++inner2_jj)
    spsize += L[inner2_jj.index()];

  spsize_vec[ii] = spsize;
 }

if(ddd)Rcout << "   spsize_vec    : " << spsize_vec.transpose() << endl;
if(ddd)Rcout << "   n_act_nbr_vec : " << n_act_nbr_vec.transpose() << endl;

if(dd)Rcout << "* Iteration \n";
while (n_active>0){
  //Rcout << active << "\n";
  min_spsize = max_size;
  ii_mark = 0;
  
  for (SpVec::InnerIterator it1(active_s); it1; ++it1){
    if (spsize_vec[it1.index()]<min_spsize){
      min_spsize = spsize_vec[it1.index()];
      ii_mark    = it1.index();
    }
  }  
  if(dd)Rcout << "  Node=" << ii_mark << " Size=" << min_spsize << " Remaining=" << n_active << "\n";

  eli_vec[eli++] = ii_mark;
  act_nbr_s      = X.col(ii_mark).cwiseProduct(active_s);  
  n_act_nbr      = act_nbr_s.sum();
  
  if(ddd)Rcout << "   Active      : " << active_s.transpose() ;
  if(ddd)Rcout << "   Active nbr's: " << act_nbr_s.transpose();
  if(ddd)Rcout << "   Col         : " << X.col(ii_mark).transpose() ;

  if (n_act_nbr <= 1){ // No fill-in is necessary
    if(ddd) Rcout << "   node=" << ii_mark << ": At most one active nb; we are done\n";
  } else { 
    if(ddd)Rcout << "   node=" << ii_mark << ": More than one nbr; check if fill-in is necessary\n";
    n_nbr_obs=0;
    for (SpVec::InnerIterator it2(act_nbr_s); it2; ++it2){
      for (SpVec::InnerIterator it3(act_nbr_s); it3; ++it3){
	n_nbr_obs += X.coeff(it2.index(), it3.index());
	if (it2.index()!=it3.index()){
	  triplets.push_back(T(it2.index(), it3.index(), 1));
	  triplets.push_back(T(it3.index(), it2.index(), 1));
	}
      }
    }
    fill.setFromTriplets(triplets.begin(), triplets.end());

    n_nbr_need = (int) fill.sum()/2;
    if(ddd)Rcout << "   node=" << ii_mark << ": n_act_nbr=" <<n_act_nbr<< " n_nbr_obs="<<n_nbr_obs<< " n_nbr_need=" << n_nbr_need <<  "\n";
    triplets.clear();

    if( n_nbr_need == n_nbr_obs)
      { 
	if(ddd)Rcout << "   node=" << ii_mark << ": Active boundary is complete; we are done\n";
      } 
    else 
      { 
	if(ddd)Rcout << "   node=" << ii_mark << ": A fill in is needed\n";
	X += fill;
	for (ii=0; ii<nrX; ii++){ 
	  for (SpMat::InnerIterator inner_jj(X, ii); inner_jj; ++inner_jj){
	    X.coeffRef(inner_jj.row(),inner_jj.col())=1;
	  }
	}
      }
  }

  // decrease nbr-weights (L)
  // act_nbr_s = X.col(ii_mark).cwiseProduct(active_s);
  for (SpVec::InnerIterator inner3_jj(act_nbr_s); inner3_jj; ++inner3_jj){    
    act_nbr2_s = X.col(inner3_jj.index()).cwiseProduct(active_s);
    spsize     = L[inner3_jj.index()];
    for (SpVec::InnerIterator inner4_jj(act_nbr2_s); inner4_jj; ++inner4_jj)
      spsize += L[inner4_jj.index()];

    spsize_vec[inner3_jj.index()] = spsize;

  }
  if(ddd)Rcout << "   spsize_vec (updated): " << spsize_vec.transpose() << endl;

  active[ii_mark] = 0;
  active_s        = active.sparseView();
  n_active--;
 }


X.makeCompressed();
//return(wrap(X));

S4    Xin(XX_);
S4    Xout(wrap(X));
Xout.slot("Dimnames") = clone(List(Xin.slot("Dimnames")));
return(Xout);
}
