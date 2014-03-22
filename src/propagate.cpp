#include "arrayPerm.h"
#include "arrayOps.h"
using namespace Rcpp;

//[[Rcpp::export]]
List propagateLS_cpp(List pp_, List rip){
  List pp   = clone(pp_);
  List cliq = rip["cliques"];
  List seps = rip["separators"];
  IntegerVector pa = rip["parents"];
  List childList   = rip["childList"];
  NumericVector sp_pot, cq_pot, pa_pot, tmpd;
  CharacterVector cq, sp;
  IntegerVector ch, tmp;
  double normConst, zd;
  int i, j, ncliq = cliq.length(), nch, idx;
  for (i=ncliq-1; i>0; --i){
    //std::cout << " collect    i= " << i << std::endl;
    cq = cliq[i];   //Rprintf("cq\n");     Rf_PrintValue( cq );
    sp = seps[i];   //Rprintf("sp\n");     Rf_PrintValue( sp );
    cq_pot = pp[i]; //Rprintf("cq_pot\n"); Rf_PrintValue( cq_pot );
    if (sp.size()>=1){
      idx = pa[ i ]-1;
      pa_pot = pp[ idx ];                   //Rprintf("pa_pot\n"); Rf_PrintValue(pa_pot);
      sp_pot = arrayMargin_cpp(cq_pot, sp); //Rprintf("sp_pot\n"); Rf_PrintValue(sp_pot);
      pp[ i ]   = arrayOp2_cpp(cq_pot, sp_pot, '/');
      pp[ idx ] = arrayOp2_cpp(pa_pot, sp_pot, '*');
    } else {
      zd = sum( cq_pot );
      tmpd = pp[0]; tmpd = zd * tmpd; pp[0] = tmpd;
      tmpd = pp[i]; tmpd = tmpd / zd; pp[i] = tmpd;
    }
  }

  tmpd = pp[0]; normConst = sum( tmpd );

  for (i=0; i<ncliq; ++i){
    // std::cout << " distribute i= " << i << std::endl;
    ch  = childList[i];
    nch = ch.size();
    if (nch>0){
      for (j=0; j<nch; ++j){
	idx = ch[ j ]-1;
    	sp  = seps[ idx ];
    	if( sp.size() > 0){
    	  // Rf_PrintValue(sp);
    	  sp_pot = arrayMargin_cpp(pp[i], sp);
  	  pp[ idx ] = arrayOp2_cpp( pp[ idx ], sp_pot, '*');
    	}
      }
    }
  }

  pp.attr("pFinding")=normConst;
  return pp;
}
