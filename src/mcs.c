#include <string.h>
#include <stdlib.h>
#include <Rdefines.h>
#include "shd_print.h"

// Check if S is a complete set in the symmetrical adj. matrix A
//
void C_isCompleteSet(int *Amat, int *ncA, int *S, int *lenS, int *ans){

  //printmati(Amat, ncA, ncA);
  //printveci(S, lenS);
  int ii,jj;
  *ans = 1;
  for (ii=0;ii<(*lenS-1);ii++){
    //Rprintf("ii : %i S[ii] : %i \n", ii, S[ii]);
    for (jj=ii+1; jj<*lenS;jj++){
      //Rprintf("  jj : %i S[jj] : %i\n", jj, S[jj]);
      if (Amat[ (int) (S[ii] + *ncA * S[jj])] == 0){
 	*ans = 0; 
 	break; 
      }
    } 
  }
} 
  

void C_mcs(int *Avec, int *nvar,  int *root, int *ans){
  int ii, kk;
  int nnb[*nvar];
  int print=0;
  int UU[*nvar], LL[*nvar];  // UU: unlabelled nodes; LL: labelled nodes
  int index[*nvar];
  int *nbp, nb[*nvar], nbcount;
  int ann=0; // already numbered neighbours
  int isPerfect=1, isComplete;
  int next=0, next2=0;
  int followRoot = 1;

  //Rprintf("root :"); printveci(root, nvar);

  // Initialization
  for (ii=0;ii<*nvar;ii++){
    UU[ii]   = 1;
    LL[ii]   = 0;
    ans[ii]  = -1;
    index[ii] = -1;
    nnb[ii]  = 0;
  }

  // First variable to be labelled
  next = root[0];
  //Rprintf("next (start): %i\n", next);

  // Set ordering
  UU[next]  = 0;
  LL[next]  = 1;
  ans[next] = 0;
  index[0]   = next;
  
  // Update vector with already numbered neighbours  
  nbp = Avec + next * *nvar;
  for (ii=0;ii<*nvar;ii++){
    nnb[ii] = nnb[ii] + nbp[ii];
  }

  // Rprintf("------------------------------------------------\n");

  for (kk=1; kk<*nvar; kk++){
    if (print){
      Rprintf("UU:                   ");   printveci(UU, nvar);  
      Rprintf("LL:                   ");   printveci(LL, nvar); 
      Rprintf("nnb:                  ");   printveci(nnb, nvar);
      Rprintf("ans:                  ");   printveci(ans, nvar);
    }
    // Next node to go into the labelling (as given by root)
    if (followRoot==1){
      next2 = next = root[kk];
      ann   = nnb[kk];
    } else {
      ann = -1;
    }
    
    // Find unlabelled node with maximal number of numbered labelled neighbours
    // Notice: The desired node is changed only if a better one is found
    // next=next2;
    for (ii=0;ii<*nvar;ii++){
      if ((UU[ii]==1) & (nnb[ii]>ann)){
	if (print) Rprintf("ii: %i\n", ii);
	ann  = nnb[ii];
	next = ii;
      }
    }
    
    if (next2 != next)
      followRoot=0;

    if (print==1){
      Rprintf("next (desired) %i nnb %i \n", next2, nnb[next2]);
      Rprintf("next (found)   %i nnb %i \n", next,  nnb[next]);
    }

    LL[next]  = 1;
    UU[next]  = 0;
    ans[next] = kk;
    index[kk]  = next;   

    // Neighbours of the next node 
    nbp = Avec + next * *nvar;
    if (print){Rprintf("nb's of next node   ::"); printveci(nbp, nvar);}

    // Find those neighbours of next node which are LL
    nbcount = 0;
    for (ii=0;ii<*nvar;ii++){
      if ((nbp[ii]==1) & (LL[ii]==1)){
	nb[nbcount++] = ii;
      }
    }

    // Is the set of already numbered neighbours complete?
    //Rprintf("nbcount %i :: ", nbcount); printveci(nb, &nbcount);
    C_isCompleteSet(Avec, nvar, nb, &nbcount, &isComplete);
    if (print) Rprintf("isComplete %i\n", isComplete);
    if (!isComplete){
      isPerfect = 0;
      break;
    }

    // Update nnb
    for (ii=0;ii<*nvar;ii++){
      nnb[ii] = nnb[ii] + nbp[ii];
    }
  }  

  if (print){
    Rprintf("------------------------------------------------\n");
    Rprintf("ans   (at the end)   ::"); printveci(ans, nvar);
    Rprintf("index (at the end)   ::"); printveci(index, nvar);
  }

  if (isPerfect){
    for (ii=0;ii<*nvar;ii++)
      ans[ii] = index[ii];
  } else {
    ans[0] = -1;
  }


}
















