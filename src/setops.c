#include <string.h>
#include <Rdefines.h>

/* 
  Returns 0 if x is subset of y and 1 otherwise.
     - x and y are defined through the indices (sx,ex) and (sy,ey) in aaa
     - Indexing follows R-standard, i.e. starts from 1
*/


void subsetof1(char **aaa, int *sx, int *ex, int *sy, int *ey, int *ans){
/*   Rprintf("    sx %i ex %i sy %i ey %i\n", *sx, *ex, *sy, *ey ); */
  int ii, jj, nmatch=0, nx;
  
  nx   = *ex-*sx+1;
  *ans = 0;

/*   Rprintf("x: %i", nx); */
/*   for (ii=*sx-1; ii<*ex; ii++){ */
/*     Rprintf(" %s", aaa[ii]); */
/*   } */
/*   Rprintf("\n"); */
  
/*   Rprintf(" y: "); */
/*   for (jj=*sy-1; jj<*ey; jj++){ */
/*     Rprintf(" %s", aaa[jj]); */
/*   } */
/*   Rprintf("\n"); */
  
  for (ii=*sx-1; ii<*ex; ii++){
    /* Rprintf("    ii %i %s\n", ii, aaa[ii]);  */
    for (jj=*sy-1; jj<*ey; jj++){
      /* Rprintf("      jj %i %s\n", jj, aaa[jj]);  */
      if (!strcmp(aaa[ii],aaa[jj])){
	nmatch++;
	/* 	Rprintf("  match...%i %i %s %s %i\n", ii, jj, aaa[ii], aaa[jj], nmatch);   */
	break;
      }      
    }
    if (nmatch == nx){
      *ans = 1;
      break;
    }
  }
/*    Rprintf(" ans:  %i \n", *ans);   */
}

/*
  Returns vector with entries 0 meaning that the elements are
  contained in other elements while 1 means that they are maximal
*/
void maxset(char **setlist, int *ends, int *nset, int *keepvec){
  
  int ii, jj;
  int ans;

  int starts[*nset];
  starts[0]=1;
  keepvec[0]=1;
  for (ii=1; ii<*nset; ii++){ 
    starts[ii] = ends[ii-1]+1;
    keepvec[ii] = 1;
  }

  for (ii=0; ii<*nset-1; ii++){ 
    if (keepvec[ii]==1){
      /* Rprintf("SET 1: %i %i %i\n", ii, starts[ii], ends[ii]);    */
      for (jj=ii+1; jj<*nset; jj++){
	if (keepvec[jj]==1){
	  /* Rprintf("  SET 2: %i %i %i\n", jj, starts[jj], ends[jj]);   */
	  subsetof1(setlist, &starts[jj], &ends[jj], &starts[ii], &ends[ii], &ans); 
	  /* Rprintf("  ans: %i \n", ans);    */
	  if (ans==1)
	    keepvec[jj] = 0;
	  else
	    keepvec[jj] = 1;
	}
      }
    }
  }
}


void minset(char **setlist, int *ends, int *nset, int *keepvec){
  
  int ii, jj;
  int ans;

  int starts[*nset];
  starts[0]=1;
  keepvec[0]=1;
  for (ii=1; ii<*nset; ii++){ 
    starts[ii] = ends[ii-1]+1;
    keepvec[ii] = 1;
  }

  for (ii=0; ii<*nset-1; ii++){ 
    if (keepvec[ii]==1){
/*       Rprintf("SET 1: no: %i start: %i end: %i\n", ii, starts[ii], ends[ii]);     */
      for (jj=ii+1; jj<*nset; jj++){
	if (keepvec[jj]==1){
/* 	  Rprintf("  SET 2: %i %i %i\n", jj, starts[jj], ends[jj]);    */
	  subsetof1(setlist, &starts[ii], &ends[ii], &starts[jj], &ends[jj],  &ans); 
/* 	  Rprintf("  ans: %i \n", ans);     */
	  if (ans==1)
	    keepvec[jj] = 0;
	  else
	    keepvec[jj] = 1;
	}
      }
    }
  }
}



/* ************************************************************ */
/* ************************************************************ */
/* ************************************************************ */
/* ************************************************************ */

void subsetof2(char **eee, int *ne, 
	       char **aaa, int *sx, int *ex, int *ans){
/*   Rprintf("    sx %i ex %i sy %i ey %i\n", *sx, *ex, *sy, *ey ); */
  int ii, jj, nmatch=0, nx;
  
  nx   = *ex-*sx+1;
  *ans = 0;

  
/*   Rprintf(" e: "); */
/*   for (jj=0; jj<*ne; jj++){ */
/*     Rprintf(" %s", eee[jj]); */
/*   } */
/*   Rprintf("\n"); */

/*   Rprintf("x: %i", nx); */
/*   for (ii=*sx-1; ii<*ex; ii++){ */
/*     Rprintf(" %s", aaa[ii]); */
/*   } */
/*   Rprintf("\n"); */
  
  for (ii=0; ii<*ne; ii++){
    /* Rprintf("    ii %i %s\n", ii, aaa[ii]);   */
    for (jj=*sx-1; jj<*ex; jj++){
      /* Rprintf("      jj %i %s\n", jj, aaa[jj]);   */
      if (!strcmp(eee[ii],aaa[jj])){
	nmatch++;
	/* Rprintf("  match...%i %i %s %s %i\n", ii, jj, aaa[ii], aaa[jj], nmatch);   */
	break;
      }      
    }
    if (nmatch == *ne){
      *ans = 1;
      break;
    }
  }
  /* Rprintf(" ans:  %i \n", *ans);    */
}



void isin(char **set, int *ne, char **setlist, int *ends, int *nset, int *keepvec){
  
  int ii, ans;

  int starts[*nset];
  starts[0]=1;
  keepvec[0]=1;
  for (ii=1; ii<*nset; ii++){ 
    starts[ii] = ends[ii-1]+1;
    keepvec[ii] = 1;
  }

  for (ii=0; ii<*nset; ii++){ 
    subsetof2(set, ne, setlist, &starts[ii], &ends[ii], &ans); 
    keepvec[ii] = ans;
  }
}


/* ************************************************************ */
/* ************************************************************ */
/* ************************************************************ */
/* ************************************************************ */


/* void subsetof3(char **eee, int *ne,  */
/* 	       char **aaa, int *na, int *ans){ */
/* /\*   Rprintf("    sx %i ex %i sy %i ey %i\n", *sx, *ex, *sy, *ey ); *\/ */
/*   int ii, jj, nmatch=0; */
  
/*   *ans = 0; */

  
/* /\*   Rprintf(" e: "); *\/ */
/* /\*   for (jj=0; jj<*ne; jj++){ *\/ */
/* /\*     Rprintf(" %s", eee[jj]); *\/ */
/* /\*   } *\/ */
/* /\*   Rprintf("\n"); *\/ */

/* /\*   Rprintf("x: %i", nx); *\/ */
/* /\*   for (ii=*sx-1; ii<*ex; ii++){ *\/ */
/* /\*     Rprintf(" %s", aaa[ii]); *\/ */
/* /\*   } *\/ */
/* /\*   Rprintf("\n"); *\/ */
  
/*   for (ii=0; ii<*ne; ii++){ */
/*     /\* Rprintf("    ii %i %s\n", ii, aaa[ii]);   *\/ */
/*     for (jj=0; jj<*na; jj++){ */
/*       /\* Rprintf("      jj %i %s\n", jj, aaa[jj]);   *\/ */
/*       if (!strcmp(eee[ii],aaa[jj])){ */
/* 	nmatch++; */
/* 	/\* Rprintf("  match...%i %i %s %s %i\n", ii, jj, aaa[ii], aaa[jj], nmatch);   *\/ */
/* 	break; */
/*       }       */
/*     } */
/*     if (nmatch == *ne){ */
/*       *ans = 1; */
/*       break; */
/*     } */
/*   } */
/*   /\* Rprintf(" ans:  %i \n", *ans);    *\/ */
/* } */

















