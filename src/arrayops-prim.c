/* **************************************************************
 C code for operations on table entries
 Used in the gRim package
 Søren Højsgaard
** *************************************************************/

#include <string.h>
#include <stdlib.h>
#include <Rdefines.h>
#include "_utils_print.h"

/* ************************************************************* */

void C_cell2entry(int *cell, int *flevels, int *cell_len, int *ans);
void C_cell2entry2(int *cell, int *plevels, int *cell_len, int *ans);

void C_nextCell(int *cell, int *flevels, int *cell_len);
SEXP R_nextCell(SEXP Cell, SEXP Flevels);

void C_nextCellMarg_indic(int *cell, int *flevels, int *cell_len, 
			  int *marg_indic, int *marg_len); 

void C_nextCellMarg(int *cell, int *flevels, int *cell_len, 
		    int *marg_set, int *marg_len);


void C_margcell2entryPrim(int *cell, int *plevels, int *flevels, 
			  int *cell_len, int *marg_indic, int *marg_len, 
			  int *ans, int *ans_len);

void C_margcell2entry(int *cell, int *marg_set, int *flevels, 
		      int *cell_len, int *marg_len, 
		      int *ans, int *ans_len);

SEXP R_margcell2entry(SEXP Margcell, SEXP Margset, SEXP Flevels);

int* make_marg_indic(int *cell_len, int *marg_set, int *marg_len);
int* make_prod_fact(int *flevels, int *cell_len);


void C_getCellNumber(int *cell, int *perm, int *pvec, int *len, int *ans);

void C_permuteCellEntries(int *perm, int *flevels, int *tabdim, int *entry_new, int *len_entry);
SEXP R_permuteCellEntries(SEXP Perm, SEXP Flevels);

SEXP R_aperm2(SEXP Perm, SEXP Flevels, SEXP Flevels_new);


/* ************************************************************* */

void C_cell2entry(int *cell, int *flevels, int *cell_len, int *ans)
{
  int ii, ss=1, res=cell[0]-1;
  for (ii=1;ii<*cell_len;ii++){
    ss = ss * flevels[ii-1];
    res = res + (cell[ii]-1) * ss;
  }
  *ans = res + 1;
}

void C_cell2entry2(int *cell, int *plevels, int *cell_len, int *ans)
{
  int ii;
  for (ii=0; ii<*cell_len; ii++){
    *ans = *ans + (cell[ii] -1) * plevels[ii];
  }
  *ans = *ans + 1;
}


void C_nextCell(int *cell, int *flevels, int *cell_len)
{
  int jj, n_init=0;  
  for (jj=0; jj<*cell_len; jj++){
    //Rprintf("jj %i\n", jj);  
    if (cell[jj] < flevels[jj]){
      cell[jj] = cell[jj] + 1;
      break;
    } else {
      cell[jj] = 1;
      n_init++;
    }
  }
  if (n_init == *cell_len){
    cell[0] = -1;
  }
} 


SEXP R_nextCell(SEXP Cell, SEXP Flevels)
{

  int *Cptr, *Fptr, *ansptr, cell_len;
  SEXP ans;

  PROTECT(Cell = coerceVector(Cell, INTSXP));
  PROTECT(Flevels = coerceVector(Flevels, INTSXP));

  cell_len    = length(Cell);
  Cptr  = INTEGER(Cell);
  Fptr  = INTEGER(Flevels);

  PROTECT(ans = allocVector(INTSXP, cell_len));
  ansptr = INTEGER(ans);

  C_nextCell(Cptr, Fptr, &cell_len);

  for (int ii=0;ii<cell_len;ii++)
    ansptr[ii] = Cptr[ii];

  UNPROTECT(3);
  return(ans);
}



int* make_marg_indic(int *cell_len, int *marg_set, int *marg_len)
{
  int ii, *marg_indic;
  marg_indic    = (int *) R_alloc(*cell_len, sizeof(int)); 
  for (ii=0; ii<*cell_len; ii++){
    marg_indic[ii] = 0;
  } 
  for (ii=0; ii<*marg_len; ii++){
    //Rprintf(" ii %i  marg_set[ii] %i \n", ii, marg_set[ii]);
    marg_indic[marg_set[ii]-1] = 1;
  } 
  return(marg_indic);
}

void C_nextCellMarg_indic(int *cell, int *flevels, int *cell_len, 
			  int *marg_indic, int *marg_len)
{

  int jj, n_init=0, not_marg_len=0;

  not_marg_len = *cell_len - *marg_len;

  for (jj=0; jj<*cell_len; jj++){
    if (marg_indic[jj]==0){
      //Rprintf("jj %i\n", jj);  
      if (cell[jj] < flevels[jj]){
	//Rprintf("kkkkk\n");
	cell[jj] = cell[jj] + 1;
	break;
      } else {
	cell[jj] = 1;
	n_init++;
      } 
    }
  }
  //Rprintf("n_init=%d\n", n_init);
  if (n_init == not_marg_len){
    cell[0] = -1;
  }
}

void C_nextCellMarg(int *cell, int *flevels, int *cell_len, 
		    int *marg_set, int *marg_len)
{
  int *marg_indic;
  marg_indic = make_marg_indic(cell_len, marg_set, marg_len);
  C_nextCellMarg_indic(cell, flevels, cell_len, marg_indic, marg_len);
} 

int* make_prod_fact(int *flevels, int *cell_len)
{
  int ii, *plevels;
  plevels  = (int *) R_alloc(*cell_len, sizeof(int)); 
  plevels[0]=1;
  if (*cell_len > 1){
    for (ii=1;ii<*cell_len;ii++){
      plevels[ii] = (int) (flevels[ii-1]*plevels[ii-1]);
    }
  }
  return(plevels);
}

void C_margcell2entryPrim(int *cell, int *plevels, int *flevels, 
			  int *cell_len, int *marg_indic, int *marg_len, 
			  int *ans, int *ans_len)
{

  int ii, entry;
  for (ii=0;ii<*ans_len;ii++)
    {
      entry = 0;
      C_cell2entry2(cell, plevels, cell_len, &entry); 
      //Rprintf(" ii: %i entry: %i cell:", ii, entry); printveci(cell, cell_len); Rprintf("\n");
      //C_nextCellMarg(cell, flevels, cell_len, marg_set, marg_len);
      C_nextCellMarg_indic(cell, flevels, cell_len, marg_indic, marg_len); // This is faster...
      ans[ii] = entry;
    } 
  //Rprintf("\n");
}

void C_margcell2entry(int *cell, int *marg_set, int *flevels, 
		      int *cell_len, int *marg_len, 
		      int *ans, int *ans_len)
{

  int *plevels, *marg_indic;
  marg_indic = make_marg_indic(cell_len, marg_set, marg_len);
  plevels    = make_prod_fact(flevels, cell_len);

/*   Rprintf("C_margcell2entry:\n"); */
/*   Rprintf("ans_len=%d\n", *ans_len); */
/*   printveci(marg_indic, cell_len); Rprintf("\n"); */
/*   printveci(plevels,    cell_len); Rprintf("\n"); */
/*   printveci(cell,       cell_len); Rprintf("\n"); */

  C_margcell2entryPrim(cell, plevels, flevels, cell_len, marg_indic, marg_len, ans, ans_len);
}


SEXP R_margcell2entry(SEXP Margcell, SEXP Margset, SEXP Flevels)
{
  
  int ii, *marg_cell, *marg_set, *flevels, *ansptr, *tmpptr, *cell, cell_len, marg_len, ans_len=1;
  SEXP ans, tmp ;

  PROTECT(Margcell = coerceVector(Margcell, INTSXP));
  PROTECT(Margset  = coerceVector(Margset,  INTSXP));
  PROTECT(Flevels  = coerceVector(Flevels,  INTSXP));

  cell_len   = length(Flevels);
  marg_len   = length(Margcell);
  marg_cell  = INTEGER(Margcell);
  marg_set   = INTEGER(Margset);
  flevels    = INTEGER(Flevels);

  int *plevels, *marg_indic;
  // marg_indic: [0,1,1,0,0] if entry 2,3 are fixed
  marg_indic = make_marg_indic(&cell_len, marg_set, &marg_len);
  plevels    = make_prod_fact(flevels, &cell_len);

  // Need to know dimension of result
  PROTECT(tmp = allocVector(INTSXP, cell_len));
  tmpptr = INTEGER(tmp);
  for (ii=0;ii<cell_len;ii++)
    tmpptr[ii] = flevels[ii];
  //  printveci(tmpptr, &cell_len); Rprintf("\n");
  for (ii=0;ii<marg_len;ii++)
    tmpptr[marg_set[ii]-1] = 1;
  //printveci(tmpptr, &cell_len); Rprintf("<- tmpptr\n");
  for (ii=0;ii<cell_len;ii++)
    ans_len = ans_len * tmpptr[ii];
  PROTECT(ans = allocVector(INTSXP, ans_len));
  ansptr = INTEGER(ans);
  
  // Create initial cell
  cell    = (int *) R_alloc(cell_len, sizeof(int)); 
  for (ii=0;ii<cell_len;ii++)
    cell[ii] = 1;
  for (ii=0;ii<marg_len;ii++)
    cell[marg_set[ii]-1] = marg_cell[ii];

/*   Rprintf("R_margcell2entry:\n"); */
/*   Rprintf("ans_len=%d\n", ans_len); */
/*   printveci(marg_indic, &cell_len); Rprintf("\n"); */
/*   printveci(plevels, &cell_len); Rprintf("\n"); */
/*   printveci(cell, &cell_len); Rprintf("\n"); */

  //  printveci(ansptr, &ans_len);
  C_margcell2entryPrim(cell, plevels, flevels, &cell_len, marg_indic, &marg_len, ansptr, &ans_len);
  // printveci(ansptr, &ans_len);

  UNPROTECT(5);
  return(ans);
}






void C_permuteCellEntries(int *perm, int *flevels, int *tabdim, int *entry_new, int *len_entry)
{ 

  int *cell, *pvec, *nlev_new, ii, ans=0;

  cell  = (int *) R_alloc(*tabdim, sizeof(int));
  for (ii=0; ii<*tabdim; ii++)
    cell[ii] = 1;

  pvec  = (int *) R_alloc(*tabdim, sizeof(int));
  pvec[0] = 1;
  if (*tabdim>1){
    for (ii=1; ii<*tabdim; ii++)
      pvec[ii] = (int) pvec[ii-1]*flevels[ii];
  }

  nlev_new  = (int *) R_alloc(*tabdim, sizeof(int));
  for (ii=0; ii<*tabdim;ii++)
    nlev_new[ii] = flevels[perm[ii]-1];

  for (ii=0; ii<*len_entry; ii++){
    C_getCellNumber(cell, perm, pvec, tabdim, &ans);
    //Rprintf("ans %i\n", ans);
    entry_new[ii] = ans;
    C_nextCell(cell, nlev_new, tabdim);
  }
  // printveci(entry_new, len_entry);
}


SEXP R_permuteCellEntries(SEXP Perm, SEXP Flevels)
{

  int tabdim, *ansptr, *Perm_ptr, *Flevels_ptr, ii, n_entries=1;
  SEXP ans;

  PROTECT(Perm = coerceVector(Perm, INTSXP));
  PROTECT(Flevels = coerceVector(Flevels, INTSXP));

  tabdim = length(Flevels);
  Perm_ptr = INTEGER(Perm);
  Flevels_ptr = INTEGER(Flevels);

  for (ii=0; ii<tabdim; ii++){
    n_entries = n_entries * Flevels_ptr[ii];
  }

  PROTECT(ans = allocVector(INTSXP, n_entries));
  ansptr = INTEGER(ans);

  C_permuteCellEntries(Perm_ptr, Flevels_ptr, &tabdim, ansptr, &n_entries);

  UNPROTECT(3);
  return(ans);
}


SEXP R_aperm2(SEXP Perm, SEXP Flevels, SEXP Flevels_new)
{

  int tabdim, *ansptr, *Perm_ptr, *Flevels_ptr, *Flevels_new_ptr, ii, n_entries=1;
  SEXP ans;

  PROTECT(Perm = coerceVector(Perm, INTSXP));
  PROTECT(Flevels = coerceVector(Flevels, INTSXP));
  PROTECT(Flevels_new = coerceVector(Flevels_new, INTSXP));

  tabdim = length(Flevels);
  Perm_ptr = INTEGER(Perm);
  Flevels_ptr = INTEGER(Flevels);
  Flevels_new_ptr = INTEGER(Flevels_new);

  for (ii=0; ii<tabdim; ii++){
    n_entries = n_entries * Flevels_ptr[ii];
  }

  PROTECT(ans = allocVector(INTSXP, n_entries));
  ansptr = INTEGER(ans);

  C_permuteCellEntries(Perm_ptr, Flevels_ptr, &tabdim, ansptr, &n_entries);

  setAttrib(ans, install("dim"), Flevels_new);
  UNPROTECT(4);
  return(ans);
}




/* 
## getCellNumber
## -------------
## A table is defined over factors, e.g. A,B,C with levels
## |A|,|B|,|C|. First factor in the table varies fastest.
## To each cell there is an entry number 1,2,..K
## where K=|A||B||C|. (Just think of the numbers 1,2,..K
## being stored in the table.
##
## A permuted table can have factors B,A,C. The function
## returns the entry of a cell in the permuted table.
##
## Example: All factors are binary. The cell (1,2,2) has
## entry 7 in the original A-B-C-table. The B-A-C-table is
## formed by permuting the factors as 2,1,3. In the new
## the cell (1,2,2) (in the old table) has entry 6.
##
## Arguments:
## 'nlev': the levels of the factors in the original table.
## 'perm': match(c("B","A","C"), c("A","B","C")) -> 2,1,3
## 'cell': (1,2,2) is a cell in the original table
## Output: The entry of the cell in the new table
*/ 

void C_getCellNumber(int *cell, int *perm, int *pvec, int *len, int *ans)
{
  
  *ans = 0;
  for (int ii=0; ii<*len; ii++){
    //Rprintf("pvec %i perm %i cell[perm] %i \n", 
    // pvec[ii], perm[ii], cell[perm[ii]-1]);
    *ans = *ans + pvec[ii] * (cell[perm[ii]-1] - 1);
  }
  *ans = *ans+1;
}

