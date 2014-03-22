#include <Rcpp.h>
using namespace Rcpp;

IntegerVector make_indic(int ndim, IntegerVector slice){
  IntegerVector indic( ndim );
  for (int i=0;i<slice.length();i++){
    indic[ slice[ i ] - 1 ] = 1;
  }
  return indic;
}

IntegerVector make_prod( int ndim, IntegerVector adim ){
  IntegerVector plevels( ndim );
  plevels[0] = 1;
  for (int ii=1; ii<ndim; ii++){
    plevels[ii] =  adim[ii-1] * plevels[ii-1];
  }
  return plevels;
}

//[[Rcpp::export]]
int cell2entry_cpp(NumericVector cell, IntegerVector adim){
  int ndim=adim.length();
  int ii, ss=1, res=cell[0]-1;

  for (ii=1;ii<ndim;ii++){
    ss  *=  adim[ii-1];
    res += (cell[ii]-1) * ss;
  }
  return res + 1;
}

//[[Rcpp::export]]
int cell2entry2_cpp(NumericVector cell, IntegerVector plevels){
  int ndim=cell.length(), ans=0;
  for (int ii=0; ii<ndim; ii++){
    ans += (cell[ii] -1) * plevels[ii];
  }
  return ans + 1;
}

//[[Rcpp::export]]
NumericVector nextCell_cpp(NumericVector cell, IntegerVector adim){
  int ndim=adim.length(), jj, n_init=0;
  for (jj=0; jj<ndim; jj++){
    if (cell[jj] < adim[jj]){
      cell[jj] = cell[jj] + 1;
      break;
    } else {
      cell[jj] = 1;
      n_init++;
    }
  }
  return cell;
}

//[[Rcpp::export]]
NumericVector nextCellSlicePrim_cpp(NumericVector cell, IntegerVector adim, IntegerVector sliceIndic){
  int ndim = cell.length();
  int sum=0, n_init=0;
  for (int jj=0; jj<ndim; jj++){
    sum += sliceIndic[jj];
    if (sliceIndic[jj]==0){
      if (cell[jj] < adim[jj]){
	cell[jj] = cell[jj] + 1;
	break;
      } else {
	cell[jj] = 1;
	n_init++;
      }
    }
  }
  if (n_init == (ndim - sum)){
    cell[0] = -1;
  }
  return cell;
}

//[[Rcpp::export]]
NumericVector nextCellSlice_cpp(NumericVector cell, IntegerVector adim, IntegerVector sliceSet){
  IntegerVector sliceIndic = make_indic( adim.length(), sliceSet);
  return nextCellSlicePrim_cpp(cell, adim, sliceIndic);
}

IntegerVector slice2entryPrim_cpp(IntegerVector sliceCell, IntegerVector sliceSet,
				  IntegerVector adim,
				  IntegerVector sliceIndic, IntegerVector plevels
				  ){
  int ii, ndim=adim.length(), entry, ans_len=1;
  NumericVector cell( ndim );
  // Create initial cell
  for (ii=0; ii<ndim; ii++)
    cell[ii] = 1;
  for (ii=0; ii<sliceCell.length(); ii++)
    cell[sliceSet[ii]-1] = sliceCell[ii];

  IntegerVector tmp( ndim );
  tmp = adim;
  for (ii=0;ii<sliceSet.length();ii++)
    tmp[sliceSet[ii]-1] = 1;

  for (ii=0;ii<ndim;ii++){
    ans_len *= tmp[ ii ];
  }

  IntegerVector ans( ans_len );

  for (ii=0; ii<ans_len; ii++){
    entry = cell2entry2_cpp( cell, plevels );
    ans[ ii ] = entry;
    cell  = nextCellSlicePrim_cpp( cell, adim, sliceIndic );
  }

  return ans;
}

//[[Rcpp::export]]
IntegerVector slice2entry_cpp(IntegerVector sliceCell, IntegerVector sliceSet, IntegerVector adim){
  IntegerVector sliceIndic = make_indic( adim.length(), sliceSet);
  IntegerVector plevels    = make_prod( adim.length(), adim);

  return slice2entryPrim_cpp(sliceCell, sliceSet, adim, sliceIndic, plevels);
}


//[[Rcpp::export]]
int getCellNumberPrim_cpp(NumericVector cell, IntegerVector perm, IntegerVector pvec){
  int cell_number = 0, ndim=cell.length();
  for (int ii=0; ii<ndim; ii++){
    cell_number +=  (pvec[perm[ii]-1] * (cell[ii] - 1));
  }
  return cell_number + 1;
}

//[[Rcpp::export]]
int getCellNumber_cpp(NumericVector cell, IntegerVector adim, IntegerVector perm){
  IntegerVector pvec=make_prod( adim.length(), adim );
  return getCellNumberPrim_cpp( cell, perm, pvec );
}

//[[Rcpp::export]]
IntegerVector permuteCellEntries_cpp(IntegerVector perm, IntegerVector adim){

  int ndim=adim.length(), ii;
  NumericVector cell( ndim );
  for (ii=0; ii<ndim; ii++) cell[ii] = 1;
  int ans=0, len_entry=1;

  IntegerVector adim_new( ndim );
  for (ii=0; ii<ndim;ii++) {
    len_entry *= adim[ ii ];
    adim_new[ii] = adim[perm[ii]-1];
  }

  IntegerVector pvec = make_prod( ndim, adim);
  IntegerVector entry_new( len_entry );
  for (ii=0; ii<len_entry; ii++){
    //Rf_PrintValue( cell );
    ans = getCellNumberPrim_cpp(cell, perm, pvec);
    entry_new[ii] = ans;
    nextCell_cpp(cell, adim_new);
  }
  return entry_new;
}







/*** R

library(gRbase)
## 1-dimensional array
x1 <- 1:8; dim(x1) <- 8; x1
c(is.array(x1), is.matrix(x1))

## 2-dimensional array (matrix)
x2 <- 1:8; dim(x2) <- c(2,4); x2
c(is.array(x2), is.matrix(x2))

## 3-dimensional array
x3 <- array(1:8, dim=c(2,2,2)); x3
c(is.array(x3), is.matrix(x3))

adim2222 <- c(2,2,2,2)
adim2323 <- c(2,3,2,3)

entry2cell(1, adim2222)
entry2cell(6, adim2222)

cell2entry(c(1,1,1,1), adim2222)
cell2entry_cpp(c(1,1,1,1), adim2222)
cell2entry(c(2,1,2,1), adim2222)
cell2entry_cpp(c(2,1,2,1), adim2222)

#library(microbenchmark)
#microbenchmark(
#cell2entry(c(1,1,1,1), adim2222),
#cell2entry_cpp(c(1,1,1,1), adim2222),
#cell2entry(c(2,1,2,1), adim2222),
#cell2entry_cpp(c(2,1,2,1), adim2222)
#)

nextCell(c(1,1,2,1), adim2222)
nextCell(c(2,2,2,1), adim2222)
nextCell_cpp(c(1,1,2,1), adim2222)
nextCell_cpp(c(2,2,2,1), adim2222)

x<-c(1,1,2,1)
nextCell_cpp(x, adim2222)
x ## notice: x has changed!!!


nextCellSlice(c(2,1,1,2),  sliceset=c(2), adim2323)
nextCellSlice_cpp(c(2,1,1,2),  sliceSet=c(2), adim2323)
nextCellSlice(c(1,3,2,1),  sliceset=c(2,3), adim2323)
nextCellSlice_cpp(c(1,3,2,1),  sliceSet=c(2,3), adim2323)

x<-c(1,3,2,1)
nextCellSlice_cpp(x,  sliceSet=c(2,3), adim2323)
x ## notice: x has changed

#library(microbenchmark)
#microbenchmark(
#nextCellSlice(c(2,1,1,2),  sliceset=c(2), adim2323),
#nextCellSlice_cpp(c(2,1,1,2),  sliceSet=c(2), adim2323),
#nextCellSlice(c(1,3,2,1),  sliceset=c(2,3), adim2323),
#nextCellSlice_cpp(c(1,3,2,1),  sliceSet=c(2,3), adim2323)
#)


(r1<-slice2entry(slicecell=c(1,2), sliceset=c(2,3), adim2222))
(r2<-slice2entry_cpp(sliceCell=c(1,2), sliceSet=c(2,3), adim2222))


x  <- HairEyeColor
ii <- seq_along(x)
dim(ii) <- dim(x)
pp <- c(2,3,1)
as.integer(aperm(ii, pp))
permuteCellEntries_cpp(pp, dim(x))

permuteCellEntries_cpp(c(2,1), c(2,3))

as.integer(aperm(ii, pp))
permuteCellEntries_cpp(pp, c(4,4,2))

library(microbenchmark)
microbenchmark(as.integer(aperm(ii, pp)),permuteCellEntries_cpp(pp, c(4,4,2)))

*/


// NumericVector cello(ndim);
  // for (int ii=0; ii<ndim; ii++){
  //   cello[perm[ii]-1]=cell[ii];
  //   //cell_number +=  (pvec[ii] * (cell[perm[ii]-1] - 1));
  // }
  // for (int ii=0; ii<ndim; ii++){
  //   cell_number +=  (pvec[ii] * (cello[ii] - 1));
  // }

  //Rf_PrintValue( cello );
