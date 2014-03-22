## -----------------------------------------------------------
## cell2entry 
## -----------------------------------------------------------

entry2cell <- function(entry, adim, plev=cumprod(adim)/adim){
  cell <- rep(NA, length(adim))
  rrr <- entry-1
  for (ii in length(adim):1){
    cell[ii] <- rrr %/% plev[ii]
    rrr <- rrr %% plev[ii]
  }
  cell + 1 
}

cell2entry <- function(cell, adim){
  ans <- .C("C_cell2entry",  as.integer(cell), as.integer(adim),
            length(cell), integer(1)
            ,PACKAGE="gRbase"
            )[[4]]
  return(ans)
}

.cell2entry2 <- function(cell, adim){
  plevels <- cumprod(adim)/adim[1]
  ans <- .C("C_cell2entry2",  as.integer(cell), as.integer(plevels),
            length(cell), integer(1)
            ,PACKAGE="gRbase"
            )[[4]]
  return(ans)
}

.cell2entryR <- function(cell, adim, plev=cumprod(adim)/adim){
  1 + sum((cell-1) * plev)
}

## -----------------------------------------------------------
## nextCell
## -----------------------------------------------------------

## returns next table entry. eg if cell=(1,2,1) then the
## value is (2,2,1)
nextCell <- function(cell, adim){
  ans <- .C("C_nextCell",  as.integer(cell), as.integer(adim), length(cell)
            ,PACKAGE="gRbase"
            )[[1]]
  if (ans[1]<0)
    return(NULL)
  ans
}

.nextCellR <- function(cell, adim){
  jj <- (seq_along(adim)[cell<adim])[1]
  if (!is.na(jj)){
    ans <- cell # Kan bare overskrive cell!!
    if (jj>1){
      ans[1:(jj-1)] <- 1
    }
    ans[jj] <- ans[jj] + 1
    return(ans)
  } else {
    return(NULL)
  }
}

## -----------------------------------------------------------
## nextCellSlice
## -----------------------------------------------------------

nextCellSlice <- function(cell, sliceset, adim){
  ans <- .C("C_nextCellSlice",  as.integer(cell), as.integer(adim), length(cell),
            as.integer(sliceset), length(sliceset)
            ,PACKAGE="gRbase"
            )[[1]]
  if (ans[1]<0)
    return(NULL)
  ans
}

.nextCellSliceR <- function(cell, sliceset, adim){
  ans <- cell
  zzz <- nextCell(cell[-sliceset], adim[-sliceset])
  if (!is.null(zzz)){
    ans[-sliceset] <- zzz
    ##ans[sliceset] <- slicecell
    return(ans)
  } else {
    NULL
  }
}

.nextCellSliceIndic <- function(cell, marg.indic, adim){
  ans <- .C("C_nextCellSlice_indic",  as.integer(cell), as.integer(adim), length(cell),
            as.integer(marg.indic), as.integer(sum(marg.indic))
            ,PACKAGE="gRbase"
            )[[1]]
  if (ans[1]<0)
    return(NULL)
  ans
}


## -----------------------------------------------------------
## slice2entry
## -----------------------------------------------------------

slice2entry <- function(slicecell, sliceset, adim){
  .Call("R_slice2entry", slicecell, sliceset, adim
        ,PACKAGE="gRbase"
        )
}

.slice2entry <- function(slicecell, sliceset, adim){
  ans_len       <- prod(adim[-sliceset])
  ans           <- rep.int(0L, ans_len)
  cell          <- rep.int(1L,length(adim))
  cell[sliceset] <- as.integer(slicecell)

  ##cat(sprintf("R: ans_len=%d\n", ans_len))
  res <- .C("C_slice2entry",
            cell     = cell,
            sliceset  = as.integer(sliceset),
            adim  = as.integer(adim), 
	    cell_len = length(cell),
            marg_len = length(slicecell), 
	    ans      = ans,
            ans_len  = as.integer(ans_len)
            ,PACKAGE="gRbase"
            )[[6]]
  return(res)
}

permuteCellEntries <- function(perm, adim){
  return(.Call("R_permuteCellEntries", perm, adim
               ,PACKAGE="gRbase"
               ))
}

.permuteCellEntriesR <- function(perm, adim){

  pvec         <- as.integer(cumprod(adim)/adim[1])
  adim.new  <- adim[perm]
  entry.new    <- rep.int(0, prod(adim))

  cell = as.integer(rep(1, length(adim)))
  for (ii in 1:(length(entry.new))){
    entry.new[ii]  <- .getCellNumberR(cell, perm, pvec=pvec)
    cell           <- .nextCellR(cell, adim.new)
  }
  entry.new
}

.permuteCellEntriesC <- function(perm, adim){

  adim   <- as.integer(adim)
  entry.new <- rep.int(0L, prod(adim))
  ans <- .C("C_permuteCellEntries", perm, adim, length(adim),
            entry.new, length(entry.new)
            ,PACKAGE="gRbase"
            )[[4]]
  ans
}



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
## 'adim': the levels of the factors in the original table.
## 'perm': match(c("B","A","C"), c("A","B","C")) -> 2,1,3
## 'cell': (1,2,2) is a cell in the original table
## Output: The entry of the cell in the new table


getCellNumberC <- function(cell, perm, adim, pvec=cumprod(adim)/adim){
  .C("C_getCellNumber", as.integer(cell), as.integer(perm), as.integer(pvec),
     length(cell), integer(1), DUP=FALSE
     ,PACKAGE="gRbase"
     )[[5]]
}

.getCellNumberR <- function(cell, perm, adim, pvec=cumprod(adim)/adim){
  sum(pvec*(cell[perm]-1)) + 1
}


## -----------------------------------------------------------
## factgrid
## -----------------------------------------------------------

factGrid <- function(adim, slicecell=NULL, sliceset=NULL){
  if (is.null(slicecell)){
    .factgrid1Prim(adim)
  } else {
    .factgrid2Prim(adim, slicecell, sliceset)
  }
}

.factgrid1Prim <- function( adim ){

  nr <- prod(adim)
  nc <- length(adim)
  mm <- matrix(NA, nrow=nr, ncol=nc)
  
  cell    <- rep(1, nc)
                                        #print(cell)
  mm[1,]  <- cell
  if (nr>1)
    for (ii in 2:nr){
      cell <- nextCell(cell, adim)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}

.factgrid2Prim <- function(adim , slicecell, sliceset){

  nr <- prod(adim[-sliceset])
  nc <- length(adim)
  mm <- matrix(NA, nrow=nr, ncol=nc)

  cell    <- rep(1, nc)
  cell[sliceset] <- slicecell
                                        #print(cell)
  mm[1,]  <- cell
  if (nr>1)
    for (ii in 2:nr){
      cell <- nextCellSlice(cell, sliceset, adim)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}




















