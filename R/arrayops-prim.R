## -----------------------------------------------------------
## cell2entry 
## -----------------------------------------------------------

cell2entry <- function(cell, flevels)
  {
    ans <- .C("C_cell2entry",  as.integer(cell), as.integer(flevels),
              length(cell), integer(1)
              ,PACKAGE="gRbase"
              )[[4]]
    return(ans)
  }

.cell2entry2 <- function(cell, flevels)
  {
    plevels <- cumprod(flevels)/flevels[1]
    ans <- .C("C_cell2entry2",  as.integer(cell), as.integer(plevels),
              length(cell), integer(1)
              ,PACKAGE="gRbase"
              )[[4]]
    return(ans)
  }

## FIXME : plev er vist forkert???? cumprod(c(1,flevels[1:(length(flevels)-1)]))
.cell2entryR <- function(cell, flevels, plev=cumprod(flevels)/flevels[1])
  {
    1 + sum((cell-1) * plev)
  }

.cell2entryR <- function(cell, flevels, plev=cumprod(c(1,flevels[1:(length(flevels)-1)])))
  {
    1 + sum((cell-1) * plev)
  }

entry2cell <- function(entry, flevels, plev=cumprod(flevels)/flevels[1])
  {
    cell <- rep(NA, length(flevels))
    rrr <- entry-1
    for (ii in length(flevels):1){
      cell[ii] <- rrr %/% plev[ii]
      rrr <- rrr %% plev[ii]
    }
    cell + 1 
  }

## -----------------------------------------------------------
## nextCell
## -----------------------------------------------------------

## returns next table entry. eg if cell=(1,2,1) then the
## value is (2,2,1)
nextCell <- function(cell, flevels){
  ans <- .C("C_nextCell",  as.integer(cell), as.integer(flevels), length(cell)
            ,PACKAGE="gRbase"
            )[[1]]
  if (ans[1]<0)
    return(NULL)
  ans
}

.nextCellR <- function(cell, flevels){
  jj <- (seq_along(flevels)[cell<flevels])[1]

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
## nextCellMarg
## -----------------------------------------------------------

nextCellMarg <- function(cell, flevels, margset){
  ans <- .C("C_nextCellMarg",  as.integer(cell), as.integer(flevels), length(cell),
            as.integer(margset), length(margset)
            ,PACKAGE="gRbase"
            )[[1]]
  if (ans[1]<0)
    return(NULL)
  ans
}

.nextCellMargIndic <- function(cell, flevels, marg.indic){
  ans <- .C("C_nextCellMarg_indic",  as.integer(cell), as.integer(flevels), length(cell),
            as.integer(marg.indic), as.integer(sum(marg.indic))
            ,PACKAGE="gRbase"
            )[[1]]
  if (ans[1]<0)
    return(NULL)
  ans
}


.nextCellMargR <- function(cell, margset, flevels)
{
  ans <- cell
  zzz <- nextCell(cell[-margset], flevels[-margset])
  if (!is.null(zzz)){
    ans[-margset] <- zzz
    #ans[margset] <- margcell
    return(ans)
  } else {
    NULL
  }
}

## -----------------------------------------------------------
## margcell2entry
## -----------------------------------------------------------

margcell2entry <- function(margcell, margset, flevels){
  .Call("R_margcell2entry", margcell, margset, flevels
        ,PACKAGE="gRbase"
        )
}

.margcell2entry <- function(margcell, margset, flevels){
  ans_len       <- prod(flevels[-margset])
  ans           <- rep.int(0L, ans_len)
  cell          <- rep.int(1L,length(flevels))
  cell[margset] <- as.integer(margcell)

  ##cat(sprintf("R: ans_len=%d\n", ans_len))
  res <- .C("C_margcell2entry",
            cell     = cell,
            margset  = as.integer(margset),
            flevels  = as.integer(flevels), 
	    cell_len = length(cell),
            marg_len = length(margcell), 
	    ans      = ans,
            ans_len  = as.integer(ans_len)
            ,PACKAGE="gRbase"
            )[[6]]
  return(res)
}


## -----------------------------------------------------------
## factgrid
## -----------------------------------------------------------

factgrid <- function( flevels , margcell=NULL, margset=NULL){
  if (is.null(margcell)){
    .factgrid1Prim(flevels)
  } else {
    .factgrid2Prim(flevels, margcell, margset)
  }
}

.factgrid1Prim <- function( flevels ){

  nr <- prod(flevels)
  nc <- length(flevels)
  mm <- matrix(NA, nr=nr, nc=nc)
  
  cell    <- rep(1, nc)
                                        #print(cell)
  mm[1,]  <- cell
  if (nr>1)
    for (ii in 2:nr){
      cell <- nextCell(cell, flevels)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}

.factgrid2Prim <- function( flevels , margcell, margset){

  nr <- prod(flevels[-margset])
  nc <- length(flevels)
  mm <- matrix(NA, nr=nr, nc=nc)

  cell    <- rep(1, nc)
  cell[margset] <- margcell
                                        #print(cell)
  mm[1,]  <- cell
  if (nr>1)
    for (ii in 2:nr){
      cell <- nextCellMarg(cell, flevels, margset)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}




permuteCellEntries <- function(perm, flevels){
  return(.Call("R_permuteCellEntries", perm, flevels
               ,PACKAGE="gRbase"
               ))
}

.permuteCellEntriesR <- function(perm, flevels){

  pvec         <- as.integer(cumprod(flevels)/flevels[1])
  flevels.new  <- flevels[perm]
  entry.new    <- rep.int(0, prod(flevels))

  cell = as.integer(rep(1, length(flevels)))
  for (ii in 1:(length(entry.new))){
    entry.new[ii]  <- .getCellNumberR(cell, perm, pvec=pvec)
    cell           <- .nextCellR(cell, flevels.new)
  }
  entry.new
}

.permuteCellEntriesC <- function(perm, flevels){

  flevels   <- as.integer(flevels)
  entry.new <- rep.int(0L, prod(flevels))
  ans <- .C("C_permuteCellEntries", perm, flevels, length(flevels),
            entry.new, length(entry.new)
            ,PACKAGE="gRbase"
            )[[4]]
  ans
}

aperm2 <- function(a, permvec){
  flevels.orig  <- dim(a)

  if (is.numeric(permvec)){
    perm <- permvec
  } else {   
    vn            <- names(dimnames(a))
    perm          <- .flexmatch(permvec, vn)
  }
  flevels.new   <- flevels.orig[perm]
    
  .Call("R_aperm2", perm, flevels.orig, flevels.new
        ,PACKAGE="gRbase"
        )
}

.aperm2C <- function(a, permvec){
  dn            <- dimnames(a)
  vn            <- names(dn)
  flevels.orig  <- dim(a)
  perm          <- .flexmatch(permvec, vn)
  flevels.new   <- flevels.orig[perm]

  zz            <- .permuteCellEntriesC (perm, flevels.orig)
  ans           <- a[zz]
  dim(ans)      <- flevels.new
  dimnames(ans) <- dn[perm]
  ans
}

.flexmatch <- function(x, tab){
  if (is.numeric(x)){
    return(x)
  } else {
    if (is.null(tab)){
      stop("x is char and tab is NULL; can not proceed")
    } else {
      match(x, tab)
    }
  }
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
## 'flevels': the levels of the factors in the original table.
## 'perm': match(c("B","A","C"), c("A","B","C")) -> 2,1,3
## 'cell': (1,2,2) is a cell in the original table
## Output: The entry of the cell in the new table

.getCellNumberR <- function(cell, perm, flevels, pvec=cumprod(flevels)/flevels[1]){
  sum(pvec*(cell[perm]-1)) + 1
}

getCellNumberC <- function(cell, perm, flevels, pvec=cumprod(flevels)/flevels[1]){
  .C("C_getCellNumber", as.integer(cell), as.integer(perm), as.integer(pvec),
     length(cell), integer(1), DUP=FALSE
     ,PACKAGE="gRbase"
     )[[5]]
}
























