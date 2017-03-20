#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void C_cell2entry(void *, void *, void *, void *);
extern void C_cell2entry2(void *, void *, void *, void *);
extern void C_getCellNumber(void *, void *, void *, void *, void *);
extern void C_isin(void *, void *, void *, void *, void *, void *);
extern void C_maxset(void *, void *, void *, void *);
extern void C_minset(void *, void *, void *, void *);
extern void C_nextCell(void *, void *, void *);
extern void C_nextCellSlice(void *, void *, void *, void *, void *);
extern void C_nextCellSlice_indic(void *, void *, void *, void *, void *);
extern void C_permuteCellEntries(void *, void *, void *, void *, void *);
extern void C_slice2entry(void *, void *, void *, void *, void *, void *, void *);
extern void combnC(void *, void *, void *, void *);

/* .Call calls */
extern SEXP R_colSums(SEXP);
extern SEXP R_colwiseProd(SEXP, SEXP);
extern SEXP R_permuteCellEntries(SEXP, SEXP);
extern SEXP R_rowSums(SEXP);
extern SEXP R_slice2entry(SEXP, SEXP, SEXP);
extern SEXP gRbase_MAT2ftM_(SEXP);
extern SEXP gRbase_RcppExport_registerCCallable();
extern SEXP gRbase_adjList2dgCMatrix(SEXP);
extern SEXP gRbase_adjList2ftList(SEXP);
extern SEXP gRbase_adjList2ftM(SEXP);
extern SEXP gRbase_adjList2matrix(SEXP);
extern SEXP gRbase_adjList2tfList(SEXP);
extern SEXP gRbase_adjList2tfM(SEXP);
extern SEXP gRbase_allSubsets0__(SEXP);
extern SEXP gRbase_allSubsets__(SEXP);
extern SEXP gRbase_aperm__(SEXP, SEXP);
extern SEXP gRbase_cell2entry2_cpp(SEXP, SEXP);
extern SEXP gRbase_cell2entry_cpp(SEXP, SEXP);
extern SEXP gRbase_colmat2list(SEXP);
extern SEXP gRbase_dagList2dgCMatrix(SEXP, SEXP);
extern SEXP gRbase_dagList2matrix(SEXP, SEXP);
extern SEXP gRbase_dgCMatrix2matrix(SEXP);
extern SEXP gRbase_dimnames_match_(SEXP, SEXP, SEXP);
extern SEXP gRbase_do_getcq_dense(SEXP, SEXP);
extern SEXP gRbase_do_getcq_sparse(SEXP, SEXP);
extern SEXP gRbase_do_mcs_dense(SEXP, SEXP);
extern SEXP gRbase_do_mcs_sparse(SEXP, SEXP);
extern SEXP gRbase_do_triangulate_elo(SEXP, SEXP);
extern SEXP gRbase_getCellNumberPrim_cpp(SEXP, SEXP, SEXP);
extern SEXP gRbase_getCellNumber_cpp(SEXP, SEXP, SEXP);
extern SEXP gRbase_getCliquesDec_(SEXP, SEXP);
extern SEXP gRbase_get_subset_(SEXP, SEXP, SEXP);
extern SEXP gRbase_get_superset_(SEXP, SEXP, SEXP);
extern SEXP gRbase_is_dimnames_(SEXP);
extern SEXP gRbase_is_named_array_(SEXP);
extern SEXP gRbase_is_number_vector_(SEXP);
extern SEXP gRbase_is_subsetof_(SEXP, SEXP);
extern SEXP gRbase_isadjMAT_(SEXP);
extern SEXP gRbase_isdagMAT_(SEXP);
extern SEXP gRbase_issymMAT_(SEXP);
extern SEXP gRbase_isugMAT_(SEXP);
extern SEXP gRbase_matrix2dgCMatrix(SEXP);
extern SEXP gRbase_mcsMAT0_(SEXP, SEXP);
extern SEXP gRbase_mcsMAT_(SEXP, SEXP);
extern SEXP gRbase_moralizeMAT(SEXP);
extern SEXP gRbase_names2pairsM(SEXP, SEXP, SEXP, SEXP);
extern SEXP gRbase_nextCellSlicePrim_cpp(SEXP, SEXP, SEXP);
extern SEXP gRbase_nextCellSlice_cpp(SEXP, SEXP, SEXP);
extern SEXP gRbase_nextCell_cpp(SEXP, SEXP);
extern SEXP gRbase_permuteCellEntries_cpp(SEXP, SEXP);
extern SEXP gRbase_rip_internal(SEXP, SEXP, SEXP);
extern SEXP gRbase_rowmat2list(SEXP);
extern SEXP gRbase_slice2entry_cpp(SEXP, SEXP, SEXP);
extern SEXP gRbase_solveSPD(SEXP);
extern SEXP gRbase_sp_setXtf1(SEXP, SEXP);
extern SEXP gRbase_symMAT2ftM_(SEXP);
extern SEXP gRbase_tabAdd__(SEXP, SEXP);
extern SEXP gRbase_tabAlign__(SEXP, SEXP);
extern SEXP gRbase_tabDiv0__(SEXP, SEXP);
extern SEXP gRbase_tabDiv__(SEXP, SEXP);
extern SEXP gRbase_tabEqual__(SEXP, SEXP, SEXP);
extern SEXP gRbase_tabExpand__(SEXP, SEXP);
extern SEXP gRbase_tabListAdd__(SEXP);
extern SEXP gRbase_tabListMult__(SEXP);
extern SEXP gRbase_tabMarg__(SEXP, SEXP);
extern SEXP gRbase_tabMult__(SEXP, SEXP);
extern SEXP gRbase_tabOp__(SEXP, SEXP, SEXP);
extern SEXP gRbase_tabPerm__(SEXP, SEXP);
extern SEXP gRbase_tabSubt__(SEXP, SEXP);
extern SEXP gRbase_testOperations(SEXP, SEXP);
extern SEXP gRbase_topoSortMAT_(SEXP);
extern SEXP gRbase_triangulateMAT_(SEXP, SEXP);
extern SEXP gRbase_ugList2dgCMatrix(SEXP, SEXP);
extern SEXP gRbase_ugList2matrix(SEXP, SEXP);
extern SEXP gRbase_which_matrix_index(SEXP);

static const R_CMethodDef CEntries[] = {
    {"C_cell2entry", (DL_FUNC) &C_cell2entry, 4},
    {"C_cell2entry2", (DL_FUNC) &C_cell2entry2, 4},
    {"C_getCellNumber", (DL_FUNC) &C_getCellNumber, 5},
    {"C_isin", (DL_FUNC) &C_isin, 6},
    {"C_maxset", (DL_FUNC) &C_maxset, 4},
    {"C_minset", (DL_FUNC) &C_minset, 4},
    {"C_nextCell", (DL_FUNC) &C_nextCell, 3},
    {"C_nextCellSlice", (DL_FUNC) &C_nextCellSlice, 5},
    {"C_nextCellSlice_indic", (DL_FUNC) &C_nextCellSlice_indic, 5},
    {"C_permuteCellEntries", (DL_FUNC) &C_permuteCellEntries, 5},
    {"C_slice2entry", (DL_FUNC) &C_slice2entry, 7},
    {"combnC", (DL_FUNC) &combnC, 4},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"R_colSums", (DL_FUNC) &R_colSums, 1},
    {"R_colwiseProd", (DL_FUNC) &R_colwiseProd, 2},
    {"R_permuteCellEntries", (DL_FUNC) &R_permuteCellEntries, 2},
    {"R_rowSums", (DL_FUNC) &R_rowSums, 1},
    {"R_slice2entry", (DL_FUNC) &R_slice2entry, 3},
    {"gRbase_MAT2ftM_", (DL_FUNC) &gRbase_MAT2ftM_, 1},
    {"gRbase_RcppExport_registerCCallable", (DL_FUNC) &gRbase_RcppExport_registerCCallable, 0},
    {"gRbase_adjList2dgCMatrix", (DL_FUNC) &gRbase_adjList2dgCMatrix, 1},
    {"gRbase_adjList2ftList", (DL_FUNC) &gRbase_adjList2ftList, 1},
    {"gRbase_adjList2ftM", (DL_FUNC) &gRbase_adjList2ftM, 1},
    {"gRbase_adjList2matrix", (DL_FUNC) &gRbase_adjList2matrix, 1},
    {"gRbase_adjList2tfList", (DL_FUNC) &gRbase_adjList2tfList, 1},
    {"gRbase_adjList2tfM", (DL_FUNC) &gRbase_adjList2tfM, 1},
    {"gRbase_allSubsets0__", (DL_FUNC) &gRbase_allSubsets0__, 1},
    {"gRbase_allSubsets__", (DL_FUNC) &gRbase_allSubsets__, 1},
    {"gRbase_aperm__", (DL_FUNC) &gRbase_aperm__, 2},
    {"gRbase_cell2entry2_cpp", (DL_FUNC) &gRbase_cell2entry2_cpp, 2},
    {"gRbase_cell2entry_cpp", (DL_FUNC) &gRbase_cell2entry_cpp, 2},
    {"gRbase_colmat2list", (DL_FUNC) &gRbase_colmat2list, 1},
    {"gRbase_dagList2dgCMatrix", (DL_FUNC) &gRbase_dagList2dgCMatrix, 2},
    {"gRbase_dagList2matrix", (DL_FUNC) &gRbase_dagList2matrix, 2},
    {"gRbase_dgCMatrix2matrix", (DL_FUNC) &gRbase_dgCMatrix2matrix, 1},
    {"gRbase_dimnames_match_", (DL_FUNC) &gRbase_dimnames_match_, 3},
    {"gRbase_do_getcq_dense", (DL_FUNC) &gRbase_do_getcq_dense, 2},
    {"gRbase_do_getcq_sparse", (DL_FUNC) &gRbase_do_getcq_sparse, 2},
    {"gRbase_do_mcs_dense", (DL_FUNC) &gRbase_do_mcs_dense, 2},
    {"gRbase_do_mcs_sparse", (DL_FUNC) &gRbase_do_mcs_sparse, 2},
    {"gRbase_do_triangulate_elo", (DL_FUNC) &gRbase_do_triangulate_elo, 2},
    {"gRbase_getCellNumberPrim_cpp", (DL_FUNC) &gRbase_getCellNumberPrim_cpp, 3},
    {"gRbase_getCellNumber_cpp", (DL_FUNC) &gRbase_getCellNumber_cpp, 3},
    {"gRbase_getCliquesDec_", (DL_FUNC) &gRbase_getCliquesDec_, 2},
    {"gRbase_get_subset_", (DL_FUNC) &gRbase_get_subset_, 3},
    {"gRbase_get_superset_", (DL_FUNC) &gRbase_get_superset_, 3},
    {"gRbase_is_dimnames_", (DL_FUNC) &gRbase_is_dimnames_, 1},
    {"gRbase_is_named_array_", (DL_FUNC) &gRbase_is_named_array_, 1},
    {"gRbase_is_number_vector_", (DL_FUNC) &gRbase_is_number_vector_, 1},
    {"gRbase_is_subsetof_", (DL_FUNC) &gRbase_is_subsetof_, 2},
    {"gRbase_isadjMAT_", (DL_FUNC) &gRbase_isadjMAT_, 1},
    {"gRbase_isdagMAT_", (DL_FUNC) &gRbase_isdagMAT_, 1},
    {"gRbase_issymMAT_", (DL_FUNC) &gRbase_issymMAT_, 1},
    {"gRbase_isugMAT_", (DL_FUNC) &gRbase_isugMAT_, 1},
    {"gRbase_matrix2dgCMatrix", (DL_FUNC) &gRbase_matrix2dgCMatrix, 1},
    {"gRbase_mcsMAT0_", (DL_FUNC) &gRbase_mcsMAT0_, 2},
    {"gRbase_mcsMAT_", (DL_FUNC) &gRbase_mcsMAT_, 2},
    {"gRbase_moralizeMAT", (DL_FUNC) &gRbase_moralizeMAT, 1},
    {"gRbase_names2pairsM", (DL_FUNC) &gRbase_names2pairsM, 4},
    {"gRbase_nextCellSlicePrim_cpp", (DL_FUNC) &gRbase_nextCellSlicePrim_cpp, 3},
    {"gRbase_nextCellSlice_cpp", (DL_FUNC) &gRbase_nextCellSlice_cpp, 3},
    {"gRbase_nextCell_cpp", (DL_FUNC) &gRbase_nextCell_cpp, 2},
    {"gRbase_permuteCellEntries_cpp", (DL_FUNC) &gRbase_permuteCellEntries_cpp, 2},
    {"gRbase_rip_internal", (DL_FUNC) &gRbase_rip_internal, 3},
    {"gRbase_rowmat2list", (DL_FUNC) &gRbase_rowmat2list, 1},
    {"gRbase_slice2entry_cpp", (DL_FUNC) &gRbase_slice2entry_cpp, 3},
    {"gRbase_solveSPD", (DL_FUNC) &gRbase_solveSPD, 1},
    {"gRbase_sp_setXtf1", (DL_FUNC) &gRbase_sp_setXtf1, 2},
    {"gRbase_symMAT2ftM_", (DL_FUNC) &gRbase_symMAT2ftM_, 1},
    {"gRbase_tabAdd__", (DL_FUNC) &gRbase_tabAdd__, 2},
    {"gRbase_tabAlign__", (DL_FUNC) &gRbase_tabAlign__, 2},
    {"gRbase_tabDiv0__", (DL_FUNC) &gRbase_tabDiv0__, 2},
    {"gRbase_tabDiv__", (DL_FUNC) &gRbase_tabDiv__, 2},
    {"gRbase_tabEqual__", (DL_FUNC) &gRbase_tabEqual__, 3},
    {"gRbase_tabExpand__", (DL_FUNC) &gRbase_tabExpand__, 2},
    {"gRbase_tabListAdd__", (DL_FUNC) &gRbase_tabListAdd__, 1},
    {"gRbase_tabListMult__", (DL_FUNC) &gRbase_tabListMult__, 1},
    {"gRbase_tabMarg__", (DL_FUNC) &gRbase_tabMarg__, 2},
    {"gRbase_tabMult__", (DL_FUNC) &gRbase_tabMult__, 2},
    {"gRbase_tabOp__", (DL_FUNC) &gRbase_tabOp__, 3},
    {"gRbase_tabPerm__", (DL_FUNC) &gRbase_tabPerm__, 2},
    {"gRbase_tabSubt__", (DL_FUNC) &gRbase_tabSubt__, 2},
    {"gRbase_testOperations", (DL_FUNC) &gRbase_testOperations, 2},
    {"gRbase_topoSortMAT_", (DL_FUNC) &gRbase_topoSortMAT_, 1},
    {"gRbase_triangulateMAT_", (DL_FUNC) &gRbase_triangulateMAT_, 2},
    {"gRbase_ugList2dgCMatrix", (DL_FUNC) &gRbase_ugList2dgCMatrix, 2},
    {"gRbase_ugList2matrix", (DL_FUNC) &gRbase_ugList2matrix, 2},
    {"gRbase_which_matrix_index", (DL_FUNC) &gRbase_which_matrix_index, 1},
    {NULL, NULL, 0}
};

void R_init_gRbase(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
