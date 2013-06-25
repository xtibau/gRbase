\name{gRbase-internal}

%% Various R/C/C++ implementations; 
%% --------------------------------
%% no need to document further
\alias{topoSortMAT_spCpp}
\alias{topoSortMAT_stCpp}

\alias{triangulateMAT_stC}
\alias{triangulateMAT_spR}
\alias{triangulateMAT_spCpp} %% RcppEigen
\alias{triangulateMAT_stCpp} %% Wrapper to Eigen version

\alias{mcsMAT_stR}
\alias{mcsMAT_stC}
\alias{mcsMAT_spR}
\alias{mcsMAT_spCpp}
\alias{mcsMAT_stCpp}

\alias{moralizeMAT_stR}
\alias{moralizeMAT_spC}

\alias{sp_topoSort} %% FIXME: Delete when new version of gRain is on CRAN. 
\alias{glist2setMAT}


%% Fast coercion of "matrix" to "dgCMatrix"
\alias{asdgCMatrix}         
\alias{asdgCMatrix.matrix}
\alias{asdgCMatrix.Matrix}

\alias{igraph-class}

%% Some methods which need not be in the proper .Rd files (to shorten
%% the listing)


\alias{varNames.parray}
\alias{nLevels.parray}
\alias{valueLabels.parray}

\alias{varNames.array}
\alias{nLevels.array}
\alias{valueLabels.array}
\alias{print.parray}


\alias{maxClique}


%% Some igraph stuff. Should be removed one day
\alias{mcs.igraph}
\alias{mcsmarked.igraph}
\alias{junctionTree.igraph}
\alias{jTree.igraph}
\alias{rip.igraph}
\alias{moralize.igraph}
\alias{triangulate.igraph}

%% Needs .Rd - New Cpp implementations
%% -----------------------------------
%% Undocumented utilities
\alias{sp_getXi}
\alias{sp_getXj}
\alias{sp_getXij}
\alias{sp_getXtf}
\alias{sp_getXM}
\alias{sp_setXij1}
\alias{sp_setXtf1}
\alias{sp_setXM1}
\alias{sp_moralize}
\alias{sp_fromto}
\alias{st_fromto}

\alias{ripMAT2}
\alias{moralMAT}
\alias{lapplyV2I}
\alias{lapplyI2V}
\alias{lapplyMatch}

\alias{solveSPD} %% FIXME: Inverse of symmetric pos. def. matrix; needs .Rd


%% Needs .Rd

\alias{mcsMATR}
\alias{triang2R}
\alias{vparlist2tf}
\alias{isGSD_glist}
\alias{properties_glist}

\alias{coerceGraph}
\alias{coerceGraph.graphNEL}
\alias{coerceGraph.igraph}
\alias{coerceGraph.matrix}

%% Array operations in C
%% ---------------------
%% Examples of the usage of these functions are given in
%% vignette
\alias{aperm2}
\alias{cell2entry}
\alias{entry2cell}
\alias{factGrid}
\alias{getCellNumberC}
\alias{slice2entry}
\alias{nextCell}
\alias{nextCellSlice}
\alias{permuteCellEntries}

%%%
%%% SHD stuff
%%%

\alias{makeDimNames}
\alias{names2pairs}
\alias{rowmat2list}
\alias{colmat2list}
\alias{matrix2list}
\alias{ellK}

\alias{list2rhsFormula}
\alias{rhsFormula2list}
\alias{list2rhsf}
\alias{rhsf2list}

\alias{which.arr.ind}
\alias{pairs2num}
%% !!

%% DED stuff
\alias{showf}  % used in old gRbase stuff; delete later
\alias{readf}  % used in old gRbase stuff; delete later


\alias{dual.rep}
%% !!

%% ggm related stuff
\alias{fit.ggm}
\alias{ggm}
\alias{ips}
\alias{outfun}
\alias{ell}
%% !!

\alias{addEdge}
\alias{dropEdge}
\alias{addEdge.gModel}
\alias{dropEdge.gModel}



\alias{all.subsets}
\alias{extract.power}
\alias{selectOrder}
\alias{processFormula}
\alias{partial.corr.matrix}

\alias{update.gModel}
\alias{gRfit-class}

\alias{stepwise}
\alias{stepwise.default}



\title{Internal gRbase functions}
\description{
  Internal functions called by other functions.
}
\keyword{internal}

%%\alias{showg}
%%\alias{readg}
%%\alias{varset}
%%\alias{in.list}
%%\alias{is.cont}
%%\alias{contains}
%%\alias{is.graphical}
