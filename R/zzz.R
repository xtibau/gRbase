.onLoad <- function(libname, pkgname){
  library.dynam("gRbase", package = pkgname, lib.loc = libname)
  return(invisible(0))
}
