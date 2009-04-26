
 .onLoad <- function(lib, pkg)
###.First.lib <- function(lib, pkg)
{
  library.dynam("gRbase", package = pkg, lib.loc = lib)
  return(invisible(0))
}


