#.Load.gRbase <- function() {
#  .Load.gRbase.general()
#  .Load.gRbase.hllm()
#  .Load.gRbase.hllmfit()
#  .Load.gRbase.hllmodify()
#  .Load.dynamicgraph()
#}


.First.lib <- function(lib, pkg)
{
  if((R.version$major == 1) && (as.numeric(R.version$minor) < 9))
    packageDescription <- package.description
  
  cat("\n")
  cat("-------------------------------------------------------------\n")
  cat(packageDescription("gRbase", lib = lib, field="Title"))
  cat("\n")
  ver  <- packageDescription("gRbase", lib = lib, field="Version")
  maint<- packageDescription("gRbase", lib = lib, field="Maintainer")
  autho<- packageDescription("gRbase", lib = lib, field="Author")
  descr<- packageDescription("gRbase", lib = lib, field="Description")
  built<- packageDescription("gRbase", lib = lib, field="Built")
  URL  <- packageDescription("gRbase", lib = lib, field="URL")
  cat(descr,"\n")
  cat(paste("gRbase, version", ver,  "is now loaded\n"))
  cat("Authors:",autho,"\n")
  cat("Maintained by",maint,"\n")
  cat("Webpage:",URL,"\n")
  cat("\nBuilt:",built,"\n")
  cat("-------------------------------------------------------------\n")

  require(methods)
  require(MASS)
  require(dynamicGraph)
#  .Load.gRbase()
  
  return(invisible(0))
}

.onAttach <- function (lib, pkg) 
{
    require(methods)
  require(MASS)
  require(dynamicGraph)
#    .Load.gRbase()
  }

.onLoad <- function (lib, pkg) 
{
    require(methods)
  require(MASS)
  require(dynamicGraph)
#    .Load.gRbase()
}


.Last.lib <- function(lib) {
  cat("Thank you for using gRbase\n")
  return(invisible(0))
}
