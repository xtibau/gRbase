
triangulate <-
  function(object, method="mcwh",
           nLevels=rep(2,length(nodes(object))), matrix=FALSE)
{
  UseMethod("triangulate")
}

compile <- 
function (object, ...) 
{
    UseMethod("compile")
}

propagate <- 
function (object, ...) 
{
    UseMethod("propagate")
}
