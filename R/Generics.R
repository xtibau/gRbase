
## Generic functions used in gRbase, gRain, gRim

fit <- function(object, ...)
{
  UseMethod("fit")
}

triangulate <- function(object, ...)
{
  UseMethod("triangulate")
}

compile <- function (object, ...) 
{
    UseMethod("compile")
}

propagate <- function (object, ...) 
{
    UseMethod("propagate")
}
