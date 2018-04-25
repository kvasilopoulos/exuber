# Testing arguments ------------------------------------------------------

is.positive.int <- function(arg) {
  level <- deparse(substitute(arg))
  if (arg != round(arg) || arg <= 0) stop(sprintf("Argument '%s' should be a positive integer", level))
}

is.nonnegeative.int <- function(arg) {
  level <- deparse(substitute(arg))
  if (arg != round(arg) | arg < 0) stop(sprintf("Argument '%s' should be a non-negative integer", level))
}

is.between <- function(x, arg1, arg2) {
  level <- deparse(substitute(x))
  if (x < arg1 | x > arg2) stop(sprintf("Argument '%s' should be a be between '%d' and '%d'", level, arg1, arg2))
}

# .onUnload <- function(libpath) {
#   library.dynam.unload("exdyn", libpath)
# }
