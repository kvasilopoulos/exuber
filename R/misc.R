
#' Retrieve/Replace the Index of a 'radf' object
#'
#' @description  The user can retrieve/replace the index of a'radf' object.
#'
#' @inheritParams report
#' @param ... further arguements passed to methods.
#' @param value an ordered vector of the same length as the 'index' attribute of x.
#'
#' @details If the use didn't specify the index during the estimation, the function will generate pseudo-index
#' via creating a numeric sequence adancing by one. After the estimation, the user can use \code{index} to retrieve
#' or \code{`index<-`} to replace the index. The index can numeric or Date.
#'
#'
#' @export
index <- function(x, ...)
{
  UseMethod("index")
}

#' @export
index.default <- function(x, ...)
{
  if (exists('attr(x, "index")')) {
    index(attr(x, "index"))
  }else {
  seq_len(NROW(x))
  }
}

#' @export
index.radf <- function(x, ...)
{
  attr(x, "index")
}


#' @rdname index
#' @export
`index<-` <- function(x, value)
{
  UseMethod("index<-")
}


#' @export
`index<-.radf` <- function(x, value)
{
  if (length(index(x)) != length(value)) {
    stop("length of index vectors does not match", call. = FALSE)
  }
  attr(x, "index") <- value
  return(x)
}

#' Column Names of a 'radf' object
#'
#' @inheritParams index
#'
#' @export
col_names <- function(x, ...)
{
  UseMethod("col_names")
}

#' @rdname col_names
#' @export
`col_names<-` <- function(x, value)
{
  UseMethod("col_names<-")
}

#' @export
col_names.radf <- function(x, ...)
{
  attr(x, "col_names")
}

#' @export
`col_names<-.radf` <- function(x, value)
{
  if (length(col_names(x)) != length(value)) {
    stop("length of col_names vectors does not match", call. = FALSE)
  }
  attr(x, "col_names") <- value
  x
}
