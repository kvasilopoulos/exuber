#' Retrieve/Replace the Index
#'
#' @description  Retrieve or replace the index of a \code{radf} object.
#'
#' @param x An object of class \code{\link[=radf]{radf()}}
#' @param ... Further arguments passed to methods.
#' @param value An ordered vector of the same length as the `index' attribute of x.
#'
#' @details If the user does not specify an index during the estimation a
#' pseudo-index is generated which is a sequential numeric series. After the estimation,
#' the user can use \code{index} to retrieve or \code{`index<-`} to replace the index.
#' The index can be either numeric or Date.
#' @name index.radf
NULL

#' @rdname index.radf
#' @export
index.radf <- function(x, ...) {
  attr(x, "index")
}

#' @rdname  index.radf
#' @export
`index<-.radf` <- function(x, value) {
  if (length(index(x)) != length(value)) {
    stop("length of index vectors does not match", call. = FALSE)
  }
  attr(x, "index") <- value
  return(x)
}
