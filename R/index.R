#' Retrieve/Replace the Index
#'
#' @description  Retrieve or replace the index of an \code{radf} object.
#'
#' @param x An object of class \code{\link[=radf]{radf()}}
#' @param ... Further arguments passed to methods.
#' @param value An ordered vector of the same length as the `index' attribute of x.
#'
#' @details If the user does not specify an index for the estimation a
#' pseudo-index is generated which is a sequential numeric series. After the estimation,
#' the user can use \code{index} to retrieve or \code{`index<-`} to replace the index.
#' The index can be either numeric or Date.
#' @name index.radf
NULL

#' @rdname index.radf
#' @param trunc default FALSE. If TRUE the index formed by truncating the value
#' in the minimum window.
#' @export
index.radf <- function(x, trunc = FALSE, ...) {
  value <- attr(x, "index")
  if (trunc) value <- value[-c(1:(minw(x) + lagr(x)))]
  value
}

#' @importFrom purrr detect_index
#' @importFrom lubridate is.Date
#' @rdname index.radf
#' @export
index.data.frame <- function(x, ...) {
  date_index <- purrr::detect_index(x, lubridate::is.Date)
  if (as.logical(date_index)) x[, date_index, drop = TRUE] else seq(1, NROW(x))
}

#' @rdname index.radf
#' @export
index.datestamp <- function(x, ...) {
  attr(x, "index")
}

#' @rdname  index.radf
#' @inheritParams index.radf
#' @export
`index<-.radf` <- function(x, value) {
  if (length(index(x)) != length(value)) {
    stop("length of index vectors does not match", call. = FALSE)
  }

  attr(x, "index") <- value
  return(x)
}
