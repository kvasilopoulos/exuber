
#' Retrieve/Replace the index
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
  if (trunc) value <- value[-c(1:(get_minw(x) + get_lag(x)))]
  value
}


#' @rdname  index.radf
#' @inheritParams index.radf
#' @export
`index<-.radf` <- function(x, value) {
  if (length(index(x)) != length(value))
    stop_glue("length of index vectors does not match")

  attr(x, "index") <- value
  return(x)
}

#' @export
index.default <- function(x, ...) {
  seq_len(NROW(x))
}

# Defensive ---------------------------------------------------------------


#' @export
index.cv <- function(x, trunc = FALSE, ...) {
  if (is_mc(x))
    stop_glue("method `index` is not suppoted for {get_method(x)}")
  value <- attr(x, "index")
  if (is_sb(x)) {
    if (get_lag(x) != 0) {
      if (trunc) value <- value[-c(1:(get_minw(x) + get_lag(x) + 2))]
    }else{
      if (trunc) value <- value[-c(1:(get_minw(x)))]
    }
  }else{
    if (trunc) value <- value[-c(1:get_minw(x))]
  }
  value
}

#' @importFrom purrr detect_index
#' @importFrom lubridate is.Date
#' @export
index.data.frame <- function(x, ...) {
  date_index <- purrr::detect_index(x, lubridate::is.Date)
  if (as.logical(date_index)) x[, date_index, drop = TRUE] else seq(1, NROW(x))
}

#' @export
index.datestamp <- function(x, ...) {
  attr(x, "index")
}
