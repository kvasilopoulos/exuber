
#' Retrieve/Replace the index
#'
#' @description  Retrieve or replace the index of an object.
#'
#' @param x An object.

#' @param ... Further arguments passed to methods.
#' @param value An ordered vector of the same length as the `index' attribute of x.
#'
#' @details If the user does not specify an index for the estimation a
#' pseudo-index is generated which is a sequential numeric series. After the estimation,
#' the user can use \code{index} to retrieve or \code{`index<-`} to replace the index.
#' The index can be either numeric or Date.
#' @export
#' @name index-rd
index <- function(x, ...) {
  UseMethod("index")
}

#' @export
index.default <- function(x, ...) {
  if (is.null(attr(x, "index"))) {
    seq_len(NROW(x))
  }else{
    attr(x, "index")
  }
}

#' @importFrom purrr detect_index
#' @importFrom lubridate is.Date
#' @export
index.data.frame <- function(x, ...) {
  date_index <- purrr::detect_index(x, lubridate::is.Date)
  if (as.logical(date_index)) x[, date_index, drop = TRUE] else seq_len(NROW(x))
}

#' @export
index.datestamp <- index.radf <- function(x, trunc = FALSE, ...) {
  idx <- attr(x, "index")
  if (trunc) idx <- idx[-c(1:(get_minw(x) + get_lag(x)))]
  idx
}

#' @export
index.cv <- function(x, trunc = FALSE, ...) {
  if (is_mc(x)) {
    stop_glue("method `index` is not suppoted for {get_method(x)}")
  }
  value <- attr(x, "index")
  if (trunc) {
    if (is_sb(x) && (get_lag(x) != 0)) {
      value <- value[-c(1:(get_minw(x) + get_lag(x) + 2))]
    }else{
      value <- value[-c(1:get_minw(x))]
    }
  }
  value
}

#'@rdname index-rd
#'@export
`index<-` <- function(x,  value) {
  UseMethod("index<-")
}

#' @export
`index<-.default` <- function(x, value) {
  stop_glue("Don't know how to handle {class(x)} objects.")
}

#' @export
`index<-.radf` <- function(x, value) {
  if (length(index(x)) != length(value)) {
    stop_glue("length of index vectors does not match")
  }
  attr(x, "index") <- value
  x
}

