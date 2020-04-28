
#' Retrieve/Replace the index
#'
#' @description Retrieve or replace the index of an object.
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

#' @rdname index-rd
#' @export
`index<-` <- function(x,  value) {
  UseMethod("index<-")
}

#' @export
index.default <- function(x, ...) {
  attr(x, "index") %||% idx_seq(x)
}

#' @importFrom purrr detect_index
#' @importFrom lubridate is.Date
#' @export
index.data.frame <- function(x, ...) {
  date_index <- purrr::detect_index(x, lubridate::is.Date)
  if (as.logical(date_index)) x[, date_index, drop = TRUE] else seq_len(NROW(x))
}

#' @export
index.radf_obj <- function(x, trunc = FALSE, ...) {
  idx <- attr(x, "index")
  if (trunc) idx <- idx[-c(1:(get_minw(x) + get_lag(x)))]
  idx
}

#' @export
index.ds_radf <- index.radf_obj


#' @export
index.radf_cv <- function(x, trunc = FALSE, ...) {
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

index_radf_cv <- function(x, ...) {
  UseMethod(index_radf_cv())
}

index_radf_cv.mc_cv <- function(x, trunc, ...) {
  stop_glue("`index` is not suppoted for class `mc_cv`.")
}

index_radf_cv.wb_cv <- function(x, trunc, ...) {
  value <- attr(x, "index")
  value[-c(1:get_minw(x))]
}

index_radf_cv.sb_cv <- function(x, trunc, ...) {
  value <- attr(x, "index")
  if (trunc) {
    if (get_lag(x) != 0) {
      value <- value[-c(1:(get_minw(x) + get_lag(x) + 2))]
    }else{
      value <- value[-c(1:get_minw(x))]
    }
  }
  value
}


# `index<-` -----------------------------------------------------------------

#' @export
`index<-.default` <- function(x, value) {
  stop_glue("Don't know how to handle {class(x)} objects.")
}

#' @export
`index<-.radf_obj` <- function(x, value) {
  if (length(index(x)) != length(value)) {
    stop_glue("length of index vectors does not match")
  }
  attr(x, "index") <- value
  x
}

