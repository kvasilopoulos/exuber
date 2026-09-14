#' Retrieve/Replace the index
#'
#' @description Retrieve or replace the index of an object.
#'
#' @param x An object.
#' @param ... Further arguments passed to methods.
#' @param value An ordered vector of the same length as the `index` attribute of x.
#'
#' @details If the user does not specify an index for the estimation a
#' pseudo-index is generated which is a sequential numeric series. After the estimation,
#' the user can use `index()` to retrieve or `index<-()` to replace the index.
#' The index can be either numeric or Date.
#'
#' @examples
#' \donttest{
#' # A plain numeric vector gets a pseudo-index (1, 2, 3, ...)
#' rsim <- radf(sim_data)
#' head(index(rsim))
#'
#' # A data.frame with a Date column uses it as the index automatically
#' rsim_wdate <- radf(sim_data_wdate)
#' head(index(rsim_wdate))
#' class(index(rsim_wdate))
#'
#' # autoplot() uses index() internally for the x-axis
#' autoplot(rsim_wdate)
#'
#' # Replace the index, e.g. with a custom Date sequence
#' index(rsim) <- seq(as.Date("2000-01-01"), by = "month", length.out = length(index(rsim)))
#' head(index(rsim))
#' autoplot(rsim)
#' }
#' @return The index of the object (a \code{Date} vector when the input data
#'   carried one, otherwise an integer sequence); the replacement form
#'   returns the modified object.
#' @export
#' @name index-rd
index <- function(x, ...) {
  UseMethod("index")
}

#' @rdname index-rd
#' @export
`index<-` <- function(x, value) {
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
    } else {
      value <- value[-c(1:get_minw(x))]
    }
  }
  value
}

index_radf_cv <- function(x, ...) {
  UseMethod("index_radf_cv")
}

#' @export
index_radf_cv.mc_cv <- function(x, trunc, ...) {
  stop_glue("`index` is not suppoted for class `mc_cv`.")
}

#' @export
index_radf_cv.wb_cv <- function(x, trunc, ...) {
  value <- attr(x, "index")
  value[-c(1:get_minw(x))]
}

#' @export
index_radf_cv.sb_cv <- function(x, trunc, ...) {
  value <- attr(x, "index")
  if (trunc) {
    if (get_lag(x) != 0) {
      value <- value[-c(1:(get_minw(x) + get_lag(x) + 2))]
    } else {
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


# mat ---------------------------------------------------------------------

mat <- function(x, ...) {
  attr(x, "mat")
}

# mat.radf_obj <- function(x, ...) {
#   mat <- attr(x, "mat")
#   attributes(mat) <- NULL
#   mat
# }
