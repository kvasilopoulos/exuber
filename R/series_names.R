
#' Retrieve/Set column names
#'
#' Retrieve or set the column names of a class \code{\link[=radf]{radf()}} object.
#' Similar to \code{colnames}, with the only difference that \code{series_names} is
#' for \code{\link[=radf]{radf()}} objects.
#'
#' @param x An object.
#' @param ... further arguments passed to methods.
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate bubble processes
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(dta)
#' series_names(rfd) <- c("OneBubble", "TwoBubbles")
#' }
series_names <- function(x, ...) {
  UseMethod("series_names")
}

#' @export
series_names.default <- function(x, ...) {
  attr(x, "series_names")
}

#' @rdname series_names
#' @param value n ordered vector of the same length as the "index" attribute of x.
#' @export
`series_names<-` <- function(x, value) {
  UseMethod("series_names<-")
}

#' @rdname series_names
#' @export
`series_names<-` <- function(x, value) {
  UseMethod("series_names<-")
}

#' @export
`series_names<-.default` <- function(x, value) {
  if (length(series_names(x)) != length(value)) {
    stop("length of series_names vectors does not match", call. = FALSE)
  }
  attr(x, "series_names") <- value
}

#' @export
#' @importFrom purrr imap
#' @importFrom rlang set_names
`series_names<-.radf` <- function(x, value) {
  if (length(series_names(x)) != length(value)) {
    stop("length of series_names vectors does not match", call. = FALSE)
  }
  seq_cv <- c("badf", "bsadf")
  cv <- c("adf", "sadf", "gsadf")
  x[seq_cv] <- x[seq_cv] %>%
    imap(~ `colnames<-`(.x, value))
  x[cv] <- x[cv] %>%
    imap(~ set_names(.x, value))
  attr(x, "series_names") <- value
  x
}
