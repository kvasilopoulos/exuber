
#' Retrieve/Set column names
#'
#' Retrieve or set the column names of a class \code{\link[=radf]{radf()}} object. Similar to \code{colnames}, with the only
#' difference that \code{col_names} is for \code{\link[=radf]{radf()}} objects.
#'
#' @inheritParams index.radf
#' @export
#'
#' @examples
#' \dontrun{
#' # Simulate bubble processes
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(dta)
#' col_names(rfd) <- c("OneBubble", "TwoBubbles")
#' }
col_names <- function(x, ...) {
  UseMethod("col_names")
}

#' @export
col_names.default <- function(x, ...) {
  base::colnames(x)
}

# TODO maybe rename col_names to snames

#' @rdname col_names
#' @export
`col_names<-` <- function(x, value) {
  UseMethod("col_names<-")
}

col_names.diagnostics <- function(x, ...) {
  attr(x, "col_names")
}

#' @export
col_names.radf <- function(x, ...) {
  attr(x, "col_names")
}

#' @export
#' @importFrom purrr imap
#' @importFrom rlang set_names
`col_names<-.radf` <- function(x, value) {
  if (length(col_names(x)) != length(value)) {
    stop("length of col_names vectors does not match", call. = FALSE)
  }

  seq_cv <- c("badf", "bsadf")
  cv <- c("adf", "sadf", "gsadf")

  x[seq_cv] <- x[seq_cv] %>% imap(~ `colnames<-`(.x, value))
  x[cv] <- x[cv] %>% imap(~ set_names(.x, value))

  attr(x, "col_names") <- value
  x
}
