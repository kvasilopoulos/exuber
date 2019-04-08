
#' Retrieve/Set column names
#'
#' Retrieve or set the column names of a class \code{\link[=radf]{radf()}} object. Similar to \code{colnames}, with the only
#' difference that \code{col_names} is for \code{\link[=radf]{radf()}} objects.
#'
#' @inheritParams index.radf
#' @export
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100))
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


#' @rdname col_names
#' @export
`col_names<-` <- function(x, value) {
  UseMethod("col_names<-")
}

#' @export
col_names.radf <- function(x, ...) {
  attr(x, "col_names")
}

#' @export
#' @importFrom purrr imap
#' @importFrom magrittr set_colnames set_names
`col_names<-.radf` <- function(x, value) {
  if (length(col_names(x)) != length(value)) {
    stop("length of col_names vectors does not match", call. = FALSE)
  }

  seq_cv <- c("badf", "bsadf")
  cv <- c("adf", "sadf", "gsadf")

  x[seq_cv] <- x[seq_cv] %>% imap(~ set_colnames(.x, value))
  x[cv] <- x[cv] %>% imap(~ set_names(.x, value))

  attr(x, "col_names") <- value
  x
}
