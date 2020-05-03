

#' Retrieve/Replace series names
#'
#' Retrieve or replace the series names of an object.
#'
#' @param x An object.
#' @param ... Further arguments passed to methods.
#' @export
#'
series_names <- function(x, ...) {
  UseMethod("series_names")
}


#' @rdname series_names
#' @param value n ordered vector of the same length as the "index" attribute of x.
#' @export
`series_names<-` <- function(x, value) {
  UseMethod("series_names<-")
}

# Methods -----------------------------------------------------------------


#' @export
series_names.default <- function(x, ...) {
  attr(x, "series_names")
}

#' @export
`series_names<-.default` <- function(x, value) {
  if (is.null(attr(x, "series_names"))) {
    return(NULL)
  }
  if (length(series_names(x)) != length(value)) {
    stop("length of series_names vectors does not match", call. = FALSE)
  }
  attr(x, "series_names") <- value
  x
}

#' @rdname series_names
#' @export
#' @importFrom purrr imap
#' @importFrom rlang set_names
#' @examples
#'
#' # Simulate bubble processes
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(dta)
#'
#' series_names(rfd) <- c("OneBubble", "TwoBubbles")
#'
`series_names<-.radf_obj` <- function(x, value) {
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

#' @rdname series_names
#' @export
`series_names<-.wb_cv` <- function(x, value) {
  if (length(series_names(x)) != length(value)) {
    stop("length of series_names vectors does not match", call. = FALSE)
  }
  cv <- c("adf_cv", "sadf_cv", "gsadf_cv")
  x[cv] <- x[cv] %>%
    imap(~ `rownames<-`(.x, value))
  seq_cv <- c("badf_cv", "bsadf_cv")
  x[seq_cv] <- x[seq_cv] %>%
    imap(~ `dimnames<-`(.x, list(NULL, c("90%", "95%", "99%"), value)))
  attr(x, "series_names") <- value
  x
}

#' @rdname series_names
#' @export
`series_names<-.sb_cv` <- function(x, value) {
  if (length(series_names(x)) != length(value)) {
    stop("length of series_names vectors does not match", call. = FALSE)
  }
  cv <- c("gsadf_panel")
  x[cv] <- x[cv] %>%
    imap(~ `rownames<-`(.x, value))
  seq_cv <- c("bsadf_panel")
  x[seq_cv] <- x[seq_cv] %>%
    imap(~ `dimnames<-`(.x, list(NULL, c("90%", "95%", "99%"), value)))
  attr(x, "series_names") <- value
  x
}

