#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# get crit data --------------------------------------------------------

get_crit <- function(x) {
  nr <- NROW(index(x))
  if (nr > 5 && nr <= length(crit)) {
    return(get("crit")[[nr]])
  } else {
    stop("cannot provide MC critical values see help(crit)", call. = FALSE)
  }
}


# is_panel ----------------------------------------------------------------

is_panel <- function(y) {
  assert_class(y, "cv")
  res <- if (method(y) == "Sieve Bootstrap") TRUE else FALSE
  res
}

# remove index ----------------------------------------------------------

#' @importFrom purrr detect_index
#' @importFrom lubridate is.Date
rm_index <- function(data) {
  if (is.data.frame(data)) {
    date_index <- purrr::detect_index(data, lubridate::is.Date)
    if (as.logical(date_index)) data <- data[, -date_index, drop = FALSE]
  }
  data
}

# assert arguments ------------------------------------------------------


assert_positive_int <- function(arg, strictly = TRUE, greater_than = NULL) {
  level <- deparse(substitute(arg))
  if (strictly) {
    if (arg != trunc(arg) || arg <= 0) {
      stop(sprintf("Argument '%s' should be a positive integer", level),
        call. = FALSE
      )
    }
  } else {
    if (arg != trunc(arg) | arg < 0L) {
      stop(sprintf("Argument '%s' should be a non-negative integer", level), call. = FALSE)
    }
  }
  if (!is.null(greater_than)) {
    if (arg <= greater_than) {
      stop(sprintf(
        "Argument '%s' should be greater than '%d'",
        level, greater_than
      ), call. = FALSE)
    }
  }
}

assert_between <- function(x, arg1, arg2) {
  level <- deparse(substitute(x))
  if (!dplyr::between(x, arg1, arg2)) {
    stop(sprintf(
      "Argument '%s' should be a be between '%d' and '%d'",
      level, arg1, arg2
    ), call. = FALSE)
  }
}

assert_class <- function(x, klass) {
  xstring <- deparse(substitute(x))
  # klass <- deparse(substitute(klass))
  if (!inherits(x, klass)) {
    stop(sprintf("Argument '%s' should be of class '%s'", xstring, klass),
      call. = FALSE
    )
  }
}


assert_na <- function(x) {
  if (any(is.na(x))) {
    stop("RLS estimation cannot handle NA", call. = FALSE)
  }
}

"%ni%" <- Negate("%in%")


assert_equal_arg <- function(x, y, panel = FALSE) {
  if (minw(x) != minw(y)) stop("Different minimum window", call. = FALSE)

  if (method(y) == "Sieve Bootstrap") {
    if (lagr(x) != lagr(y)) stop("Different lag values", call. = FALSE)
  }
}

# Access attributes easily ------------------------------------------------


minw <- function(x) {
  attr(x, "minw")
}

lagr <- function(x, ...) {
  attr(x, "lag")
}

method <- function(y) {
  attr(y, "method")
}

iter <- function(y) {
  attr(y, "iter")
}
