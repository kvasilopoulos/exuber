
# defensive programming ---------------------------------------------------


warning_glue <- function(..., .sep = "", .envir = parent.frame(),
                         call. = FALSE, .domain = NULL) {
  warning(
    glue(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

stop_glue <- function(..., .sep = "", .envir = parent.frame(),
                      call. = FALSE, .domain = NULL) {
  stop(
    glue(..., .sep = .sep, .envir = .envir),
    call. = call., domain = .domain
  )
}

message_glue <-  function(..., .sep = "", .envir = parent.frame(),
                          call. = FALSE, .domain = NULL) {
  message(
    glue(..., .sep = .sep, .envir = .envir),
    domain = .domain
  )
}

# asserts ------ ------------------------------------------------------

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
      stop(sprintf("Argument '%s' should be a non-negative integer", level),
           call. = FALSE)
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
  xname <- enexpr(x)
  if (!dplyr::between(x, arg1, arg2)) {
    stop_glue("Argument '{xname}' should be a be between '{arg1}' and '{arg2}'")
  }
}

#'@importFrom rlang enexpr
assert_class <- function(x, klass) {
  quas <- enexpr(x)
  if (!inherits(x, klass)) {
    stop_glue("Argument '{quas}' should be of class '{klass}'")
  }
}

assert_na <- function(x) {
  if (any(is.na(x))) {
    stop_glue("rls estimation cannot handle NA")
  }
}

# quantile(), but NA/NaN inputs (e.g. a zero-run in the data causing
# division by zero in the underlying statistic) are dropped instead of
# propagating to a NA critical value -- with a warning naming how many
# replicates were lost, so a degenerate simulation stays visible instead of
# silently returning a critical value estimated from a handful of survivors.
quantile_narm <- function(x, probs, ...) {
  n_bad <- sum(is.na(x))
  if (n_bad > 0) {
    warning_glue(
      "{n_bad} of {length(x)} replicate(s) produced NA/NaN and were dropped ",
      "before computing the critical value; treat the result with caution ",
      "if this is a large share of replicates."
    )
  }
  stats::quantile(x, probs = probs, na.rm = TRUE, ...)
}


# Model matching ----------------------------------------------------------

assert_n <- function(x) {
  if (!is_n(x)) { # case of providiing data in 'n'
    stop_glue("Argument 'n' should be a positive integer")
  }
}

assert_match <- function(x, y, panel = FALSE) {
  attr_x <- attributes(x)
  attr_y <- attributes(y)
  if (attr_x$minw != attr_y$minw) {
    stop_glue("minimum window does not match")
  }
  if (attr_x$n != attr_y$n) {
    stop_glue("sample size does not match")
  }
  if (is_sb(y)) {
    if (attr_x$lag != attr_y$lag)
      stop_glue("lag value does not match")
  }
}

# assert_model <- function(x, y) {
#   assert_class(x, "obj") # is this necessay since s3
#   assert_class(y, "cv")
#   assert_match(x, y)
# }

# predicates --------------------------------------------------------------

#' @importFrom rlang is_bare_numeric
is_n <- function(x) {
  is_scalar_atomic(x) && is_bare_numeric(x) && x == trunc(x) && x > 0
}

is_identical <- function(x, y) {
  if (identical(x, y)) TRUE else FALSE
}

