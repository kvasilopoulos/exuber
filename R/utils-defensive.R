
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

# predicates --------------------------------------------------------------

#' @importFrom rlang is_bare_numeric
is_n <- function(x) {
  is_scalar_atomic(x) && is_bare_numeric(x) && x == trunc(x) && x > 0
}

is_identical <- function(x, y) {
  if (identical(x, y)) TRUE else FALSE
}

is_mc <- function(y) {
  # assert_class(y, "cv")
  if (get_method(y) == "Monte Carlo") TRUE else FALSE
}

is_wb <- function(y) {
  # assert_class(y, "cv")
  if (get_method(y) == "Wild Bootstrap") TRUE else FALSE
}

is_sb <- function(y) {
  # assert_class(y, "cv")
  if (get_method(y) == "Sieve Bootstrap") TRUE else FALSE
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
  if (!inherits(x, klass)) {
    stop(sprintf("Argument '%s' should be of class '%s'", xstring, klass),
         call. = FALSE
    )
  }
}


assert_na <- function(x) {
  if (any(is.na(x))) {
    stop_glue("rls estimation cannot handle NA")
  }
}

assert_same_data <- function(x, y) {
  attr_x <- attributes(x)
  attr_y <- attributes(y)

  is_identical(attr_x$n, attr_y$n)
  is_identical(attr_x$index, attr_y$index)
}


assert_equal_arg <- function(x, y, panel = FALSE) {
  if (get_minw(x) != get_minw(y))
    stop_glue("Different minimum window")

  if (get_method(y) == "Sieve Bootstrap") {
    if (get_lag(x) != get_lag(y))
      stop_glue("Different lag values")
  }
}
