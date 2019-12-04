
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

#' @importFrom rlang %||%
is_mc <- function(y) {
  get_method(y) %||% FALSE == "Monte Carlo"
}

is_wb <- function(y) {
  get_method(y) %||% FALSE == "Wild Bootstrap"

}

is_sb <- function(y) {
  get_method(y) %||% FALSE == "Sieve Bootstrap"
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
  if (!dplyr::between(x, arg1, arg2)) {
    stop_glue("Argument '{x}' should be a be between '{arg1}' and '{arg2}'")
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
