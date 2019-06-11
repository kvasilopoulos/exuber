
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
    call. = call., domain = .domain
  )
}

"%ni%" <- Negate("%in%")

# predicates --------------------------------------------------------------

#' @importFrom rlang is_bare_numeric
is_n <- function(x) {
  is_scalar_atomic(x) && is_bare_numeric(x) && x == trunc(x) && x > 0
}

is_panel <- function(y) {
  attr(y, "panel")
}

is_panel_cv <- function(y) {
  assert_class(y, "cv")
  res <- if (method(y) == "Sieve Bootstrap") TRUE else FALSE
  res
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


assert_equal_arg <- function(x, y, panel = FALSE) {
  if (minw(x) != minw(y)) stop("Different minimum window", call. = FALSE)

  if (method(y) == "Sieve Bootstrap") {
    if (lagr(x) != lagr(y)) stop("Different lag values", call. = FALSE)
  }
}
