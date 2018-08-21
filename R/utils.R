
unroot <- function(x, lag) {
  if (lag == 0) {
    x_embed <- embed(x, 2)
    yxmat <- cbind(x_embed[, 1], 1, x_embed[, 2])
  } else {
    x_embed <- embed(x, lag + 2)
    dx_embed <- embed(diff(x), lag + 1)[, -1]
    x_lev <- x_embed[, 1]
    x_lag <- x_embed[, 2]
    yxmat <- cbind(x_lev, 1, x_lag, dx_embed)
  }
  return(yxmat)
}

# Check if a character string is a Date -----------------------------------


findDates <- function(strings) {
  pattern1 <- "[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]"
  pattern2 <- "[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]"
  pattern3 <- "[0-9][0-9]/[0-9][0-9][0-9][0-9]/[0-9][0-9]"
  pattern4 <- "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"

  tdBool <- grepl(pattern1, strings) | grepl(pattern2, strings) |
    grepl(pattern3, strings) | grepl(pattern4, strings)
  return(tdBool)
}

# Check arguments ------------------------------------------------------

assert_positive_int <- function(arg) {
  level <- deparse(substitute(arg))
  if (arg != round(arg) || arg <= 0L) {
    stop(sprintf(
      "Argument '%s' should be a positive integer",
      level
    ), call. = FALSE)
  }
}

assert_nonnegeative_int <- function(arg) {
  level <- deparse(substitute(arg))
  if (arg != round(arg) | arg < 0L) {
    stop(sprintf(
      "Argument '%s' should be a non-negative integer",
      level
    ), call. = FALSE)
  }
}

assert_between <- function(x, arg1, arg2) {
  level <- deparse(substitute(x))
  if (x < arg1 | x > arg2) {
    stop(sprintf(
      "Argument '%s' should be a be between '%d' and '%d'",
      level, arg1, arg2
    ), call. = FALSE)
  }
}

# radf and cv specific ----------------------------------------------------

assert_radf <- function(x) {
  if (!inherits(x, "radf")) {
    stop("Argument 'x' should be of class 'radf'", call. = FALSE)
  }
}

assert_cv <- function(y, panel) {
  if (!inherits(y, "cv")) {
    stop("Argument 'y' should be of class 'cv'", call. = FALSE)
  }
  if (method(y) == "Sieve Bootstrap" && panel != TRUE)
    stop("Sieve Bootstrapped critical values are used for panel estimation",
         call. = FALSE)
}

# inverse of in to control for cv in panel_check
'%ni%' <- Negate('%in%')

assert_panel <- function(x, y) {
  if (method(y) %ni% c("Monte Carlo", "Sieve Bootstrap")) {
    stop("Wrong critical values", call. = FALSE)
  }
  if (method(y) == "Sieve Bootstrap") {
    if (lagr(x) != lagr(y)) {
      stop("Different lag values", call. = FALSE)
    }
  }
}

assert_minw <- function(x, y) {
  if (minw(x) != minw(y)) {
    stop("The critical values should have the same minumum", "
         window with the t-statistics!", call. = FALSE)
  }
}

# Rest --------------------------------------------------------------------
# Access attributes easily

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
