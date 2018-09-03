
# Check if a character string is a Date -----------------------------------


# findDates <- function(strings) {
#   pattern1 <- "[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]"
#   pattern2 <- "[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]"
#   pattern3 <- "[0-9][0-9]/[0-9][0-9][0-9][0-9]/[0-9][0-9]"
#   pattern4 <- "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
#
#   tdBool <- grepl(pattern1, strings) | grepl(pattern2, strings) |
#     grepl(pattern3, strings) | grepl(pattern4, strings)
#   return(tdBool)
# }


# access crit data --------------------------------------------------------

provide_crit <- function(cv, x) {
  if (missing(cv)) {
    nr <- NROW(index(x))
    if (nr > 5 && nr <= 500) {
      return(get("crit")[[nr]])
    } else {
      stop("cannot provide MC critical values see ?crit", call. = FALSE)
    }
  }else{
    return(cv)
  }
}


# assert arguments ------------------------------------------------------


warning_redudant <- function(arg, cond = TRUE) {
  level <- deparse(substitute(arg))
  if (cond) {
    warning(sprintf("Argument '%s' is redundant", level),
            call. = FALSE)
  }
}

assert_positive_int <- function(arg, strictly = TRUE, greater_than = NULL) {
  level <- deparse(substitute(arg))
  if (strictly) {
    if (arg != round(arg) || arg <= 0) {
      stop(sprintf("Argument '%s' should be a positive integer", level),
           call. = FALSE)
    }
  }else{
    if (arg != round(arg) | arg < 0L) {
      stop(sprintf("Argument '%s' should be a non-negative integer",level
      ), call. = FALSE)
    }
  }
  if (!is.null(greater_than)) {
    if (arg <= greater_than) {
      stop(sprintf("Argument '%s' should be greater than '%d'",
                   level, greater_than), call. = FALSE)
    }
  }
}

assert_between <- function(x, arg1, arg2) {
  level <- deparse(substitute(x))
  if (x < arg1 | x > arg2) {
    stop(sprintf("Argument '%s' should be a be between '%d' and '%d'",
                 level, arg1, arg2), call. = FALSE)
  }
}

assert_class <- function(x, klass) {
  xstring <- deparse(substitute(x))
  klass <- deparse(substitute(klass))
  if (!inherits(x, klass)) {
    stop(sprintf("Argument '%s' should be of class '%s'", xstring, klass),
         call. = FALSE)
  }
}


# radf and cv specific ----------------------------------------------------

assert_na <- function(x) {
  if (any(is.na(x))) {
    stop("Recursive least square estimation cannot handle NA", call. = FALSE)
  }
}

# inverse of in to control for cv in panel_check
'%ni%' <- Negate('%in%')


assert_panel <- function(x, y, panel = FALSE) {
  if (method(y) %ni% c("Monte Carlo", "Sieve Bootstrap") && panel == TRUE) {
    stop("Wrong critical values", call. = FALSE)
  }
  if (method(y) == "Sieve Bootstrap" && panel == FALSE) {
    stop("Sieve Bootstrapped critical values are used for panel estimation",
         call. = FALSE)
  }
  if (method(y) == "Sieve Bootstrap") {
    if (lagr(x) != lagr(y)) {
      stop("Different lag values", call. = FALSE)
    }
  }
}

assert_equal_minw <- function(x, y) {
  if (minw(x) != minw(y)) {
    stop("The critical values should have the same minumum", "
         window with the t-statistics!", call. = FALSE)
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
