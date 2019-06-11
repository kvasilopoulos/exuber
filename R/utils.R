
# get crit data --------------------------------------------------------

get_crit <- function(x) {
  nr <- NROW(index(x))
  if (nr > 5 && nr <= length(crit)) {
    return(get("crit")[[nr]])
  } else {
    stop("cannot provide MC critical values see help(crit)", call. = FALSE)
  }
}


# options -----------------------------------------------------------------

get_pb <- function(condition, iter, width = getOption("width") - 10L) {
  if (condition) {
    txtProgressBar(min = 1, max = iter - 1, style = 3,
                   char = "-", width = width)
  }
}

get_pb_opts <- function(condition, pb) {
  if (condition) {
    list(progress = function(n) setTxtProgressBar(pb, n))
  }else{
    list(progress = NULL)
  }
}

# tidy --------------------------------------------------------------------

array_to_list <- function(x, var) {

  itnames <- pluck(x, var) %>%
    dimnames() %>% pluck(3)
  iter <- length(itnames)

  out <- vector("list", length = iter)
  for (i in 1:iter) {
    out[[i]] <- pluck(x, var)[, , i]
  }
  out
}

add_key <- function(x, attr_minw) {
  nkey <- minw(attr_minw)
  x %>% add_column(key = (nkey + 1):(nrow(.) + nkey))
}


#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
NULL



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

min_dur <- function(y) {
  attr(y, "min_duration")
}

