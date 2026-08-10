set_attrs <- function(x, ...) {
  attrs <- dots_list(...)
  attributes(x) <- attrs
  x
}

#'@importFrom rlang dots_list
add_attr <- function(x,  ...) {
  attrs <- dots_list(...)
  attributes(x) <- c(attributes(x), attrs)
  x
}

inherit_attrs <- function(x, y) {

  attr_x <- attributes(x) %>% names() %||% NA_character_
  attr_y <- attributes(y) %>% names() %||% NA_character_

  remove_x <- which(attr_x %in% attr_y)
  attributes(y)[remove_x] <- NULL # remove duplicates

  attributes(x) <- c(attributes(x), attributes(y))
  x
}

set_class <- function(x, nm) {
  class(x) <- nm
  x
}

add_class <- function(x, ...) {
  class(x) <- append(c(...), class(x))
  x
}

# Access attributes easily ------------------------------------------------


get_trunc <- function(x) {
  has_lag <- !is.null(get_lag(x))
  if (is_sb(x) && get_lag(x) != 0) {
    get_minw(x) + get_lag(x) + 2
  }else if (has_lag){
    get_minw(x) + get_lag(x)
  }else{
    get_minw(x)
  }
}

get_minw <- function(x) {
  attr(x, "minw")
}

get_lag <- function(x, ...) {
  attr(x, "lag")
}

get_n <- function(x) {
  attr(x, "n")
}

get_method <- function(y) {
  attr(y, "method")
}

get_iter <- function(y) {
  attr(y, "iter")
}

get_min_dur <- function(y) {
  attr(y, "min_duration")
}

get_panel <- function(y) {
  attr(y, "panel")
}

get_caveat <- function(x) {
  attr(x, "caveat")
}

# Functions whose validation/source status isn't a clean "clean" (a
# preprint source, a not-fully-resolved validation gap, ...) set a
# `caveat` attribute (add_attr(..., caveat = ...)) and emit the same text
# via message_glue() at call time -- one string, not two copies to keep in
# sync. print.*_obj methods call this to surface it; the roxygen
# `@section Caveats:` on the function itself remains the long-form doc.
#' @importFrom cli cat_line col_yellow symbol
cat_caveat <- function(x) {
  caveat <- get_caveat(x)
  if (is.null(caveat)) {
    return(invisible())
  }
  cli::cat_line(cli::col_yellow(cli::symbol$info), " ", caveat)
  cli::cat_line()
}



