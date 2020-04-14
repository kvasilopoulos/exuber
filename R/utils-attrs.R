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


get_minw <- function(x) {
  attr(x, "minw")
}

get_lag <- function(x, ...) {
  attr(x, "lag")
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


