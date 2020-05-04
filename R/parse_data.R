
idx_seq <- function(x) {
  seq(1, NROW(x), 1)
}

parse_dt <- function(x) {
  UseMethod("parse_dt")
}

parse_dt.default <- function(x) {
  stop_glue("unsupported class")
}

parse_dt.data.frame <- function(x) {
  date_index <- purrr::detect_index(x, lubridate::is.Date)
  if (as.logical(date_index)) {
    index <- x[, date_index, drop = TRUE]
    message(glue("Using `{colnames(x)[date_index]}` as index variable."))
    x <- x[, -date_index, drop = FALSE]
  } else {
    index <- idx_seq(x)
  }
  list(data = x, index = index)
}

parse_dt.ts <- function(x) {
  sim_index <- seq(1, NROW(x), 1)
  vec_time <- as.vector(time(x))
  if (identical(time(x), sim_index)) {
    index <- sim_index
  } else {
    index <- time(x) %>%
      as.numeric() %>%
      date_decimal()
    if (frequency(x) %in% c(1, 4, 12)) {
      index <- round_date(index, "month")
    } else if (frequency(x) == 52) {
    } else {
      index <- round_date(index, "day")
    }
    index <- as.Date(index)
  }
  list(data = x, index = index)
}

parse_dt.numeric <- function(x) {
  list(data = x, index = idx_seq(x))
}


#' @importFrom stats frequency time
#' @importFrom lubridate date_decimal round_date
#' @importFrom purrr detect_index
#' @importFrom stats is.ts
parse_data <- function(x) {
  lst <- parse_dt(x)
  matx <- as.matrix(lst$data)
  if (is.character(matx)) {
    stop_glue("non-numeric argument to data argument.")
  }
  if (is.null(colnames(matx))) {
    colnames(matx) <- paste0("series", seq(1, ncol(matx), 1))
  }
  matx %>%
    add_attr(index = lst$index) %>%
    add_attr(series_names = colnames(matx))
}
