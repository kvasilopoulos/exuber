
idx_seq <- function(x) {
  seq(1, NROW(x), 1)
}

is.index <- function(x) {
  lubridate::is.Date(x) || is.POSIXt(x)
}

parse_dt <- function(x) {
  UseMethod("parse_dt")
}

parse_dt.default <- function(x) {
  stop_glue("unsupported class")
}

parse_dt.data.frame <- function(x) {
  date_index <- vapply(x, is.index, logical(1))
  n_index <- sum(date_index, na.rm = TRUE)
  if(n_index > 1) {
    stop_glue("The `index` match to multiple variables.")
  }
  if (n_index == 1) {
    num_index <- which(date_index)
    index <- x[, num_index, drop = TRUE]
    message_glue("Using `{colnames(x)[num_index]}` as index variable.")
    x <- x[, -num_index, drop = FALSE]
  } else {
    index <- idx_seq(x)
  }
  list(data = x, index = index)
}

parse_dt.ts <- function(x) {
  sim_index <- idx_seq(x)
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
      #empty no further modification
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

is_wide <- function(index) {
  if(is_duplicate(index)) {
    return(FALSE)
  }
  TRUE
}
is_duplicate <- function(x) {
  any(duplicated(x))
}


#' @importFrom stats frequency time
#' @importFrom lubridate date_decimal round_date is.Date is.POSIXt
#' @importFrom purrr detect_index
#' @importFrom stats is.ts
parse_data <- function(x) {
  lst <- parse_dt(x)
  if(!is_wide(lst$index)) {
    stop_glue("The data do not have the appropriate format.")
  }
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
