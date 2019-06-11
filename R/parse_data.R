
parse_df <- function(x) {

  date_index <- purrr::detect_index(x, lubridate::is.Date)
  if (as.logical(date_index)) {
    index <- x[, date_index, drop = TRUE]
    message(glue("Using `{colnames(x)[date_index]}` as index variable."))
    x <- x[, -date_index, drop = FALSE]
  } else {
    index <- seq(1, NROW(x), 1)
  }

  list(data = x, index = index)
}

parse_ts <- function(x) {

  sim_index <- seq(1, NROW(x), 1)
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

parse_num <- function(x) {
  list(data = x, index = seq(1, NROW(x), 1))
}

#' @importFrom stats frequency time
#' @importFrom lubridate date_decimal round_date
#' @importFrom purrr detect_index
#' @importFrom stats is.ts
parse_data <- function(x) {

  if (is.ts(x)) {
    lst <- parse_ts(x)
  } else if (is.data.frame(x)) {
    lst <- parse_df(x)
  } else if (is.numeric(x)) {
    lst <- parse_num(x)
  } else {
    stop_glue("Unsupported class")
  }

  matx <- as.matrix(lst$data)

  if (is.null(colnames(matx)))
    colnames(matx) <- paste("Series", seq(1, ncol(matx), 1))

  list(data = matx, index = lst$index)
}
