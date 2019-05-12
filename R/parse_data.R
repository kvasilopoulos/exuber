
parse_df <- function(x) {

  date_index <- purrr::detect_index(x, lubridate::is.Date)
  if (as.logical(date_index)) {
    index <- x[, date_index, drop = TRUE]
    x <- x[, -date_index, drop = FALSE]
  } else {
    index <- seq(1, NROW(x), 1)
  }

  return(
    list(data = x, index = index)
  )
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

  return(
    list(data = x, index = index)
  )
}

#' @importFrom stats frequency time
#' @importFrom lubridate date_decimal round_date
#' @importFrom purrr detect_index
#' @importFrom stats is.ts
parse_data <- function(x) {

  tryCatch(
    if (is.null(colnames(x)))
      colnames(x) <- paste("Series", seq(1, NCOL(x), 1)),
    error = function(e) {}
  )

  if (is.ts(x)) {
    lst <- parse_ts(x)
  } else if (is.data.frame(x)) {
    lst <- parse_df(x)
  } else if (is.numeric(x)) {
    lst <- list(data = x, index = seq(1, NROW(x), 1))
  } else {
    stop("Unsupported class", call. = FALSE)
  }
  return(lst)
}
