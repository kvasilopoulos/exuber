#' Recursive Augmented Dickey-Fuller Test
#'
#' \code{radf} returns the recursive univariate and panel Augmented Dickey-Fuller test statistics.
#'
#' @param data A univariate or multivariate numeric time series object, a numeric
#' vector or matrix, or a data.frame. A column may have leading and/or trailing
#' \code{NA} values (an uneven/unbalanced panel where series enter or exit the
#' sample at different times) -- those periods are filled with \code{NA} in
#' \code{badf}/\code{bsadf} and excluded from that series' \code{adf}/\code{sadf}/
#' \code{gsadf}. Interior \code{NA} values (a gap in the middle of a series) are
#' not supported. When any series is padded this way, the panel statistic
#' (\code{bsadf_panel}/\code{gsadf_panel}) is not available and is returned as
#' \code{NA}, with a warning.
#' @param minw A positive integer. The minimum window size (default =
#' \eqn{(0.01 + 1.8/\sqrt(T))T}{(0.01 + 1.8 / \sqrtT)T}, where T denotes the sample size).
#' @param lag A non-negative integer. The lag length of the Augmented Dickey-Fuller regression (default = 0L).
#'
#' @details The \code{radf()} function is vectorized, i.e., it can handle multiple series
#' at once, to improve efficiency. This property also enables the computation of panel
#' statistics internally as a by-product of the univariate estimations with minimal
#' additional cost incurred.
#'
#' @return A list that contains the unit root test statistics (sequence):
#'   \item{adf}{Augmented Dickey-Fuller}
#'   \item{badf}{Backward Augmented Dickey-Fuller}
#'   \item{sadf}{Supremum Augmented Dickey-Fuller}
#'   \item{bsadf}{Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf}{Generalized Supremum Augmented Dickey-Fuller}
#'   \item{bsadf_panel}{Panel Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf_panel}{Panel Generalized Supremum Augmented Dickey-Fuller}
#'
#' And attributes:
#'   \item{mat}{The matrix used in the estimation.}
#'   \item{index}{The index parsed from the dataset.}
#'   \item{lag}{The lag used in the estimation.}
#'   \item{n}{The number of rows.}
#'   \item{minw}{The minimum window used in the estimation.}
#'   \item{series_names}{The series names.}
#'
#' @references Phillips, P. C. B., Wu, Y., & Yu, J. (2011). Explosive Behavior
#' in The 1990s Nasdaq: When Did Exuberance Escalate Asset Values? International
#' Economic Review, 52(1), 201-226.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @references Pavlidis, E., Yusupova, A., Paya, I., Peel, D., Martínez-García,
#' E., Mack, A., & Grossman, V. (2016). Episodes of exuberance in housing markets:
#' in search of the smoking gun. The Journal of Real Estate Finance and Economics,
#' 53(4), 419-449.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # We will use simulated data that are stored as data
#' sim_data
#'
#' rsim <- radf(sim_data)
#'
#' str(rsim)
#'
#' # We would also use data that contain a Date column
#' sim_data_wdate
#'
#' rsim_wdate <- radf(sim_data_wdate)
#'
#' tidy(rsim_wdate)
#'
#' augment(rsim_wdate)
#'
#' tidy(rsim_wdate, panel = TRUE)
#'
#' head(index(rsim_wdate))
#'
#' # For lag = 1 and minimum window = 20
#' rsim_20 <- radf(sim_data, minw = 20, lag = 1)
#' }
radf <- function(data, minw = NULL, lag = 0L) {
  x <- parse_data(data)
  minw <- minw %||% psy_minw(data)
  nc <- ncol(x)
  n <- nrow(x)

  ranges <- na_edges(x)
  uneven <- any(ranges["start", ] != 1L) || any(ranges["end", ] != n)
  valid_len <- ranges["end", ] - ranges["start", ] + 1L

  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)
  if (any(valid_len - lag <= minw)) {
    stop_glue(
      "Argument 'minw' should be smaller than the number of non-NA ",
      "observations minus the lag, for every series."
    )
  }

  pointer <- n - minw - lag
  snames <- colnames(x)
  adf <- sadf <- gsadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, snames)))
  badf <- bsadf <- matrix(NA_real_, pointer, nc, dimnames = list(NULL, snames))

  for (i in 1:nc) {
    start <- ranges["start", i]
    offset <- start - 1L
    yi <- x[start:ranges["end", i], i]
    yxmat <- unroot(yi, lag = lag)
    results <- rls_gsadf(yxmat, min_win = minw, lag = lag)

    pointer_i <- length(yi) - minw - lag
    badf[offset + seq_len(pointer_i), i] <- results[1:pointer_i]
    adf[i] <- results[pointer_i + 1]
    sadf[i] <- results[pointer_i + 2]
    gsadf[i] <- results[pointer_i + 3]
    bsadf[offset + seq_len(pointer_i), i] <- results[-c(1:(pointer_i + 3))]
  }

  if (uneven) {
    bsadf_panel <- rep(NA_real_, pointer)
    gsadf_panel <- NA_real_
    warning_glue(
      "the panel statistic ('bsadf_panel'/'gsadf_panel') is not available ",
      "for an uneven panel (leading/trailing NA padding detected); ",
      "returning NA."
    )
  } else {
    bsadf_panel <- apply(bsadf, 1, mean)
    gsadf_panel <- max(bsadf_panel)
  }

  list(
    adf = adf,
    badf = badf,
    sadf = sadf,
    bsadf = bsadf,
    gsadf = gsadf,
    bsadf_panel = bsadf_panel,
    gsadf_panel = gsadf_panel
  ) %>%
    add_attr(
      mat = x,
      index = index(x),
      series_names = snames,
      minw = minw,
      lag = lag,
      n = n,
      valid_range = ranges
    ) %>%
    add_class("radf_obj")
}

#' @export
print.radf_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(
    left = glue("radf (minw = {get_minw(x)}, lag = {get_lag(x)})")
  )
  cat_line()

  print(format(as.data.frame(tidy(x)),
    digits = digits
  ), print.gap = 2L, row.names = FALSE)
  cat_line()

  print(format(as.data.frame(tidy(x, panel = TRUE)),
    digits = digits
  ), print.gap = 2L, row.names = FALSE)
  cat_line()
}

#' @export
print.radf_cv <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  iter_char <- if (is_mc(x)) "nrep" else "nboot"
  lag_str <- if (is_sb(x)) paste0(", lag = ", get_lag(x)) else ""
  cat_line()
  cat_rule(
    left = glue("{get_method(x)} (minw = {get_minw(x)}, {iter_char} = {get_iter(x)}{lag_str})")
  )
  cat_line()

  print(format(as.data.frame(tidy(x)),
    digits = digits
  ), print.gap = 2L, row.names = FALSE)
  cat_line()
}
