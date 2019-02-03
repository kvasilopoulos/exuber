#' Recursive Augmented Dickey-Fuller Test
#'
#' \code{radf} returns the t-statistics from a recursive Augmented Dickey-Fuller
#' test.
#'
#' @param data A univariate or multivariate numeric ts object, data.frame or matrix.
#' The estimation process cannot handle NA values.
#' @param minw A positive integer. The minimum window size, which defaults to
#' \eqn{(0.01 + 1.8/\sqrt(T))*T}{(0.01 + 1.8 / \sqrtT)*T}.
#' @param lag A non-negative integer. The lag of the Augmented Dickey-Fuller regression.
#'
#' @return A list that contains the t-statistic (sequence) for:
#'   \item{adf}{Augmented Dickey-Fuller}
#'   \item{badf}{Backward Augmented Dickey-Fuller}
#'   \item{sadf}{Supremum Augmented Dickey-Fuller}
#'   \item{bsadf}{Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf}{Generalized Supremum Augmented Dickey-Fuller}
#'
#' @references Phillips, P. C. B., Wu, Y., & Yu, J. (2011). Explosive Behavior
#' in The 1990s Nasdaq: When Did Exuberance Escalate Asset Values? International
#' Economic Review, 52(1), 201-226.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @importFrom stats embed frequency time
#' @importFrom lubridate date_decimal round_date
#' @importFrom purrr detect_index
#' @export
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100))
#'
#' rfd <- radf(dta)
#'
#' # For lag = 1 and minimum window = 20
#' rfd <- radf(dta, minw = 20, lag = 1)
#' }
radf <- function(data, minw, lag = 0) {

  # class checks
  sim_index <- seq(1, NROW(data), 1)
  if (any(class(data) %in% c("mts", "ts"))) {
    if (all(time(data) == sim_index)) {
      dating <- sim_index
    }else{
      dating <- time(data) %>%
        as.numeric() %>%
        date_decimal()
      if (frequency(data) %in% c(1, 4, 12)) {
        dating <- dating %>%
          round_date("month") %>%
          as.Date()
      }else if (frequency(data) == 52) {
        dating <- dating %>%
          as.Date()
      }else{
        dating <- dating %>%
          round_date("day") %>%
          as.Date()
      }
    }
  } else if (is.data.frame(data)) {
    date_index <- purrr::detect_index(data, lubridate::is.Date)
    if (as.logical(date_index)) {
      dating <- data[, date_index, drop = TRUE]
      data <- data[, -date_index, drop = FALSE]
    } else {
      dating <- sim_index
    }
  } else if (class(data) %in% c("numeric", "matrix")) {
    dating <- sim_index
  } else {
    stop("Unsupported class", call. = FALSE)
  }
  x <- as.matrix(data)
  nc <- NCOL(data)
  nr <- NROW(data)
  # args
  if (is.null(colnames(x))) colnames(x) <- paste("Series", seq(1, nc, 1))
  if (missing(minw)) minw <-  floor((0.01 + 1.8 / sqrt(nr)) * nr)
  # checks
  assert_na(data)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  point <- nr - minw - lag

  adf <- sadf <- gsadf <- drop(matrix(0, 1, nc,
                                     dimnames = list(NULL, colnames(x))))
  badf <- bsadf <- matrix(0, point, nc, dimnames = list(NULL, colnames(x)))

  for (i in 1:nc) {

    yxmat <- unroot(x[, i], lag = lag)
    results <- rls_gsadf(yxmat, min_win = minw, lag = lag)

    badf[, i] <- results[1:point]
    adf[i]  <- results[point + 1]
    sadf[i] <- results[point + 2]
    gsadf[i] <- results[point + 3]
    bsadf[, i] <- results[-c(1:(point + 3))]
  }

  bsadf_panel <- apply(bsadf, 1, mean)
  gsadf_panel <- max(bsadf_panel)

  value <- structure(list(adf = adf,
                          badf = badf,
                          sadf = sadf,
                          bsadf = bsadf,
                          gsadf = gsadf,
                          bsadf_panel = bsadf_panel,
                          gsadf_panel = gsadf_panel),
                     index = dating,
                     lag = lag,
                     minw = minw,
                     lag = lag,
                     col_names = colnames(x),
                     class = "radf")

  return(value)
}

unroot <- function(x, lag = 0) {
  if (lag == 0) {
    x_embed <- embed(x, 2)
    yxmat <- cbind(x_embed[, 1], x_embed[, 2])
  } else {
    x_embed <- embed(x, lag + 2)
    dx_embed <- embed(diff(x), lag + 1)[, -1]
    x_lev <- x_embed[, 1]
    x_lag <- x_embed[, 2]
    yxmat <- cbind(x_lev, 1, x_lag, dx_embed)
  }
  return(yxmat)
}
