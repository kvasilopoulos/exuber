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
#' @export
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(dta)
#'
#' # For lag = 1 and minimum window = 20
#' rfd <- radf(dta, minw = 20, lag = 1)
#' }
radf <- function(data, minw = psy_minw(data), lag = 0) {

  lst <- parse_data(data)
  x <- lst$data

  nc <- ncol(x)

  assert_na(x)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  point <- nrow(x) - minw - lag

  adf <- sadf <- gsadf <-
    drop(matrix(0, 1, nc, dimnames = list(NULL, colnames(x))))
  badf <- bsadf <-
    matrix(0, point, nc, dimnames = list(NULL, colnames(x)))

  for (i in 1:nc) {
    yxmat <- unroot(x[, i], lag = lag)
    results <- rls_gsadf(yxmat, min_win = minw, lag = lag)

    badf[, i]  <- results[1:point]
    adf[i]     <- results[point + 1]
    sadf[i]    <- results[point + 2]
    gsadf[i]   <- results[point + 3]
    bsadf[, i] <- results[-c(1:(point + 3))]
  }

  bsadf_panel <- apply(bsadf, 1, mean)
  gsadf_panel <- max(bsadf_panel)

  structure(
    list(
      adf = adf,
      badf = badf,
      sadf = sadf,
      bsadf = bsadf,
      gsadf = gsadf,
      bsadf_panel = bsadf_panel,
      gsadf_panel = gsadf_panel),
    index = lst$index,
    lag = lag,
    minw = minw,
    lag = lag,
    col_names = colnames(x),
    class = "radf"
  )

}

#' Helper functions in accordance to PSY(2015)
#'
#' \code{psy_minw} proposed a minimum window. \code{psy_ds} proposed a rule of
#' thumb to exclude periods of exuberance.
#'
#' @inheritParams mc_cv
#' @export
#' @importFrom rlang is_scalar_atomic
#' @examples
#' psy_minw(100)
#' psy_ds(100)
psy_minw <- function(n) {

  if (!is_n(n)) {
    n <- NROW(n)
  }

  floor((0.01 + 1.8 / sqrt(n)) * n)
}

#' @rdname psy_minw
psy_ds <- function(n, rule = 1, delta = 1) {

  if (!is_n(n)) {
    n <- NROW(n)
  }
  stopifnot(x == 1 || x == 2)

  if (rule == 1) {
    round(delta * log(n))
  } else if (rule == 2) {
    round(delta*log(n)/n)
  }

}

#' @importFrom stats embed
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
