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
#'   \item{bsadf_panel}{Panel Backward Supremum Augmented Dickey-Fuller}
#'   \item{gsadf_panle}{Panel Generalized Supremum Augmented Dickey-Fuller}
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
#' # We will use simulated data that are stored as data with the name `sim_data`
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
#' glance(rsim_wdate)
#'
#' head(index(rsim_wdate))
#'
#' # For lag = 1 and minimum window = 20
#' rsim_20 <- radf(sim_data, minw = 20, lag = 1)
#'}
radf <- function(data, minw = NULL, lag = 0L) {

  x <- parse_data(data)
  minw <- minw %||% psy_minw(data)
  nc <- ncol(x)

  assert_na(x)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  pointer <- nrow(x) - minw - lag
  snames <- colnames(x)
  adf <- sadf <- gsadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, snames)))
  badf <- bsadf <- matrix(0, pointer, nc, dimnames = list(NULL, snames))

  for (i in 1:nc) {
    yxmat <- unroot(x[, i], lag = lag)
    results <- rls_gsadf(yxmat, min_win = minw, lag = lag)

    badf[, i]  <- results[1:pointer]
    adf[i]     <- results[pointer + 1]
    sadf[i]    <- results[pointer + 2]
    gsadf[i]   <- results[pointer + 3]
    bsadf[, i] <- results[-c(1:(pointer + 3))]
  }

  bsadf_panel <- apply(bsadf, 1, mean)
  gsadf_panel <- max(bsadf_panel)


  list(
    adf = adf,
    badf = badf,
    sadf = sadf,
    bsadf = bsadf,
    gsadf = gsadf,
    bsadf_panel = bsadf_panel,
    gsadf_panel = gsadf_panel) %>%
    add_attr(
      index = attr(x, "index"),
      lag = lag,
      n = nrow(x),
      minw = minw,
      lag = lag,
      series_names = snames,
    ) %>%
    add_class("radf_obj", "obj")
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


