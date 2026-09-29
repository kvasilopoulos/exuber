# Wu, Shi & Wu (2025, JTSA 46(5), "Quantile analysis for financial bubble
# detection and surveillance", "WSW") -- the QPWY recursive
# monitoring strategy of their Section 3.2 (eq. 25, 28). See
# docs/alternative-paradigms.md, "Quantile-based detection", for the full
# evaluation this implements.
#
# QPWY_r(tau) := t_T^{0,r}(tau), the quantile-regression t-ratio on the
# EXPANDING window [1, r] (radf()'s own badf shape). The point statistic
# needs O(T) genuine QR fits (no closed-form recursive update the way OLS
# has). The paper's QPSY (sup over window starts too, O(T^2) fits) is not
# implemented.
#
# Critical values come from Theorem 1 / Corollary 1-2: under the null,
# t_T^{r1,r2}(tau) => int W~ dB_psi / sqrt(int W~^2), with B_psi a
# Brownian motion correlated delta(tau) with W. Writing B_psi = delta*W +
# sqrt(1-delta^2)*V (V independent of W) gives
#   U_{r1,r2} = delta*Q_{r1,r2} + sqrt(1-delta^2)*Z_{r1,r2},
#   Q = int W~ dW / sqrt(int W~^2),  Z = int W~ dV / sqrt(int W~^2).
# Z_{r1,r2} is N(0,1) for any ONE window (the "z" of Corollary 1, all
# quantile_test() needs) but varies across windows. A monitoring boundary
# is a functional of the whole path, so Z must be simulated as a process
# -- the original QPWY boundary drew one z per replicate and reused it for
# every r, which understates sup_r U and oversizes the test (7% at
# delta = 0.8, 12% at 0.5, 22% at 0.2 for a nominal 5%, n = 200; found
# 2026-09-29, see docs/alternative-paradigms.md). quantile_boundary_sim()
# simulates discretized Q and Z for every window via prefix sums, O(1) per
# window, no QR fits and no radf() call.

# QR t-ratio on one window `yy` (WSW's QUr statistic, eq. 18 / Section
# 3.2): density estimated from the window's own first differences.
quantile_window_stat <- function(yy, tau) {
  m <- length(yy)
  ylag <- yy[-m]
  yresp <- yy[-1L]
  alpha_hat <- quantreg::rq.fit(cbind(1, ylag), yresp, tau = tau, method = "br")$coefficients[2L]
  f_hat <- quantile_check_density(yresp - ylag, tau)$f_hat
  yPzy <- sum((ylag - mean(ylag))^2)
  unname((f_hat / sqrt(tau * (1 - tau))) * sqrt(yPzy) * (alpha_hat - 1))
}

# QPWY_r(tau) for every window-end r in `r_idx` -- window fixed at [1, r].
qpwy_stat_path <- function(y, tau, r_idx) {
  vapply(r_idx, function(r) quantile_window_stat(y[1:r], tau), numeric(1))
}

# Simulated null path suprema, one column per entry of `delta`: for each
# replicate, sup over the monitoring path of U = delta*Q + sqrt(1-delta^2)*Z
# (see header). Windows are over the n - 1 regression pairs
# (y_{t-1}, Delta y_t); window (lo, hi] of pairs <-> y[(lo + 1):(hi + 1)],
# so QPWY's window [1, r] is (0, r - 1]. `lo` is kept general for a
# double-recursion boundary.
quantile_boundary_sim <- function(n, minw, nrep, delta, seed = NULL) {
  set_rng(seed)
  np <- n - 1L
  hi <- minw:np
  lo <- 0L
  valid <- outer(lo, hi, function(l, h) h - l >= minw)
  mk <- function(v) c(0, cumsum(v))
  win <- function(cs) outer(lo, hi, function(l, h) cs[h + 1L] - cs[l + 1L])
  L <- outer(lo, hi, function(l, h) h - l)
  L[!valid] <- NA
  out <- matrix(NA_real_, nrep, length(delta))
  for (i in seq_len(nrep)) {
    e <- stats::rnorm(np)
    v <- stats::rnorm(np)
    x <- c(0, cumsum(e))[1:np] # y_{t-1}
    Sx <- win(mk(x))
    sxx <- win(mk(x^2)) - Sx^2 / L
    Q <- (win(mk(x * e)) - Sx * win(mk(e)) / L) / sqrt(sxx)
    Z <- (win(mk(x * v)) - Sx * win(mk(v)) / L) / sqrt(sxx)
    for (j in seq_along(delta)) {
      U <- delta[j] * Q + sqrt(1 - delta[j]^2) * Z
      out[i, j] <- max(U[valid])
    }
  }
  out
}

#' QPWY Recursive Quantile Monitoring (Wu, Shi & Wu 2025)
#'
#' \code{monitor_quantile} implements the QPWY real-time monitoring strategy of
#' Wu, Shi & Wu (2025): a quantile-regression (QR) analogue of PWY's own
#' recursive ADF t-statistic, testing at a chosen conditional quantile
#' \code{tau} over an expanding window \code{[1, r]} (start fixed at the
#' beginning of the sample, exactly \code{\link{radf}}'s own \code{badf}
#' convention) rather than \code{\link{quantile_test}}'s single
#' full-sample test.
#'
#' Only \code{QPWY} (single recursion) is implemented, not the paper's
#' own \code{QPSY} (double recursion, additionally optimizing over the
#' window start): \code{QPWY_r(tau)} needs \code{O(T)} actual quantile
#' -regression fits (no closed-form recursive update the way OLS has),
#' tractable at the same cost order as \code{radf()}'s own \code{badf};
#' \code{QPSY} needs \code{O(T^2)} such fits.
#'
#' The critical value is simulated per call from the limiting null
#' distribution \code{delta * Q_{r1,r2} + sqrt(1 - delta^2) * Z_{r1,r2}},
#' with \code{delta} a data-estimated correlation coefficient (as in
#' \code{\link{quantile_test}}), \code{Q} the Dickey-Fuller t functional
#' and \code{Z} its counterpart driven by an independent Brownian motion,
#' both simulated for every window (no QR fits needed). A single
#' \strong{flat} boundary is used (not one value per \code{r}): the
#' quantile of each simulated path's own supremum, exactly how
#' \code{\link{radf_mc_cv}}'s own \code{sadf_cv} is constructed, which
#' controls the first-crossing false-alarm rate.
#'
#' @note The critical value (boundary) is simulated internally on every
#' call (via an unexported helper, \code{quantile_boundary_sim}) -- there is
#' currently no reusable/exported cv counterpart for this function (a
#' known, separately-tracked gap, not addressed here).
#'
#' @inheritParams radf
#' @param tau Quantile to test at, in \code{(0, 1)} (fixed, unlike
#' \code{\link{quantile_test}}'s \code{"optimal"} grid search -- WSW's own
#' eq. 25 takes \code{tau} as a given parameter for the monitoring
#' statistic, not re-selected at each recursion point).
#' @param nrep Number of Monte Carlo replications for the boundary.
#' @param sig_lvl Significance level, one of \code{90}, \code{95}, \code{99}.
#' @param seed Optional seed for the Monte Carlo draws.
#'
#' @return An object of class \code{monitor_quantile_obj}: a list with the
#' statistic path \code{stat}, the (flat) \code{boundary}, the estimated
#' \code{delta}, and \code{alarm}/\code{alarm_date} (the first breach,
#' \code{NA} if none).
#'
#' @references Wu, R., Shi, S., & Wu, J. (2025). Quantile analysis for
#' financial bubble detection and surveillance. Journal of Time Series
#' Analysis, 46(5), 908-931.
#'
#' @seealso \code{\link{quantile_test}} for the static, full-sample
#' version of this test. \code{\link{monitor}} for the OLS-based
#' monitoring alternative.
#'
#' @note Returns its own class (not `radf_obj`), so it does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`; it has its own `print()` and
#' `autoplot()` methods instead. Prints its own
#' statistic/boundary/delta summary -- see
#' `vignette("naming-and-analysis", package = "exuber")` for the full
#' picture of which functions do and don't fit that pipeline.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' # Heavy-tailed (t3) innovations, explosive from t = 150 to the sample end
#' y <- sim_psy1(n = 200, te = 150, tf = 200, seed = 7,
#'   e = sim_innov(199, dist = "t", df = 3))
#' res <- monitor_quantile(y, tau = 0.5, nrep = 100, seed = 1)
#' print(res)
#' autoplot(res)
#'
#' # Upper-quantile monitoring is typically more powerful for right-tailed bubbles
#' autoplot(monitor_quantile(y, tau = 0.9, nrep = 100, seed = 1))
#' }
#'
#' @family monitoring
#' @export
monitor_quantile <- function(data, tau = 0.5, minw = NULL, nrep = 500L, sig_lvl = 95, seed = NULL) {
  stopifnot(tau > 0 && tau < 1)
  assert_sig_lvl(sig_lvl)
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)
  r_idx <- (minw + 1L):n

  stat_path <- matrix(NA_real_, length(r_idx), nc, dimnames = list(NULL, snames))
  delta <- setNames(rep(NA_real_, nc), snames)
  alarm <- setNames(rep(NA_integer_, nc), snames)

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    stat_path[, j] <- qpwy_stat_path(y, tau, r_idx)
    dy_full <- diff(y)
    psi <- tau - as.numeric(dy_full < quantile_narm(dy_full, probs = tau, names = FALSE))
    delta[j] <- max(min(stats::cor(dy_full, psi), 1), -1)
  }

  sup_U <- quantile_boundary_sim(n, minw, nrep, delta, seed = seed)
  boundary <- setNames(apply(sup_U, 2, quantile_narm, probs = sig_lvl / 100, names = FALSE), snames)
  for (j in seq_len(nc)) {
    breach <- which(stat_path[, j] > boundary[j])
    if (length(breach) > 0L) alarm[j] <- r_idx[breach[1L]]
  }

  alarm_date <- vapply(alarm, function(i) {
    if (is.na(i)) NA_character_ else as.character(idx[i])
  }, character(1))

  list(
    stat = stat_path, boundary = boundary, delta = delta,
    alarm = alarm, alarm_date = alarm_date
  ) %>%
    add_attr(
      index = idx, series_names = snames, n = n, minw = minw,
      tau = tau, sig_lvl = sig_lvl, iter = nrep
    ) %>%
    add_class("monitor_quantile_obj")
}

#' Plot method for monitor_quantile() output
#'
#' Plots the quantile monitoring statistic against its boundary, one panel per series, with a vertical marker at the alarm date.
#'
#' @param object An object of class \code{monitor_quantile_obj}, the output of \code{\link{monitor_quantile}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{monitor_quantile}}
#' @export
autoplot.monitor_quantile_obj <- function(object, ...) {
  minw <- attr(object, "minw")
  pos <- (minw + 1L):(minw + nrow(object$stat))
  snames <- colnames(object$stat)
  vlines <- tibble(id = names(object$alarm), label = "alarm", at = object$alarm) %>% tidyr::drop_na(at)
  autoplot_stat_boundary(pos, object$stat, object$boundary, vlines = vlines, ylab = "QPWY statistic")
}

#' @export
print.monitor_quantile_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "monitor_quantile (n = {attr(x, 'n')}, minw = {attr(x, 'minw')}, ",
    "tau = {attr(x, 'tau')}, sig_lvl = {attr(x, 'sig_lvl')}%)"
  ))
  cat_line()
  print(
    data.frame(
      series = names(x$alarm), delta = round(x$delta, 3),
      boundary = round(x$boundary, 3),
      alarm = x$alarm, alarm_date = x$alarm_date,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
