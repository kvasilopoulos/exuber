# Kurozumi, E. & Nishi, M. (2025, JTSA 46(5), 945-965, "Bubble testing
# with stochastically varying explosive coefficient"; "KN"). See
# docs/enhancements/volatility-robustness.md, "Stochastic explosive
# -coefficient test", for the full evaluation this implements.
#
# Only the SSU statistic (their eq. 7, sup-type, r1 fixed at 0) is
# implemented -- the minimum-viable subset this project's own earlier
# triage identified ("SSU alone, without GSSU's double-recursion,
# without CUSUM/CUSUM-SQ, without the union"). Re-triaged 2026-08-10:
# the original "not a contained addition" verdict undersold it on two
# fronts, confirmed by re-reading rendered pages 5-6, 9 directly (not
# the raw text extraction):
#
# 1. KN's own Table I publishes SSU's critical values directly (2.90/
#    3.30/4.20 at the 10%/5%/1% level, from their own 10,000-rep Monte
#    Carlo) -- no new simulation needed, the same published-table
#    shortcut used for Kurozumi (2020)'s and HB's own boundaries
#    elsewhere in this project. SSU's own r0 = 0.01 + 1.8/sqrt(T) is
#    *exactly* exuber's existing psy_minw() formula, reused directly.
# 2. The bias-corrected statistic t^{omega,c} (eq. after their Remark 1,
#    page 6) looks like it needs "the residuals from two fitted
#    regressions per window" (the original assessment's stated blocker),
#    but the cross-moment sigma_hat_{epsilon*eta} it needs is a BILINEAR
#    expansion of the two regressions' fitted coefficients against a
#    fixed set of window sums -- more cumulative sums to track than any
#    prior item in this project, but still O(1) per window and requiring
#    no new estimation machinery beyond what hls_prefix_sums()'s own
#    generic (x, z)-over-a-segment closed form already established.
#
# Model: (6) is the plain ADF regression Delta y_t = mu1 + delta*y_{t-1}
# + e_t (exactly radf()'s own construction); (7) is the "stochastic unit
# root" regression on squares, (Delta y_t)^2 = mu2 + omega*y_{t-1}^2 +
# eta_t (Lee 1998/Nagakura 2009), testing omega for a bubble in the
# *variance* of the increments rather than the level. The raw t-stat on
# omega is not asymptotically pivotal (its limit depends on the
# correlation between the two regressions' innovations); the correction
# in t^{omega,c} removes that dependence.

# All cumulative sums SSU's closed form needs, built from two base
# per-observation series: x1 = y_{t-1} (level lag) and d1 = Delta y_t
# (difference). Every term in the ADF regression (6), the SSU regression
# (7), and the eq.-after-Remark-1 cross-moment correction reduces to a
# window sum of one of these twelve products.
ssu_prefix_sums <- function(y) {
  n1 <- length(y) - 1L
  x1 <- y[1:n1]
  d1 <- y[2:(n1 + 1L)] - x1

  mk <- function(v) c(0, cumsum(v))
  list(
    n1 = n1,
    x1 = mk(x1), x1_2 = mk(x1^2), x1_3 = mk(x1^3), x1_4 = mk(x1^4),
    d1 = mk(d1), d1_2 = mk(d1^2), d1_3 = mk(d1^3), d1_4 = mk(d1^4),
    d1x1 = mk(d1 * x1),
    d1_2x1_2 = mk(d1^2 * x1^2),
    d1x1_2 = mk(d1 * x1^2),
    x1d1_2 = mk(x1 * d1^2)
  )
}

# t^{omega,c}_{0, r2} (SSU's own r1 = 0, fixed) for every candidate r2 =
# hi in `hi_idx` (i-index terms, matching hls_segment_ssr()'s (lo, hi]
# convention with lo = 0 throughout, since SSU is a single-recursion sup
# statistic like SADF/badf, not a double recursion like GSADF/bsadf).
ssu_stat_path <- function(ps, hi_idx) {
  S <- function(nm) ps[[nm]][hi_idx + 1L] # window (0, hi] sum, lo = 0
  L <- hi_idx

  # Regression 6: Delta y_t = mu1 + delta*y_{t-1} + e_t.
  Sx1 <- S("x1")
  Sx1x1 <- S("x1_2")
  Sd1 <- S("d1")
  Sd1x1 <- S("d1x1")
  Sd1d1 <- S("d1_2")
  delta_hat <- (L * Sd1x1 - Sx1 * Sd1) / (L * Sx1x1 - Sx1^2)
  mu1_hat <- (Sd1 - delta_hat * Sx1) / L
  ssr6 <- Sd1d1 - mu1_hat * Sd1 - delta_hat * Sd1x1
  sigma2_eps <- ssr6 / (L - 2)

  # Regression 7: (Delta y_t)^2 = mu2 + omega*y_{t-1}^2 + eta_t.
  Sx2 <- Sx1x1 # x2 := x1^2
  Sx2x2 <- S("x1_4")
  Sd2 <- Sd1d1 # d2 := d1^2
  Sd2x2 <- S("d1_2x1_2")
  Sd2d2 <- S("d1_4")
  omega_hat <- (L * Sd2x2 - Sx2 * Sd2) / (L * Sx2x2 - Sx2^2)
  mu2_hat <- (Sd2 - omega_hat * Sx2) / L
  ssr7 <- Sd2d2 - mu2_hat * Sd2 - omega_hat * Sd2x2
  sigma2_eta <- ssr7 / (L - 2)
  Sx2x2_c <- Sx2x2 - Sx2^2 / L
  t_omega <- omega_hat / sqrt(sigma2_eta / Sx2x2_c)

  # Cross-moment sigma_hat_{eps*eta} := (1/(L-1)) * sum(eps_hat*eta_hat),
  # eps_hat_t = d1_t - mu1_hat - delta_hat*x1_t,
  # eta_hat_t = d2_t - mu2_hat - omega_hat*x2_t -- expanded into a
  # bilinear combination of window sums (verified against a brute-force
  # per-observation computation, see test-ssu.R).
  Sd1d2 <- S("d1_3") # sum(d1*d2) = sum(d1^3)
  Sd1x2 <- S("d1x1_2") # sum(d1*x2) = sum(d1*x1^2)
  Sx1d2 <- S("x1d1_2") # sum(x1*d2) = sum(x1*d1^2)
  Sx1x2 <- S("x1_3") # sum(x1*x2) = sum(x1^3)
  sum_eh <- Sd1d2 - mu2_hat * Sd1 - omega_hat * Sd1x2 - mu1_hat * Sd2 +
    L * mu1_hat * mu2_hat + mu1_hat * omega_hat * Sx2 -
    delta_hat * Sx1d2 + delta_hat * mu2_hat * Sx1 + delta_hat * omega_hat * Sx1x2
  sigma2_epseta <- sum_eh / (L - 1)

  sigma_eps <- sqrt(sigma2_eps)
  sigma_eta <- sqrt(sigma2_eta)
  psi_hat <- sigma2_epseta / (sigma_eps * sigma_eta)

  ybar2 <- Sx2 / L # mean of y_{t-1}^2 over the window
  # sum((y_{t-1}^2 - ybar2) * Delta y_t) = Sd1x2 - ybar2*Sd1 (window sum,
  # centered via the standard sum-of-products identity).
  num_corr <- Sd1x2 - ybar2 * Sd1
  # sum((y_{t-1}^2 - ybar2)^2) = Sx2x2 - 2*ybar2*Sx2 + L*ybar2^2 = Sx2x2_c.
  den_corr <- sqrt(Sx2x2_c)

  correction <- (psi_hat / sigma_eps) * num_corr / den_corr
  (t_omega - correction) / sqrt(1 - psi_hat^2)
}

#' Stochastic Unit Root Bubble Test (Kurozumi & Nishi 2025)
#'
#' \code{radf_ssu} implements the SSU statistic of Kurozumi & Nishi
#' (2025): a sup-type test for a bubble based on testing for a
#' stochastic (rather than deterministic) unit root in the *squared*
#' first differences, \code{(Delta y_t)^2 = mu2 + omega*y_{t-1}^2 +
#' eta_t}, bias-corrected against its dependence on the correlation
#' between this regression's and the plain ADF regression's innovations.
#'
#' A different generalization from the rest of exuber's volatility
#' -robustness tests: it doesn't touch the innovation variance at all,
#' but instead allows the explosive AR coefficient itself to vary
#' stochastically over time, \code{1 + c1/T + a*u_t/sqrt(T)}, rather than
#' the deterministic \code{1 + c/T^alpha} every recursive-ADF-family
#' statistic in this package assumes.
#'
#' Only the single-recursion \code{SSU} statistic (sup over the end
#' point, start fixed at the beginning of the sample) is implemented --
#' not \code{GSSU} (the double-recursion generalization), the paper's
#' separate CUSUM/CUSUM-SQ statistics, or the union-of-rejections
#' procedure combining SSU/GSSU with SADF/GSADF.
#'
#' @inheritParams radf
#' @param level Nominal confidence level, one of \code{0.90}, \code{0.95},
#' \code{0.99} (the levels Kurozumi & Nishi's Table I tabulates).
#'
#' @return An object of class \code{radf_ssu_obj}: a list with the
#' statistic path (\code{stat}, one value per candidate end point from
#' \code{minw} to \code{n}), the constant \code{crit} from Table I, and
#' \code{sadf} (the maximum, compared against \code{crit}) and
#' \code{detected}.
#'
#' @references Kurozumi, E., & Nishi, M. (2025). Bubble testing with
#' stochastically varying explosive coefficient. Journal of Time Series
#' Analysis, 46(5), 945-965.
#'
#' @seealso \code{\link{radf}} for the deterministic-coefficient
#' recursive ADF-family alternative this complements.
#'
#' @export
radf_ssu <- function(data, minw = NULL, level = 0.95) {
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  crit <- ssu_q(level)
  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  hi_idx <- minw:(n - 1L)
  stat_path <- matrix(NA_real_, length(hi_idx), nc, dimnames = list(NULL, snames))
  for (j in seq_len(nc)) {
    ps <- ssu_prefix_sums(as.numeric(x[, j]))
    stat_path[, j] <- ssu_stat_path(ps, hi_idx)
  }

  sadf <- apply(stat_path, 2, max)
  detected <- setNames(sadf > crit, snames)

  list(stat = stat_path, sadf = sadf, crit = crit, detected = detected) %>%
    add_attr(index = idx, series_names = snames, n = n, minw = minw, level = level) %>%
    add_class("radf_ssu_obj")
}

# Kurozumi & Nishi (2025) Table I: SSU's own published asymptotic
# critical value, one scalar per significance level (their own 10,000
# -rep Monte Carlo, r0 = 0.01 + 1.8/sqrt(T) -- exactly psy_minw()'s own
# formula).
ssu_q <- function(level) {
  beta <- 1 - level
  beta_choices <- c(0.10, 0.05, 0.01)
  match_idx <- which(abs(beta - beta_choices) < 1e-8)
  if (length(match_idx) == 0L) {
    stop_glue(
      "'level' must be one of {paste(1 - beta_choices, collapse = ', ')} ",
      "(Kurozumi & Nishi (2025)'s Table I only tabulates these ",
      "significance levels)."
    )
  }
  c(2.90, 3.30, 4.20)[match_idx]
}

#' @export
print.radf_ssu_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "radf_ssu (n = {attr(x, 'n')}, minw = {attr(x, 'minw')}, ",
    "level = {attr(x, 'level') * 100}%, crit = {x$crit})"
  ))
  cat_line()
  print(
    data.frame(
      series = names(x$sadf), sadf = x$sadf, detected = x$detected,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
