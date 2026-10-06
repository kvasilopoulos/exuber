# Kurozumi, E. & Nishi, M. (2025, JTSA 46(5), 945-965, "Bubble testing
# with stochastically varying explosive coefficient"; "KN"). See
# docs/volatility-robustness.md, "Stochastic explosive
# -coefficient test", for the full evaluation this implements.
#
# SSU (their eq. 7, sup-type, r1 fixed at 0) shipped first (2026-08-10)
# as the minimum-viable subset; GSSU (the double-sup over window starts)
# and the UR/GUR union-of-rejections procedure followed 2026-09-29 --
# every one of them has a published asymptotic critical value in Table I
# (including the union scaling constants ur/gur), so none needs new
# simulation. The paper's CUSUM/CUSUM-SQ statistics live in cusum_test.R.
# Re-triaged 2026-08-10:
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
    x1 = mk(x1),
    x1_2 = mk(x1^2),
    x1_3 = mk(x1^3),
    x1_4 = mk(x1^4),
    d1 = mk(d1),
    d1_2 = mk(d1^2),
    d1_3 = mk(d1^3),
    d1_4 = mk(d1^4),
    d1x1 = mk(d1 * x1),
    d1_2x1_2 = mk(d1^2 * x1^2),
    d1x1_2 = mk(d1 * x1^2),
    x1d1_2 = mk(x1 * d1^2)
  )
}

# t^{omega,c}_{r1, r2} for every window (lo, hi] of regression pairs
# (matching hls_segment_ssr()'s convention), `lo`/`hi_idx` recycled
# against each other: lo = 0 gives SSU's own single-recursion path, a
# grid of (lo, hi) pairs gives GSSU's double recursion.
ssu_stat_path <- function(ps, hi_idx, lo = 0L) {
  S <- function(nm) ps[[nm]][hi_idx + 1L] - ps[[nm]][lo + 1L] # window (lo, hi] sum
  L <- hi_idx - lo

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

  # Cross-moment sigma_hat_{eps*eta} := (1/(L-2)) * sum(eps_hat*eta_hat)
  # (the same 1/(floor(T r2) - floor(T r1) - 1) as both variances, page 6),
  # eps_hat_t = d1_t - mu1_hat - delta_hat*x1_t,
  # eta_hat_t = d2_t - mu2_hat - omega_hat*x2_t -- expanded into a
  # bilinear combination of window sums (verified against a brute-force
  # per-observation computation, see test-ssu.R).
  Sd1d2 <- S("d1_3") # sum(d1*d2) = sum(d1^3)
  Sd1x2 <- S("d1x1_2") # sum(d1*x2) = sum(d1*x1^2)
  Sx1d2 <- S("x1d1_2") # sum(x1*d2) = sum(x1*d1^2)
  Sx1x2 <- S("x1_3") # sum(x1*x2) = sum(x1^3)
  sum_eh <- Sd1d2 -
    mu2_hat * Sd1 -
    omega_hat * Sd1x2 -
    mu1_hat * Sd2 +
    L * mu1_hat * mu2_hat +
    mu1_hat * omega_hat * Sx2 -
    delta_hat * Sx1d2 +
    delta_hat * mu2_hat * Sx1 +
    delta_hat * omega_hat * Sx1x2
  sigma2_epseta <- sum_eh / (L - 2)

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

# GSSU's recursive path: for each end point hi, the sup over window
# starts lo = 0, ..., hi - minw (bsadf's shape); its max is GSSU.
gssu_stat_path <- function(ps, hi_idx, minw) {
  vapply(hi_idx, function(hi) max(ssu_stat_path(ps, hi, 0:(hi - minw))), numeric(1))
}

# KN's recommended GSSU minimum window, r0 = -0.004 + 2.24/sqrt(T) (Table I
# note: psy_minw()'s formula oversizes GSSU).
gssu_minw <- function(n) floor(n * (-0.004 + 2.24 / sqrt(n)))

#' Stochastic Unit Root Bubble Test (Kurozumi & Nishi 2025)
#'
#' \code{ssu_test} implements the SSU and GSSU statistics of Kurozumi & Nishi
#' (2025). They are sup-type tests for a bubble that test for a stochastic, and not
#' deterministic, unit root in the \emph{squared} first differences,
#' \code{(Delta y_t)^2 = mu2 + omega*y_{t-1}^2 + eta_t}. The statistic is
#' bias-corrected for the dependence on the correlation between the innovations of
#' this regression and those of the plain ADF regression.
#'
#' This test generalizes the framework differently from the other
#' volatility-robustness tests in exuber. It does not touch the innovation variance
#' at all. It allows the explosive AR coefficient itself to vary stochastically over
#' time, \code{1 + c1/T + a*u_t/sqrt(T)}, where every recursive-ADF-family statistic
#' in this package assumes the deterministic coefficient \code{1 + c/T^alpha}.
#'
#' \code{type = "ssu"} is the single recursion, with the start fixed at the
#' beginning of the sample, which has the shape of \code{SADF}. \code{type = "gssu"}
#' also takes the supremum over window starts, which has the shape of \code{GSADF},
#' with the minimum window \code{r0 = -0.004 + 2.24/sqrt(n)} from the paper. The
#' paper finds that GSSU is no more powerful than SSU.
#'
#' \code{union = TRUE} adds the union-of-rejections procedure that the paper
#' recommends: \code{UR = max(SADF / cv_sadf, SSU / cv_ssu)} (or \code{GUR} with
#' GSADF and GSSU), compared with the published scaling constant \code{ur}
#' (\code{gur}). Neither SADF nor SSU dominates. SSU wins when the explosive
#' coefficient is stochastic, SADF wins when it is deterministic, and the union
#' stays close to the better of the two. The SADF or GSADF side is
#' \code{\link{radf}} with its default minimum window and \code{lag = 0}, compared
#' with \code{cv} (default: the precomputed critical values).
#'
#' @note The SSU and GSSU critical values and the union constants are published
#' asymptotic values (Table I of Kurozumi & Nishi 2025), so no simulation is
#' needed. The union constant is valid only at the level for which the statistic
#' was built.
#'
#' @inheritParams radf
#' @param minw Minimum window. The default is \code{\link{psy_minw}} for
#' \code{"ssu"} and the value of the paper, \code{floor(n * (-0.004 + 2.24/sqrt(n)))},
#' for \code{"gssu"}. Table I is computed at these values.
#' @param sig_lvl Significance level on the 0 to 100 scale used throughout the
#' package, one of \code{90}, \code{95} or \code{99}. Table I of Kurozumi & Nishi
#' tabulates these levels.
#' @param type \code{"ssu"} or \code{"gssu"}.
#' @param union Logical. Also run the union-of-rejections procedure with SADF
#' (\code{"ssu"}) or GSADF (\code{"gssu"}).
#' @param cv Critical values for the SADF or GSADF side of the union, for example from
#' \code{\link{radf_mc_cv}} with \code{lag = 0}. The default is the precomputed
#' critical values, which are fetched on first use.
#'
#' @return An object of class \code{ssu_test_obj}: a list with the statistic path
#' (\code{stat}, one value for each candidate end point from \code{minw} to \code{n}.
#' For GSSU it is the sup over window starts at each end point), the constant
#' \code{crit} from Table I, \code{sadf} (the maximum, which is compared with
#' \code{crit}) and \code{detected}. With \code{union = TRUE} the list also contains
#' \code{adf_stat} (SADF or GSADF), \code{union_stat}, \code{union_crit} and
#' \code{union_detected}.
#'
#' @references Kurozumi, E., & Nishi, M. (2025). Bubble testing with
#' stochastically varying explosive coefficient. Journal of Time Series
#' Analysis, 46(5), 945-965.
#'
#' @seealso \code{\link{radf}} for the recursive ADF-family alternative with a
#' deterministic coefficient, which this test complements.
#'
#' @note The function returns its own class and not `radf_obj`, so it does not work
#' with `summary()`, `\link{datestamp}` and `tidy`. It has its own `print()` and
#' `autoplot()` methods instead. `print()` shows the statistic and the critical value. See
#' `vignette("naming-and-analysis", package = "exuber")` for which functions fit
#' the shared pipeline and which do not.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' # A stochastically varying explosive root, rho_t = 1 + 3/n + 4 * u_t / sqrt(n).
#' # This is the alternative that ssu_test() is built for, and lbi_test() is built
#' # for a fixed root
#' y <- sim_psy1(n = 150, te = 75, tf = 150, c = 3, alpha = 1, seed = 2001,
#'   coef_noise = rnorm(149), coef_a = 4)
#' res <- ssu_test(y, sig_lvl = 95)
#' print(res)
#'
#' # The double-recursion version
#' ssu_test(y, type = "gssu")
#'
#' # Plot the recursive SSU statistic path against its critical value
#' autoplot(res)
#' }
#'
#' @family volatility-robust tests
#' @export
ssu_test <- function(
  data,
  minw = NULL,
  sig_lvl = 95,
  type = c("ssu", "gssu"),
  union = FALSE,
  cv = NULL
) {
  type <- match.arg(type)
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% if (type == "ssu") psy_minw(n) else gssu_minw(n)
  assert_positive_int(minw, greater_than = 2)

  crit <- ssu_q(sig_lvl, type)
  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  hi_idx <- minw:(n - 1L)
  stat_path <- matrix(NA_real_, length(hi_idx), nc, dimnames = list(NULL, snames))
  for (j in seq_len(nc)) {
    ps <- ssu_prefix_sums(as.numeric(x[, j]))
    stat_path[, j] <- if (type == "ssu") {
      ssu_stat_path(ps, hi_idx)
    } else {
      gssu_stat_path(ps, hi_idx, minw)
    }
  }

  sadf <- apply(stat_path, 2, max)
  detected <- setNames(sadf > crit, snames)
  out <- list(stat = stat_path, sadf = sadf, crit = crit, detected = detected)

  if (union) {
    r <- radf(x, lag = 0L)
    cv <- cv %||% retrieve_crit(r)
    lvl <- paste0(sig_lvl, "%")
    adf_stat <- if (type == "ssu") r$sadf else r$gsadf
    cv_adf <- if (type == "ssu") cv$sadf_cv[lvl] else cv$gsadf_cv[lvl]
    union_stat <- setNames(pmax(adf_stat / cv_adf, sadf / crit), snames)
    union_crit <- ssu_q(sig_lvl, if (type == "ssu") "ur" else "gur")
    out <- c(
      out,
      list(
        adf_stat = setNames(adf_stat, snames),
        union_stat = union_stat,
        union_crit = union_crit,
        union_detected = union_stat > union_crit
      )
    )
  }

  out %>%
    add_attr(
      index = idx,
      series_names = snames,
      n = n,
      minw = minw,
      sig_lvl = sig_lvl,
      type = type
    ) %>%
    add_class("ssu_test_obj")
}

#' Plot method for ssu_test() output
#'
#' Plots the recursive SSU statistic path against its critical value, with one panel for each series.
#'
#' @param object An object of class \code{ssu_test_obj}, the output of \code{\link{ssu_test}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{ssu_test}}
#' @export
autoplot.ssu_test_obj <- function(object, ...) {
  minw <- attr(object, "minw")
  pos <- minw:(minw + nrow(object$stat) - 1L)
  autoplot_stat_boundary(
    pos,
    object$stat,
    object$crit,
    ylab = paste(toupper(attr(object, "type") %||% "ssu"), "statistic")
  )
}

# Kurozumi & Nishi (2025) Table I: published asymptotic critical values
# (their own 10,000-rep Monte Carlo, Brownian motion from 1000 steps) --
# SSU at r0 = 0.01 + 1.8/sqrt(T) (psy_minw()), GSSU at r0 = -0.004 +
# 2.24/sqrt(T), and the union scaling constants ur (SADF+SSU) and gur
# (GSADF+GSSU). Also the CUSUM-family columns used by cusum_test(): CS,
# GCS, and the two-sided CSSQ/GCSSQ pairs (each tail at alpha/2, so the
# "level alpha" row is the two-sided test at alpha).
kn_table <- list(
  ssu = c(2.90, 3.30, 4.20),
  gssu = c(4.83, 5.37, 6.81),
  ur = c(1.16, 1.13, 1.09),
  gur = c(1.11, 1.10, 1.08),
  cs = c(1.62, 1.93, 2.57),
  gcs = c(1.90, 2.20, 2.78),
  cssq_sup = c(1.19, 1.32, 1.59),
  cssq_inf = c(-1.21, -1.34, -1.60),
  gcssq_sup = c(1.60, 1.72, 1.98),
  gcssq_inf = c(-1.62, -1.72, -1.97)
)

ssu_q <- function(sig_lvl, stat = "ssu") {
  choices <- c(90, 95, 99)
  match_idx <- which(abs(sig_lvl - choices) < 1e-8)
  if (length(match_idx) == 0L) {
    stop_glue(
      "'sig_lvl' must be one of {paste(choices, collapse = ', ')} ",
      "(Kurozumi & Nishi (2025)'s Table I only tabulates these ",
      "significance levels)."
    )
  }
  kn_table[[stat]][match_idx]
}

#' @export
print.ssu_test_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(
    left = glue(
      "ssu_test ({toupper(attr(x, 'type') %||% 'ssu')}, n = {attr(x, 'n')}, minw = {attr(x, 'minw')}, ",
      "sig_lvl = {attr(x, 'sig_lvl')}%, crit = {x$crit})"
    )
  )
  cat_line()
  df <- data.frame(series = names(x$sadf), sadf = x$sadf, detected = x$detected, row.names = NULL)
  if (!is.null(x$union_stat)) {
    df$union <- x$union_stat
    df$union_detected <- x$union_detected
  }
  print(df, digits = digits, print.gap = 2L, row.names = FALSE)
  if (!is.null(x$union_stat)) {
    cat_line("union critical value: ", x$union_crit)
  }
  cat_line()
}
