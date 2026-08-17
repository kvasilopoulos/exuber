# Greenaway-McGrevy, R. & Phillips, P.C.B. (2016, New Zealand Economic
# Papers 50(1), 88-113; Cowles Foundation Discussion Paper 2004 (2015),
# "Hot Property in New Zealand"; "GMP"), Section 2.6 / Technical Appendix
# 5.1.2 ("Contagion Regressions"). See docs/enhancements/multivariate.md,
# "Contagion regression", for the full evaluation this implements.
#
# Re-triaged (2026-08-10) after an earlier pass scoped this as "the most
# expensive item in the file, no reusable machinery" -- wrong on two of
# three cost drivers, confirmed by reading rendered PDF pages 17 and 26
# directly (not the raw pdftotext extraction the earlier pass had
# flagged as needing this re-check):
#
# 1. The fixed-window AR(1) coefficient sequence (eq. 1, GMP's own
#    "fixed window width subsample" scheme) is the SAME closed-form
#    cumulative-sum-difference OLS-window construction already used for
#    dating_hls.R's hls_segment_ssr()/hls_prefix_sums() -- just levels-on-
#    lagged-levels (eq. 1: y_t = delta + beta*y_{t-1} + e_t) instead of
#    differences-on-lagged-levels, and a MOVING rather than expanding
#    window. hls_segment_coef()'s own closed form is generic (any x, z
#    pair over a segment), so only a differently-built prefix-sum object
#    is needed here, not new regression math.
# 2. The Nadaraya-Watson step (eq. 6) is NOT a general nonparametric
#    estimator -- it is the closed-form solution of a no-intercept,
#    single-regressor WEIGHTED least squares fit at each evaluation
#    point r, i.e. a ratio of two Gaussian-kernel-weighted sums,
#    vectorizable as one T x length(r_grid) weight matrix (outer() +
#    dnorm()) times two vectors.
# 3. The LOOCV bandwidth (eq. 7) IS genuinely new (the one place the
#    earlier assessment was right) but is a textbook 1-D bounded
#    optimization (stats::optimize() over eq. 7's own H_T interval), not
#    a grid search.
#
# No published numeric table exists to validate against -- GMP's own
# results are Figures 7-8 (time-varying coefficient plots), not tabulated
# numbers, the same "figures only" situation as SBZ's own size/power
# results elsewhere in this project. Validated instead via structural
# checks and a direct brute-force cross-check of the closed-form NW ratio
# against a manual weighted-least-squares fit at several (r, h) points.
#
# Scoped as the minimum-viable subset this project's own re-triage
# recommended: the fixed-window AR(1) sequence, the NW regression at a
# SINGLE caller-supplied delay `d` (eq. 6), and the LOOCV bandwidth (eq.
# 7). Eq. 8's automatic delay search (repeat the above for d = 0..12,
# keep the best by SSE) is a thin, separable follow-on, not implemented
# here -- deliberately, matching how KNP's multi-bubble DP algorithm and
# HLW's fragmentation-joining heuristic were scoped out elsewhere in this
# project rather than rushed. GMP's own text: `d in {0,...,12}` "months",
# but the empirical section discusses delays in quarters against
# quarterly data -- a paper-internal unit inconsistency (confirmed on the
# rendered page); read `d` as "native sampling periods of the input
# series," not literally months.

# Prefix sums for the fixed-window AR(1) coefficient sequence (eq. 1):
# same structure as dating_hls.R's hls_prefix_sums(), but z = y_t (levels)
# rather than hls_prefix_sums()'s own hard-coded z = Delta y_t.
contagion_ar1_prefix_sums <- function(y) {
  n1 <- length(y) - 1L
  x <- y[1:n1]
  z <- y[2:(n1 + 1L)]
  list(
    cx = c(0, cumsum(x)), cx2 = c(0, cumsum(x^2)),
    cz = c(0, cumsum(z)), cxz = c(0, cumsum(x * z)), n1 = n1
  )
}

# Fixed-window OLS slope sequence (eq. 1). GMP's own definition -- window
# {t = s-S+1, ..., s} -- means S LEVEL observations (S T-indices), which
# is S-1 (response, lag) PAIRS, not S pairs (confirmed via brute-force
# lm() cross-check against exactly this reading; an initial S-pairs
# version was off by one against it). Window (lo, hi] in i-index terms
# (i = t - 1) therefore has hi - lo = S - 1. Returns a numeric vector
# named by t (the window's own end date, in levels-index terms), for
# every window-end s = S, ..., T -- reusing hls_segment_ssr()'s (lo, hi]
# convention.
contagion_fixed_window_beta <- function(y, S) {
  ps <- contagion_ar1_prefix_sums(y)
  hi <- S:ps$n1
  lo <- hi - (S - 1L)
  Sx <- ps$cx[hi + 1L] - ps$cx[lo + 1L]
  Sxx <- ps$cx2[hi + 1L] - ps$cx2[lo + 1L]
  Sz <- ps$cz[hi + 1L] - ps$cz[lo + 1L]
  Sxz <- ps$cxz[hi + 1L] - ps$cxz[lo + 1L]
  n_seg <- S - 1L
  beta <- (n_seg * Sxz - Sx * Sz) / (n_seg * Sxx - Sx^2)
  setNames(beta, hi + 1L)
}

# eq. 6's Gaussian kernel weight K_hs(r) = (1/h)*K((s/T - r)/h), for
# every combination of "position" s/T_len and "evaluation point" r --
# one vectorized matrix, positions as rows, evaluation points as columns.
contagion_kernel_weights <- function(s, T_len, r, h) {
  outer(s / T_len, r, function(sp, rr) stats::dnorm((sp - rr) / h) / h)
}

# eq. 6: delta_2j(r; h, d), the Nadaraya-Watson local-constant kernel
# regression coefficient -- a closed-form ratio of two kernel-weighted
# sums (a no-intercept WLS solution), NOT a general nonparametric
# estimator. `beta_core`/`beta_j` are named numeric vectors from
# contagion_fixed_window_beta() (names = window-end date s); `T_len` is
# GMP's own "T" (total sample length) used for the s/T weight position.
# Centering (eq. 6's beta-tilde) uses each series' own full range, before
# any delay shift is applied to core.
contagion_nw_delta2 <- function(beta_core, beta_j, T_len, r_grid, h, d) {
  bj_c <- beta_j - mean(beta_j)
  bcore_c <- beta_core - mean(beta_core)

  s <- as.integer(names(beta_j))
  core_shift <- bcore_c[as.character(s - d)]
  valid <- !is.na(core_shift)
  s <- s[valid]
  bj_c <- bj_c[valid]
  core_shift <- core_shift[valid]

  K <- contagion_kernel_weights(s, T_len, r_grid, h)
  num <- as.numeric(crossprod(K, bj_c * core_shift))
  den <- as.numeric(crossprod(K, core_shift^2))
  num / den
}

# eq. 7's leave-one-out SSE at bandwidth h, delay d: evaluates the LOO
# version of eq. 6 at r = s/(T-S+1) for each available s (excluding the
# point itself from the kernel sum, GMP's own p != s), then sums squared
# prediction errors.
contagion_loocv_sse <- function(h, beta_core, beta_j, T_len, d) {
  bj_c <- beta_j - mean(beta_j)
  bcore_c <- beta_core - mean(beta_core)

  s <- as.integer(names(beta_j))
  core_shift <- bcore_c[as.character(s - d)]
  valid <- !is.na(core_shift)
  s <- s[valid]
  bj_c <- bj_c[valid]
  core_shift <- core_shift[valid]

  m <- length(s)
  r <- s / m
  K <- contagion_kernel_weights(s, T_len, r, h)
  diag(K) <- 0
  # crossprod(K, v)[i] = sum_p K[p, i] * v[p] -- sum over POSITIONS p
  # (rows) for each fixed EVALUATION point i (column), matching eq. 6/7's
  # own sum over s. Plain `K %*% v` would instead sum over columns per
  # row -- wrong orientation, since K is not symmetric (K[p, i], weighing
  # position p at evaluation point i, differs from K[i, p]). Caught by a
  # brute-force double-loop cross-check that initially disagreed with
  # this function by ~1% -- traced to exactly this K %*% v vs
  # crossprod(K, v) mismatch (contagion_nw_delta2() already used the
  # correct crossprod() form; this function originally didn't).
  num <- as.numeric(crossprod(K, bj_c * core_shift))
  den <- as.numeric(crossprod(K, core_shift^2))
  pred <- (num / den) * core_shift
  sum((bj_c - pred)^2)
}

# eq. 7: bandwidth via 1-D bounded LOOCV, H_T = [(T-S+1)^-1/2,
# (T-S+1)^-1/10] -- stats::optimize() directly, no custom search.
contagion_bandwidth_cv <- function(beta_core, beta_j, T_len, d) {
  m <- length(beta_j)
  H_T <- c(m^(-1 / 2), m^(-1 / 10))
  opt <- stats::optimize(contagion_loocv_sse, interval = H_T,
                          beta_core = beta_core, beta_j = beta_j,
                          T_len = T_len, d = d)
  opt$minimum
}

#' Bubble Contagion Regression (Greenaway-McGrevy & Phillips 2016)
#'
#' \code{contagion_reg} estimates the time-varying contagion coefficient
#' of Greenaway-McGrevy & Phillips (2016): a fixed-window rolling AR(1)
#' coefficient sequence for a "core" series and a "satellite" series
#' \code{y}, related by a functional (Nadaraya-Watson kernel) regression
#' at a chosen delay \code{d} -- how strongly and how (time-varying) does
#' the core series' local persistence transmit to \code{y}, \code{d}
#' periods later.
#'
#' This is the minimum-viable subset of the paper's own procedure: the
#' fixed-window AR(1) coefficient sequence (their eq. 1), the
#' Nadaraya-Watson regression at a single supplied \code{d} (eq. 6), and
#' leave-one-out cross-validated bandwidth selection (eq. 7). Their eq. 8
#' (searching over \code{d} automatically) is not implemented -- call
#' \code{contagion_reg} once per candidate \code{d} and compare fit if
#' an automatic search is needed.
#'
#' The paper performs no formal inference (no confidence bands, no
#' hypothesis test) on the contagion coefficient itself -- this is a
#' point-estimation and visualization tool, not a test, matching what the
#' source paper itself does.
#'
#' @note Not a hypothesis test: \code{contagion_reg} performs no formal
#' inference (no confidence bands, no significance test) on the contagion
#' coefficient, so there is no critical value at all for this function --
#' don't look for one.
#'
#' @param y Satellite (dependent) series, numeric vector.
#' @param core Core (reference) series, numeric vector, same length as
#' \code{y}.
#' @param S Fixed rolling-window width for the AR(1) coefficient sequence
#' (default \code{floor(0.33 * length(y))}, the paper's own choice).
#' @param d Non-negative integer delay (default \code{0}).
#' @param h Bandwidth for the Nadaraya-Watson regression. Default
#' \code{NULL} selects it via leave-one-out cross-validation (eq. 7).
#' @param r_grid Evaluation points for the time-varying coefficient,
#' as fractions of the sample (default \code{seq(0, 1, length.out = 100)}).
#'
#' @return An object of class \code{contagion_reg_obj}: a list with the
#' fixed-window AR(1) coefficient sequences (\code{beta_core},
#' \code{beta_j}), the selected/supplied bandwidth (\code{h}), and the
#' estimated time-varying contagion coefficient (\code{delta2}, aligned
#' with \code{r_grid}).
#'
#' @references Greenaway-McGrevy, R., & Phillips, P. C. B. (2016). Hot
#' property in New Zealand: Empirical evidence of housing bubbles in the
#' metropolitan centres. New Zealand Economic Papers, 50(1), 88-113.
#'
#' @seealso \code{\link{cobubble_test}} for a different (symmetric,
#' hypothesis-testing) bivariate bubble relationship.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- contagion_reg(sim_data$psy1, sim_data$psy2, d = 0L)
#' print(res)
#' }
#'
#' @importFrom stats dnorm optimize
#' @export
contagion_reg <- function(y, core, S = NULL, d = 0L, h = NULL,
                            r_grid = seq(0, 1, length.out = 100)) {
  y <- as.numeric(y)
  core <- as.numeric(core)
  if (length(y) != length(core)) {
    stop_glue("'y' and 'core' must be the same length.")
  }
  assert_positive_int(d, strictly = FALSE)
  T_len <- length(y)
  S <- S %||% max(floor(0.33 * T_len), 5L)
  assert_positive_int(S, greater_than = 2)

  beta_j <- contagion_fixed_window_beta(y, S)
  beta_core <- contagion_fixed_window_beta(core, S)

  if (is.null(h)) {
    h <- contagion_bandwidth_cv(beta_core, beta_j, T_len, d)
  }
  delta2 <- contagion_nw_delta2(beta_core, beta_j, T_len, r_grid, h, d)

  list(
    beta_core = beta_core, beta_j = beta_j, h = h, d = d,
    r_grid = r_grid, delta2 = delta2
  ) %>%
    add_attr(n = T_len, S = S) %>%
    add_class("contagion_reg_obj")
}

#' @export
print.contagion_reg_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "contagion_reg (n = {attr(x, 'n')}, S = {attr(x, 'S')}, d = {x$d}, ",
    "h = {round(x$h, 4)})"
  ))
  cat_line()
  cat(glue("delta_2(r) range: [{round(min(x$delta2), 3)}, {round(max(x$delta2), 3)}]"), "\n")
  cat_line()
}
