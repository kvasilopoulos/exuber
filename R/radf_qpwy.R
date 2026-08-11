# Wu, Shi & Wu (2025, JTSA 46(5), "Quantile analysis for financial bubble
# detection and surveillance", "WSW") -- the QPWY recursive monitoring
# strategy of their Section 3.2 (their eq. 25, 28). See
# docs/enhancements/alternative-paradigms.md, "Quantile-based detection",
# for the full evaluation this implements.
#
# QPWY is the single-recursion sibling of the paper's own QPSY (eq. 26):
# QPWY_r(tau) := t_T^{0,r}(tau), the quantile-regression t-ratio on the
# EXPANDING window [1, r] (start fixed at the beginning, exactly
# radf()'s own badf convention) -- QPSY additionally sup's over every
# window START r1 too (a genuine O(T^2) double recursion of QR fits, no
# closed form the way radf()'s own rls_gsadf() has for OLS), left
# unimplemented, matching this file's own earlier "not implemented"
# scoping for the double-recursion case specifically (not QPWY, which
# was originally bundled with QPSY under the same verdict without
# separating their very different cost profiles).
#
# Re-triaged 2026-08-11, re-reading rendered pages 10-11 (their
# Corollary 1-2): QPWY's point statistic genuinely needs O(T) actual QR
# fits (quantreg::rq() has no closed-form recursive update the way OLS
# does -- this part of the original "no closed form" assessment holds),
# but the CRITICAL VALUE machinery turns out to reuse what
# radf_quantile.R already validated, not new simulation theory: their
# Corollary 1 decomposes the limiting distribution of t_T^{r1,r2}(tau)
# as U'^{r1,r2}(tau) = sqrt(1-delta(tau)^2)*z + delta(tau)*Q_{r1,r2},
# EXACTLY radf_quantile()'s own global-test decomposition, and their
# Corollary 2 confirms QPWY_r(tau) => U'^{0,r}(tau) -- i.e. Q_{0,r} is
# precisely radf()'s own badf[r] under a simulated null path (an
# expanding-window ADF t-statistic distribution), not a new functional.
# One radf() call per simulated null replicate therefore gives the WHOLE
# Q_{0,r} boundary path at once (qpwy_boundary_sim() below), reusing
# radf_quantile.R's own quantile_check_density() for delta(tau) and the
# same "z ~ N(0,1) combined with Q" construction -- only the point
# statistic (qpwy_stat_path()) is genuinely new code, an O(T) loop
# mirroring radf_quantile()'s own per-window t-ratio construction.

# QPWY_r(tau) for every window-end r in `r_idx` -- window fixed at [1, r]
# (start = 1, matching radf()'s own badf convention), mirroring
# radf_quantile()'s own per-window QR t-ratio construction (eq. 18)
# exactly, just repeated over a growing window instead of the full
# sample.
qpwy_stat_path <- function(y, tau, r_idx) {
  vapply(r_idx, function(r) {
    yy <- y[1:r]
    m <- length(yy)
    ylag <- yy[1:(m - 1L)]
    yresp <- yy[2:m]
    dy <- yresp - ylag

    qr_fit <- quantreg::rq(yresp ~ ylag, tau = tau)
    alpha_hat <- unname(stats::coef(qr_fit)["ylag"])

    f_hat <- quantile_check_density(dy, tau)$f_hat
    yPzy <- sum((ylag - mean(ylag))^2)
    (f_hat / sqrt(tau * (1 - tau))) * sqrt(yPzy) * (alpha_hat - 1)
  }, numeric(1))
}

# Simulated null Q_{0,r} paths: one radf() call per replicate gives the
# WHOLE expanding-window badf sequence at once (Corollary 2's own
# identification of Q_{0,r} with the ADF-family recursive t-statistic
# distribution) -- an nrep x (n - minw) matrix, matching badf's own
# length exactly (badf[k] <-> window end t = minw + k, i.e. the first
# valid recursive point is t = minw + 1, not t = minw itself).
qpwy_boundary_sim <- function(n, minw, nrep, seed = NULL) {
  set_rng(seed)
  Q <- matrix(NA_real_, nrep, n - minw)
  for (i in seq_len(nrep)) {
    ysim <- cumsum(stats::rnorm(n))
    r <- radf(ysim, minw = minw, lag = 0L)
    Q[i, ] <- r$badf[, 1]
  }
  Q
}

#' QPWY Recursive Quantile Monitoring (Wu, Shi & Wu 2025)
#'
#' \code{radf_qpwy} implements the QPWY real-time monitoring strategy of
#' Wu, Shi & Wu (2025): a quantile-regression (QR) analogue of PWY's own
#' recursive ADF t-statistic, testing at a chosen conditional quantile
#' \code{tau} over an expanding window \code{[1, r]} (start fixed at the
#' beginning of the sample, exactly \code{\link{radf}}'s own \code{badf}
#' convention) rather than \code{\link{radf_quantile}}'s single
#' full-sample test.
#'
#' Only \code{QPWY} (single recursion) is implemented, not the paper's
#' own \code{QPSY} (double recursion, additionally optimizing over the
#' window start): \code{QPWY_r(tau)} needs \code{O(T)} actual quantile
#' -regression fits (no closed-form recursive update the way OLS has),
#' tractable at the same cost order as \code{radf()}'s own \code{badf};
#' \code{QPSY} needs \code{O(T^2)} such fits, a substantially larger
#' undertaking left unimplemented.
#'
#' The critical value is simulated per call: \code{QPWY_r(tau)}'s
#' limiting null distribution at each \code{r} is \code{sqrt(1 - delta^2)
#' * z + delta * Q_{0,r}}, with \code{z ~ N(0, 1)}, \code{delta} a
#' data-estimated correlation coefficient (as in
#' \code{\link{radf_quantile}}), and \code{Q_{0,r}} exactly \code{radf()}'s
#' own \code{badf} sequence under a simulated null path -- reusing
#' \code{radf()} directly for the simulation rather than new theory. A
#' single \strong{flat} boundary is used (not one value per \code{r}):
#' controlling the first-crossing false-alarm rate requires calibrating
#' against each simulated path's own supremum, exactly how
#' \code{\link{radf_mc_cv}}'s own \code{sadf_cv} is constructed, not a
#' per-\code{r} marginal quantile (which would badly inflate the
#' false-alarm rate).
#'
#' @inheritParams radf
#' @param tau Quantile to test at, in \code{(0, 1)} (fixed, unlike
#' \code{\link{radf_quantile}}'s \code{"optimal"} grid search -- WSW's own
#' eq. 25 takes \code{tau} as a given parameter for the monitoring
#' statistic, not re-selected at each recursion point).
#' @param nrep Number of Monte Carlo replications for the boundary.
#' @param level Significance level, one of \code{90}, \code{95}, \code{99}.
#' @param seed Optional seed for the Monte Carlo draws.
#'
#' @return An object of class \code{radf_qpwy_obj}: a list with the
#' statistic path \code{stat}, the (flat) \code{boundary}, the estimated
#' \code{delta}, and \code{alarm}/\code{alarm_date} (the first breach,
#' \code{NA} if none).
#'
#' @references Wu, R., Shi, S., & Wu, J. (2025). Quantile analysis for
#' financial bubble detection and surveillance. Journal of Time Series
#' Analysis, 46(5), 908-931.
#'
#' @seealso \code{\link{radf_quantile}} for the static, full-sample
#' version of this test. \code{\link{radf_monitor}} for the OLS-based
#' monitoring alternative.
#'
#' @export
radf_qpwy <- function(data, tau = 0.5, minw = NULL, nrep = 500L, level = 95, seed = NULL) {
  stopifnot(tau > 0 && tau < 1)
  stopifnot(level %in% c(90, 95, 99))
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)
  r_idx <- (minw + 1L):n

  Q <- qpwy_boundary_sim(n, minw, nrep, seed = seed)
  z <- stats::rnorm(nrep)

  stat_path <- matrix(NA_real_, length(r_idx), nc, dimnames = list(NULL, snames))
  delta <- boundary <- setNames(rep(NA_real_, nc), snames)
  alarm <- setNames(rep(NA_integer_, nc), snames)

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    stat_path[, j] <- qpwy_stat_path(y, tau, r_idx)

    dy_full <- diff(y)
    psi <- tau - as.numeric(dy_full < quantile_narm(dy_full, probs = tau, names = FALSE))
    delta_j <- max(min(stats::cor(dy_full, psi), 1), -1)
    delta[j] <- delta_j

    # A first-crossing/monitoring test needs a boundary controlling the
    # SUPREMUM probability P(sup_r [stat_path(r)] > boundary), not the
    # per-r marginal quantile -- using a per-r marginal quantile as a
    # r-varying boundary badly inflates the false-alarm rate (an initial
    # version gave ~50% against a nominal 5%, caught by Monte Carlo
    # validation, not assumed correct from the formula alone). Mirrors
    # radf_mc_cv()'s own sadf_cv construction exactly: take each
    # simulated path's own supremum first, then the quantile of those
    # maxima across replicates, giving one flat critical value.
    U <- sqrt(1 - delta_j^2) * z + delta_j * Q
    sup_U <- apply(U, 1, max)
    boundary[j] <- quantile_narm(sup_U, probs = level / 100, names = FALSE)

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
      tau = tau, level = level, iter = nrep
    ) %>%
    add_class("radf_qpwy_obj")
}

#' @export
print.radf_qpwy_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "radf_qpwy (n = {attr(x, 'n')}, minw = {attr(x, 'minw')}, ",
    "tau = {attr(x, 'tau')}, level = {attr(x, 'level')}%)"
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
