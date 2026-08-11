# Wu, Shi & Wu (2025, JTSA 46(5), "Quantile analysis for financial bubble
# detection and surveillance", "WSW") -- the "global test" of their
# Section 3.1: a quantile-regression (QR) analogue of the DF t-ratio,
# testing for a bubble via the tau-th conditional quantile of y_t on
# y_{t-1} rather than the conditional mean. See docs/enhancements/
# alternative-paradigms.md, "Quantile-based detection", for the full
# evaluation this implements.
#
# Deliberately scoped to the *global* (static, single-window) test only
# (their eqs. 17-23, 33) -- not their QPWY/QPSY recursive monitoring
# extension, which needs an O(T^2) double-recursive scan of QR fits (no
# closed form per window, unlike every other exuber statistic) and is a
# substantially larger undertaking, left unimplemented. This mirrors
# radf()'s own single-shot `adf` statistic, not its recursive `bsadf`
# scan.
#
# Formulas transcribed from rendered PDF pages (pp. 914-915, 921), not
# the raw pdftotext extraction -- eq. 18 in particular reads differently
# in the scrambled OCR text (`sqrt(f_hat(b_tau)/(1-tau))`) than in the
# actual paper (`f_hat(b_tau)/sqrt(tau*(1-tau))`); this file uses the
# rendered-page version, confirmed correct.
#
# A key structural finding, verified empirically not just derived: the
# statistic's critical value (eq. 22-23) is `U(tau) = sqrt(1-delta^2)*z +
# delta*Q`, where `z ~ N(0,1)` and `Q` is the *standard, demeaned Dickey-
# Fuller t-statistic distribution* (eq. 23's `(int W_bar^2)^{-1/2} int
# W_bar dW` is exactly that functional) -- i.e. Q's distribution is
# nothing new: it is the distribution of a plain intercept-only OLS
# ADF t-statistic (no lag) computed on a simulated random walk, the same
# quantity radf()'s own single-shot `adf` field already computes (see
# radf_quantile_validate_q.R in the replication scripts, which confirms
# this bit-for-bit against radf()$adf's own construction). `delta` itself
# (the correlation between the innovation and its own quantile-check
# score) is estimated directly from the data, so simulating `U(tau)`'s
# quantile is a cheap combination of a fresh standard normal draw and a
# pre-existing kind of Monte Carlo simulation, not new statistical
# machinery.

quantile_check_density <- function(u, tau) {
  b_tau <- quantile_narm(u, probs = tau, names = FALSE)
  h <- stats::bw.nrd0(u)
  f_hat <- mean(stats::dnorm((b_tau - u) / h)) / h
  list(b_tau = b_tau, f_hat = f_hat)
}

quantile_adf_tstat <- function(y) {
  n <- length(y)
  dy <- y[-1] - y[-n]
  ylag <- y[-n]
  fit <- stats::lm(dy ~ ylag)
  unname(stats::coef(summary(fit))["ylag", "t value"])
}

radf_quantile_ <- function(n, nrep, seed = NULL) {
  set_rng(seed)
  vapply(seq_len(nrep), function(i) quantile_adf_tstat(cumsum(rnorm(n))), numeric(1))
}

#' Quantile Unit Root Test for Bubble Detection (Global Test)
#'
#' \code{radf_quantile} implements the "global test" of Wu, Shi & Wu
#' (2025): a quantile-regression (QR) analogue of the Dickey-Fuller
#' t-ratio, testing for explosive behavior at a chosen conditional
#' quantile \code{tau} of \code{y_t} on \code{y_{t-1}} rather than at the
#' conditional mean. A single static test, not a recursive scan (compare
#' \code{\link{radf}}'s single-shot \code{adf} statistic, not its
#' recursive \code{bsadf}).
#'
#' @details
#' \code{tau = "optimal"} (the default) selects the quantile minimizing
#' the asymptotic variance of the QR estimator (their eq. 33) by grid
#' search over \code{tau_grid}, excluding the extreme quantiles the paper
#' itself recommends avoiding at practical sample sizes.
#'
#' The critical value is simulated per call (not a fixed table): the
#' statistic's limiting null distribution is
#' \code{sqrt(1 - delta^2) * z + delta * Q}, with \code{z ~ N(0, 1)} and
#' \code{delta} a data-estimated correlation coefficient; \code{Q} is the
#' standard demeaned Dickey-Fuller t-statistic distribution, simulated by
#' the same random-walk-plus-OLS-t-stat construction used elsewhere in
#' this package (see \code{\link{radf_mc_cv}}).
#'
#' @inheritParams radf
#' @param tau Quantile to test at, in \code{(0, 1)}, or \code{"optimal"}
#' (default) to select it via eq. 33's grid search.
#' @param tau_grid Grid searched when \code{tau = "optimal"}. Default
#' \code{seq(0.2, 0.8, by = 0.05)}, matching the paper's own recommended
#' practical range (excluding the extreme quantiles 0.1/0.9).
#' @param nrep Number of Monte Carlo replications for the critical value.
#' @param level Significance level, one of \code{90}, \code{95}, \code{99}.
#' @param seed Optional seed for the Monte Carlo draws.
#'
#' @return An object of class \code{radf_quantile_obj}: a list with the
#' test statistic \code{tstat}, the selected \code{tau}, the estimated
#' correlation \code{delta}, the simulated \code{crit} value, and
#' \code{detected} (logical, \code{tstat > crit}).
#'
#' @references Wu, R., Shi, S., & Wu, J. (2025). Quantile analysis for
#' financial bubble detection and surveillance. Journal of Time Series
#' Analysis, 46(5), 908-931.
#'
#' @seealso \code{\link{radf}} for the mean-regression (ADF/SADF/GSADF)
#' family this complements.
#'
#' @importFrom stats coef dnorm lm quantile bw.nrd0 rnorm cor
#' @export
radf_quantile <- function(data, tau = "optimal", tau_grid = seq(0.2, 0.8, by = 0.05),
                           nrep = 1000L, level = 95, seed = NULL) {
  stopifnot(level %in% c(90, 95, 99))
  x <- parse_data(data)
  n <- nrow(x)
  snames <- colnames(x)
  nc <- ncol(x)

  tstat <- crit <- delta <- setNames(rep(NA_real_, nc), snames)
  tau_used <- setNames(rep(NA_real_, nc), snames)
  detected <- setNames(rep(NA, nc), snames)
  Q <- radf_quantile_(n = n, nrep = nrep, seed = seed)

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    dy <- y[-1] - y[-n]
    ylag <- y[-n]

    tau_j <- tau
    if (identical(tau_j, "optimal")) {
      fhats <- vapply(tau_grid, function(tt) quantile_check_density(dy, tt)$f_hat, numeric(1))
      obj <- (tau_grid * (1 - tau_grid)) / fhats^2
      tau_j <- tau_grid[which.min(obj)]
    }
    if (!(tau_j > 0 && tau_j < 1)) stop_glue("'tau' must be in (0, 1) or \"optimal\".")

    qr_fit <- quantreg::rq(y ~ ylag, tau = tau_j, data = data.frame(y = y[-1], ylag = ylag))
    alpha_hat <- unname(stats::coef(qr_fit)["ylag"])

    f_hat <- quantile_check_density(dy, tau_j)$f_hat
    yPzy <- sum((ylag - mean(ylag))^2)
    tstat_j <- (f_hat / sqrt(tau_j * (1 - tau_j))) * sqrt(yPzy) * (alpha_hat - 1)

    psi <- tau_j - as.numeric(dy < quantile_narm(dy, probs = tau_j, names = FALSE))
    delta_j <- max(min(stats::cor(dy, psi), 1), -1)

    z <- stats::rnorm(nrep)
    U <- sqrt(1 - delta_j^2) * z + delta_j * Q
    crit_j <- unname(quantile_narm(U, probs = level / 100, names = FALSE))

    tstat[j] <- tstat_j; crit[j] <- crit_j; delta[j] <- delta_j
    tau_used[j] <- tau_j; detected[j] <- tstat_j > crit_j
  }

  list(
    tstat = tstat, tau = tau_used, delta = delta, crit = crit, detected = detected
  ) %>%
    add_attr(
      index = attr(x, "index"), series_names = snames, n = n,
      level = level, iter = nrep, seed = get_rng_state(seed)
    ) %>%
    add_class("radf_quantile_obj")
}

#' @export
print.radf_quantile_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_quantile (n = {attr(x, 'n')}, level = {attr(x, 'level')}%)"))
  cat_line()
  print(
    data.frame(
      series = names(x$tstat), tau = round(x$tau, 3), tstat = x$tstat,
      crit = x$crit, delta = round(x$delta, 3), detected = x$detected,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
