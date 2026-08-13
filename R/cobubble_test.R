# Co-explosive behaviour test. Evripidou, Harvey, Leybourne & Sollis (2022,
# Oxford Bulletin of Economics and Statistics, 84(3), 624-650; "EHLS"). See
# docs/enhancements/multivariate.md, "Co-bubble test", for the full
# evaluation this implements.
#
# Tests whether two series that each contain an explosive episode are
# "co-explosive": whether y_t - alpha - beta*x_{t-i} is I(0) for some lag/
# lead i, i.e. a KPSS-type stationarity test (null = co-explosive) on the
# residuals of y_t regressed on a constant and x_{t-i}, rather than an
# ADF-family right-tailed test. Heteroskedasticity-robust critical values
# come from a wild bootstrap (the null distribution of the statistic
# depends on the error volatility pattern, per Theorem 1).

# KPSS-type statistic S (eq. 3) on two already-aligned, equal-length
# vectors: sigma_y^{-2} * n^{-2} * sum_t (cumsum of OLS residuals up to
# t)^2, where the OLS regression is `y` on a constant and `xreg`. Shared by
# the raw (y, x, lag) entry point below and by the wild bootstrap, which
# regresses a bootstrap y* on the *same* xreg (Remark 2: excluding xreg
# from the bootstrap regression would make Sb's finite-sample distribution
# a worse match for S's).
coexplosive_stat_aligned <- function(y, xreg) {
  n <- length(y)
  if (n < 3L) {
    stop_glue("Series too short.")
  }
  e <- residuals(lm.fit(cbind(1, xreg), y))
  sigma2 <- mean(e^2)
  cs <- cumsum(e)
  S <- sum(cs^2) / (sigma2 * n^2)
  list(S = S, resid = e, sigma2 = sigma2, n = n)
}

# Raw-index entry point: builds the (y_t, x_{t-lag}) pair over the
# overlapping valid range implied by `lag` (t = max(lag,0)+1, ...,
# T+min(lag,0), matching the paper's index range) and calls the aligned
# core above.
coexplosive_stat <- function(y, x, lag = 0L) {
  Tn <- length(y)
  lo <- max(lag, 0L) + 1L
  hi <- Tn + min(lag, 0L)
  idx_y <- lo:hi
  idx_x <- idx_y - lag
  coexplosive_stat_aligned(y[idx_y], x[idx_x])
}

# Lag/lead selection when i is unknown (Section VI): i_hat = argmin_{j in
# lags} sigma2_hat(j), the average squared residual of the y-on-x_{t-j}
# regression -- misspecified j inflates the residual variance via the
# neglected explosive term, so the minimizer consistently recovers i.
coexplosive_select_lag <- function(y, x, lags) {
  sigma2s <- vapply(lags, function(j) coexplosive_stat(y, x, j)$sigma2, numeric(1))
  lags[which.min(sigma2s)]
}

#' Test for Co-explosive Behaviour Between Two Series
#'
#' \code{cobubble_test} tests whether two series that each contain an
#' explosive episode are \emph{co-explosive}: whether a linear combination
#' \code{y_t - alpha - beta * x_{t-lag}} is stationary, i.e. whether the
#' explosive dynamics in \code{y} and \code{x} are the same underlying
#' phenomenon (possibly migrating from one series to the other with a lead
#' or lag) rather than independent explosive episodes.
#'
#' Unlike \code{\link{radf}} (a right-tailed ADF-family test for the
#' presence of explosiveness), this is a stationarity (KPSS-type) test:
#' the null hypothesis is co-explosivity, i.e. that the residuals of
#' \code{y} regressed on a constant and \code{x_{t-lag}} are I(0). Because
#' the null limiting distribution of the statistic depends on the pattern
#' of heteroskedasticity in the errors (Evripidou, Harvey, Leybourne &
#' Sollis 2022, Theorem 1), critical values are obtained via a wild
#' bootstrap that reproduces that same heteroskedasticity pattern in the
#' bootstrap samples (Theorem 2).
#'
#' @note The critical value is a wild bootstrap of the residuals, computed
#' internally on every call (Theorem 2) -- there is no separate/reusable
#' cv function for this test.
#'
#' @param y,x Numeric vectors of equal length, or objects coercible to one
#' via \code{as.numeric()}. \code{x} is the (candidate) explosive-episode
#' regressor; \code{y} is tested for co-explosivity with \code{x_{t-lag}}.
#' @param lag The lead/lag \code{i} in \code{x_{t-lag}}. If \code{NULL}
#' (default), it is estimated from \code{lags} by minimizing the residual
#' variance (Section VI's \code{i_hat}).
#' @param lags Candidate lag values searched when \code{lag = NULL}.
#' Default \code{-6:6}, as in the paper's own simulation design.
#' @param nboot Number of wild bootstrap replications.
#' @param level Nominal test size (upper-tail rejection region).
#' @param seed Optional seed for the bootstrap draws.
#'
#' @return An object of class \code{cobubble_test}: a list with the
#' observed statistic \code{S}, the (given or estimated) \code{lag}, the
#' bootstrap critical value \code{cv} at \code{level}, the bootstrap
#' p-value \code{p_value}, and \code{reject} (\code{TRUE} if \code{S}
#' exceeds \code{cv}, i.e. co-explosivity is rejected).
#'
#' @references Evripidou, A. C., Harvey, D. I., Leybourne, S. J., & Sollis,
#' R. (2022). Testing for co-explosive behaviour in financial time series.
#' Oxford Bulletin of Economics and Statistics, 84(3), 624-650.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- cobubble_test(sim_data$sim_psy1, sim_data$sim_psy2, nboot = 199L, seed = 1)
#' print(res)
#' }
#'
#' @export
cobubble_test <- function(y, x, lag = NULL, lags = -6:6, nboot = 499L,
                           level = 0.05, seed = NULL) {
  y <- as.numeric(y)
  x <- as.numeric(x)
  if (length(y) != length(x)) {
    stop_glue("'y' and 'x' must be the same length.")
  }
  assert_positive_int(nboot, greater_than = 2)

  if (is.null(lag)) {
    lag <- coexplosive_select_lag(y, x, lags)
  }

  Tn <- length(y)
  lo <- max(lag, 0L) + 1L
  hi <- Tn + min(lag, 0L)
  idx_y <- lo:hi
  idx_x <- idx_y - lag
  xreg <- x[idx_x]

  fit <- coexplosive_stat_aligned(y[idx_y], xreg)

  set_rng(seed)
  boot_S <- numeric(nboot)
  for (b in 1:nboot) {
    ystar <- rnorm(fit$n) * fit$resid
    boot_S[b] <- coexplosive_stat_aligned(ystar, xreg)$S
  }

  cv <- quantile_narm(boot_S, 1 - level, names = FALSE)
  p_value <- mean(boot_S > fit$S)

  list(
    S = fit$S, lag = lag, cv = cv, p_value = p_value, reject = fit$S > cv
  ) %>%
    add_attr(level = level, iter = nboot, lags = lags) %>%
    add_class("cobubble_test")
}

#' @export
print.cobubble_test <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("cobubble_test (lag = {x$lag}, nboot = {attr(x, 'iter')})"))
  cat_line()
  cat_line(glue(
    "S = {format(x$S, digits = digits)}, ",
    "cv({(1 - attr(x, 'level')) * 100}%) = {format(x$cv, digits = digits)}, ",
    "p-value = {format(x$p_value, digits = digits)}"
  ))
  cat_line(glue(
    "Co-explosivity ", if (x$reject) "rejected" else "not rejected",
    " at the {attr(x, 'level') * 100}% level."
  ))
  cat_line()
}
