# Sequential sample-splitting bubble dating. Pang, Du & Chong (2021,
# journal; PDC) and its 4-regime extension by Kurozumi & Skrobotov (2023,
# journal; KS). See docs/enhancements/dating-and-root-inference.md,
# "SSR/BIC dating vs. PSY recursive dating", section 3, for the full
# evaluation this implements.
#
# Unlike HLS (2017)/HLW (2020)'s BIC-selected, jointly-fit regime-dummy
# regressions (a genuinely new, much larger piece of work -- see that
# file), PDC/KS assume a fixed 3- or 4-regime structure and estimate its
# breakpoints *sequentially*, each one a closed-form O(T) scan: PDC prove
# the collapse date is always identified first (it dominates the
# origination date in stochastic order), so there is no joint grid search
# and no BIC model-selection step at all.

# Single-breakpoint no-intercept AR(1) sample split: for y (length n),
# finds the split point k minimizing the combined residual sum of squares
# of two no-intercept AR(1) regressions of y_t on y_{t-1}, one on
# t = 2, ..., k and one on t = k+1, ..., n. `trim` is the minimum fraction
# of the (differenced) sample on either side of the break. `weights`
# (length n - 1, one per (y_{t-1}, y_t) pair) turns this into the
# GLS/WLS-weighted break search of Kurozumi & Skrobotov (2023)'s
# volatility-corrected dating estimator; NULL (default) is the plain OLS
# version of Pang, Du & Chong (2021)/Kurozumi & Skrobotov (2023)'s point
# estimator.
pdc_find_break <- function(y, trim = 0.05, weights = NULL) {
  n1 <- length(y) - 1L
  ylag <- y[1:n1]
  ycur <- y[2:(n1 + 1)]
  w <- weights %||% rep(1, n1)

  csxx <- c(0, cumsum(w * ylag^2))
  csxy <- c(0, cumsum(w * ylag * ycur))
  csyy <- c(0, cumsum(w * ycur^2))

  total_xx <- csxx[n1 + 1]
  total_xy <- csxy[n1 + 1]
  total_yy <- csyy[n1 + 1]

  k_min <- max(2L, ceiling(trim * n1))
  k_max <- n1 - k_min
  if (k_min >= k_max) {
    stop_glue("Series too short for the requested 'trim' fraction.")
  }
  ks <- k_min:k_max

  sxx_l <- csxx[ks + 1]
  sxy_l <- csxy[ks + 1]
  syy_l <- csyy[ks + 1]
  sxx_r <- total_xx - sxx_l
  sxy_r <- total_xy - sxy_l
  syy_r <- total_yy - syy_l

  rss <- (syy_l - sxy_l^2 / sxx_l) + (syy_r - sxy_r^2 / sxx_r)

  # break_idx is the index into y (1-based) after which the regime changes,
  # i.e. the break falls between y[break_idx] and y[break_idx + 1].
  list(break_idx = ks[which.min(rss)], rss = min(rss))
}

# Fitted no-intercept-AR(1) residuals of the piecewise regime model implied
# by a set of (1-based) break indices, one OLS rho per regime, evaluated at
# every (y_{t-1}, y_t) pair (same indexing as pdc_find_break()'s internal
# ylag/ycur). Feeds the nonparametric spot-volatility estimator in the WLS
# dating step (Kurozumi & Skrobotov 2023): "collect the residuals of the
# fitted [regime] model" and smooth their square nonparametrically.
pdc_regime_resid <- function(y, breaks) {
  n1 <- length(y) - 1L
  ylag <- y[1:n1]
  ycur <- y[2:(n1 + 1)]
  bounds <- c(0L, breaks, n1)

  resid <- numeric(n1)
  for (i in seq_len(length(bounds) - 1L)) {
    idx <- (bounds[i] + 1L):bounds[i + 1L]
    rho <- sum(ylag[idx] * ycur[idx]) / sum(ylag[idx]^2)
    resid[idx] <- ycur[idx] - rho * ylag[idx]
  }
  resid
}

#' Sequential Sample-Splitting Bubble Dating (PDC/KS)
#'
#' \code{radf_pdc} dates a single bubble episode using the sequential
#' sample-splitting method of Pang, Du & Chong (2021) and its 4-regime
#' extension by Kurozumi & Skrobotov (2023): a fixed regime structure
#' (unit-root, explosive, stationary-collapse, and optionally a final
#' unit-root recovery regime) whose breakpoints are estimated one at a
#' time, each a closed-form residual-sum-of-squares minimisation over a
#' no-intercept AR(1) model, in \eqn{O(T)} via cumulative sums.
#'
#' Unlike \code{\link{datestamp}} (which finds where the recursive BSADF
#' statistic crosses a critical value), this fits an explicit
#' regime-switching model directly to the series; it needs no critical
#' values at all. PDC prove the collapse date is identified first -- its
#' effect on the residual sum of squares dominates the origination date's
#' -- which is what licenses estimating the breaks sequentially rather
#' than jointly (unlike Harvey, Leybourne & Sollis's (2017) BIC-selected,
#' jointly-fit alternative, which is not implemented here; see the
#' package's enhancement notes for the cost/benefit reasoning).
#'
#' \code{type = "wls"} adds Kurozumi & Skrobotov (2023)'s time-varying-
#' volatility correction: fit the plain (\code{"ols"}) model first, collect
#' its fitted piecewise-regime residuals, smooth their square
#' nonparametrically (the same Nadaraya-Watson kernel/leave-one-out
#' bandwidth estimator exuber already uses for
#' \code{\link{radf_sbz_cv}}/\code{\link{radf_kp}}), and re-run the same
#' sequential break search with each squared term weighted by the inverse
#' of the estimated spot variance. This needs no new critical-value
#' theory -- like the OLS version, it is point estimation, not a
#' threshold-crossing test.
#'
#' @inheritParams radf
#' @param regimes Either \code{3} (PDC: unit-root, explosive, stationary
#' collapse) or \code{4} (KS: adds a final unit-root recovery regime after
#' the collapse).
#' @param trim Minimum fraction of the (differenced) sample required on
#' either side of each breakpoint search (default 0.05, as in KS's
#' empirical application; PDC use 0.05-0.1 in their simulations).
#' @param type \code{"ols"} (default) for the plain homoskedastic
#' estimator, or \code{"wls"} for Kurozumi & Skrobotov (2023)'s
#' volatility-corrected two-step estimator.
#' @param kernel Kernel for the spot-volatility estimator when
#' \code{type = "wls"}, \code{"gaussian"} (default) or \code{"uniform"}.
#' Ignored when \code{type = "ols"}.
#' @param h Bandwidth for the spot-volatility estimator when
#' \code{type = "wls"}. Default: leave-one-out cross-validation. Ignored
#' when \code{type = "ols"}.
#'
#' @return A \code{data.frame} with one row per series and columns
#' \code{origination}, \code{collapse}, and (if \code{regimes = 4})
#' \code{recovery}, giving the estimated break dates (or observation
#' indices, if no date index is available).
#'
#' @references Pang, T., Du, L., & Chong, T. T. L. (2021). Estimating
#' multiple breaks in the bubble regime with SSR minimization. Journal of
#' Management Science and Engineering.
#'
#' @references Kurozumi, E., & Skrobotov, A. (2023). Bubble dating: a
#' sequential testing approach.
#'
#' @references Kurozumi, E., & Skrobotov, A. (2023). Improving the accuracy
#' of bubble date estimators under time-varying volatility.
#' arXiv:2306.02977.
#'
#' @seealso \code{\link{datestamp}} for the PSY threshold-crossing
#' alternative.
#'
#' @export
radf_pdc <- function(data, regimes = 3L, trim = 0.05,
                      type = c("ols", "wls"),
                      kernel = c("gaussian", "uniform"), h = NULL) {
  regimes <- as.integer(regimes)
  if (!regimes %in% c(3L, 4L)) {
    stop_glue("Argument 'regimes' should be 3 or 4.")
  }
  type <- match.arg(type)
  kernel <- match.arg(kernel)
  x <- parse_data(data)
  nc <- ncol(x)
  snames <- colnames(x)
  idx <- index(x)

  to_date <- function(i) if (is.null(i)) NA else idx[i]

  # One sequential (collapse, then origination, then recovery) pass with a
  # given per-pair weight vector (NULL = OLS). Shared by the plain fit and,
  # for "wls", by the volatility-corrected refit.
  fit_sequential <- function(y, weights_full = NULL) {
    w_left <- function(b) if (is.null(weights_full)) NULL else weights_full[1:(b - 1L)]
    w_right <- function(b) if (is.null(weights_full)) NULL else weights_full[(b + 1L):length(weights_full)]

    b2 <- pdc_find_break(y, trim, weights = weights_full)$break_idx
    b1 <- pdc_find_break(y[1:b2], trim, weights = w_left(b2))$break_idx
    breaks <- c(b1, b2)
    b3 <- NULL
    if (regimes == 4L) {
      y_right <- y[(b2 + 1):length(y)]
      b3_rel <- pdc_find_break(y_right, trim, weights = w_right(b2))$break_idx
      b3 <- b2 + b3_rel
      breaks <- c(b1, b2, b3)
    }
    list(b1 = b1, b2 = b2, b3 = b3, breaks = breaks)
  }

  rows <- vector("list", nc)
  for (j in 1:nc) {
    y <- x[, j]
    fit <- fit_sequential(y)

    if (type == "wls") {
      resid <- pdc_regime_resid(y, fit$breaks)
      sigma2 <- nw_spot_vol(resid, kernel = kernel, h = h)$sigma2
      fit <- fit_sequential(y, weights_full = 1 / sigma2)
    }

    row <- list(origination = to_date(fit$b1), collapse = to_date(fit$b2))
    if (regimes == 4L) row$recovery <- to_date(fit$b3)
    rows[[j]] <- row
  }

  out <- do.call(rbind.data.frame, rows)
  rownames(out) <- snames
  out
}
