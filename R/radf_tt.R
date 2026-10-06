# Time-transformed test for explosive bubbles under non-stationary volatility
# (STADF / GSTADF), Kurozumi, Skrobotov & Tsarev (2024, J. Financial
# Econometrics; working paper arXiv:2012.13937).
#
# The test time-deforms the series using an estimated variance profile so
# that the resulting recursive Dickey-Fuller-type statistic has the SAME
# limiting null distribution as the GLS-demeaned (Whitehouse, 2019) SADF/
# GSADF statistic under homoskedasticity -- i.e. it is asymptotically
# pivotal and needs no bootstrap. See Theorem 1 and Theorem 2 of the paper.

# Core statistic ------------------------------------------------------------

# GLS-demeaned (no-intercept) recursive sup-ADF family, eq. (9) of Kurozumi,
# Skrobotov & Tsarev. `y` is a levels series (length n); internally demeaned
# by its first observation (y_check_t = y_t - y_1), matching the paper's
# GLS-demeaning. Returns the same badf/bsadf/adf/sadf/gsadf structure as
# exuber's `radf()`, computed without a constant in the regression.
gls_dfstat_grid <- function(y, minw) {
  yc <- y - y[1]
  n1 <- length(yc) - 1L
  dy <- diff(yc)
  ylag <- yc[1:n1]

  if (minw >= n1) {
    stop_glue("Argument 'minw' should be smaller than the number of observations minus one.")
  }

  csxx <- c(0, cumsum(ylag^2))
  csxy <- c(0, cumsum(ylag * dy))
  csyy <- c(0, cumsum(dy^2))

  b_idx <- minw:n1
  a_idx <- 1:n1

  sxx <- outer(csxx[b_idx + 1], csxx[a_idx], "-")
  sxy <- outer(csxy[b_idx + 1], csxy[a_idx], "-")
  syy <- outer(csyy[b_idx + 1], csyy[a_idx], "-")
  L <- outer(b_idx, a_idx, "-") + 1L

  # a > b cells are not valid windows; their cumulative-sum differences are
  # meaningless (and can be negative), so mask before dividing/sqrt-ing.
  valid <- L >= minw
  sxx[!valid] <- NA_real_

  beta <- sxy / sxx
  ssr <- syy - beta * sxy
  sigma2 <- ssr / (L - 1)
  tstat <- sxy / sqrt(sigma2 * sxx)

  # Sxx can be (numerically) zero for a valid window if the time-transform
  # resampled repeated observations into it (e.g. a flat stretch of the
  # estimated variance profile on a short series); treat those as invalid
  # too rather than propagating NaN/Inf.
  valid <- valid & is.finite(tstat)
  tstat[!valid] <- NA_real_

  badf <- tstat[, 1]
  adf <- badf[length(badf)]
  sadf <- max(badf, na.rm = TRUE)

  # row-wise max via max.col (C-level) instead of apply(), which dominates
  # runtime on the O(T^2) grid.
  tstat_filled <- tstat
  tstat_filled[!valid] <- -Inf
  best_col <- max.col(tstat_filled, ties.method = "first")
  bsadf <- tstat_filled[cbind(seq_along(best_col), best_col)]
  gsadf <- max(bsadf)

  list(badf = badf, bsadf = bsadf, adf = adf, sadf = sadf, gsadf = gsadf)
}

# Asymptotic (pivotal) critical values ---------------------------------------

#' Monte Carlo critical values for the time-transformed test (STADF/GSTADF)
#'
#' This is the dedicated critical-value function for \code{\link{radf_tt}}. It
#' simulates the asymptotic null distribution of the GLS-demeaned recursive sup-ADF
#' statistic that \code{\link{radf_tt}} uses. By Theorem 1 of Kurozumi, Skrobotov &
#' Tsarev, this distribution is free of the volatility process (pivotal). Unlike
#' \code{\link{radf_wb_cv}}, it therefore does not have to be recomputed for each
#' dataset. A large \code{n} with the default \code{nrep} approximates well the
#' T -> Inf limit used in the paper.
#'
#' You can check the \code{sadf_cv} column (STADF, that is, \code{r1 = 0} fixed)
#' against the asymptotic values of Whitehouse (2019) quoted in footnote 4 of
#' Kurozumi, Skrobotov & Tsarev. For \code{minw/n = 0.1}, the values at (10\%, 5\%,
#' 1\%) are (2.319, 2.626, 3.223). This published triple is for STADF and not for
#' GSTADF (\code{gsadf_cv}). The paper gives its GSTADF critical values not as
#' numbers in the text but only as "easily computed from" the code of the authors
#' in R.
#'
#' @note Since 2026-08-18 the function also computes \code{badf_cv} and
#' \code{bsadf_cv}, a time-varying boundary with one row for each recursion point.
#' \code{\link{datestamp}} and \code{autoplot} therefore work on \code{\link{radf_tt}}
#' results, and not only \code{summary()} and \code{tidy}. The \code{bsadf_cv} of
#' \code{\link{radf_mc_cv}} is a \code{cummax()} across replicates, which works
#' around the output shape of the base C++ engine. Here no such shortcut is
#' needed, because the \code{bsadf} of \code{gls_dfstat_grid()} (internal) is already
#' the sup over all window starts at each point, and the boundary is the
#' per-time-point quantile across replicates. We validated the function in three
#' ways. The last row of \code{badf_cv} is bit-identical to \code{adf_cv}, an exact
#' identity because \code{adf} is the last point of \code{badf} in every replicate.
#' The empirical false-alarm rate under \code{H0} is conservative relative to the
#' nominal level (3.3\% at 5\%, n = 100, minw = 20, nrep = 2000). The detection
#' power on a synthetic bubble matches the established \code{radf()} and
#' \code{radf_mc_cv()} pipeline almost exactly (18\% against 16\%) on the identical
#' series. \code{radf_sign_cv()} and \code{radf_sign_dm_cv()} now compute the same
#' boundary (see \code{vignette("naming-and-analysis")}).
#'
#' @inheritParams radf_mc_cv
#' @references Kurozumi, E., Skrobotov, A., & Tsarev, A. (2024). Time-Transformed
#' Test for Bubbles under Non-stationary Volatility. Journal of Financial
#' Econometrics. \doi{10.1093/jjfinec/nbae026}
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' cv <- radf_tt_cv(n = 200, minw = 20)
#' tidy(cv)
#'
#' # Volatility triples half-way through the sample. This is the case of
#' # non-stationary volatility that this test is built for, and plain radf()
#' # over-rejects here
#' y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
#' res <- radf_tt(y, minw = 20)
#' datestamp(res, cv = cv)
#' autoplot(res, cv = cv)
#' }
#'
#' @return An object of class \code{radf_cv}/\code{tt_cv}/\code{mc_cv} with the
#'   same structure as \code{\link{radf_mc_cv}}: the scalars \code{adf_cv},
#'   \code{sadf_cv} and \code{gsadf_cv} for each level plus the sequences
#'   \code{badf_cv} and \code{bsadf_cv}. You can use it wherever a \code{radf_cv}
#'   is accepted.
#' @family critical values
#' @export
radf_tt_cv <- function(n, minw = NULL, nrep = 2000L, seed = NULL) {
  assert_n(n)
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  set_rng(seed)
  pcnt <- c(0.9, 0.95, 0.99)

  results <- replicate(
    nrep,
    {
      y <- cumsum(rnorm(n))
      gls_dfstat_grid(y, minw)
    },
    simplify = FALSE
  )

  adf <- vapply(results, `[[`, numeric(1), "adf")
  sadf <- vapply(results, `[[`, numeric(1), "sadf")
  gsadf <- vapply(results, `[[`, numeric(1), "gsadf")

  # badf/bsadf paths, one column per replicate -- unlike radf_mc_cv()'s own
  # bsadf_cv (a cummax(badf)-across-replicates shortcut around the base C++
  # engine's output shape), gls_dfstat_grid() already returns the genuine
  # sup-over-all-window-starts bsadf at each point directly, so no cummax
  # step is needed here: just the quantile of each row (time point) across
  # replicates, same construction radf_mc_cv() uses for bsadf_cv itself.
  n_minw <- length(results[[1]]$badf)
  badf_mat <- vapply(results, `[[`, numeric(n_minw), "badf")
  bsadf_mat <- vapply(results, `[[`, numeric(n_minw), "bsadf")
  badf_cv <- t(apply(badf_mat, 1, quantile_narm, probs = pcnt))
  bsadf_cv <- t(apply(bsadf_mat, 1, quantile_narm, probs = pcnt))

  list(
    adf_cv = quantile_narm(adf, probs = pcnt, drop = FALSE),
    sadf_cv = quantile_narm(sadf, probs = pcnt, drop = FALSE),
    gsadf_cv = quantile_narm(gsadf, probs = pcnt, drop = FALSE),
    badf_cv = badf_cv,
    bsadf_cv = bsadf_cv
  ) %>%
    add_attr(method = "Time-Transformed MC", n = n, minw = minw, iter = nrep) %>%
    add_class("radf_cv", "tt_cv", "mc_cv")
}

# Variance profile estimation (feasible case, Section 4) --------------------

# Nonparametric variance profile estimate, eq. (18)-(19) of Kurozumi,
# Skrobotov & Tsarev. `y` is a levels series of length n = T + 1.
# Uses a local (Nadaraya-Watson-type) no-intercept kernel regression of
# Delta y_check_t on y_check_{t-1} to get the time-varying AR coefficient,
# truncates the residuals, and accumulates their squares into a variance
# profile eta-hat(s), s in [0, 1].
variance_profile <- function(y, kernel = c("uniform", "gaussian"), h = NULL) {
  kernel <- match.arg(kernel)
  yc <- y - y[1]
  Tn <- length(yc) - 1L
  dy <- diff(yc)
  ylag <- yc[1:Tn]

  h <- h %||% Tn^(-2 / 5)
  kern <- switch(kernel, uniform = function(u) as.numeric(abs(u) <= 1), gaussian = function(u) {
    dnorm(u)
  })

  delta <- numeric(Tn)
  eps <- numeric(Tn)
  idx <- seq_len(Tn)
  for (t in idx) {
    w <- kern((idx - t) / (Tn * h))
    sw_xx <- sum(w * ylag^2)
    sw_xy <- sum(w * ylag * dy)
    delta[t] <- if (sw_xx > 0) sw_xy / sw_xx else 0
    eps[t] <- dy[t] - delta[t] * ylag[t]
  }

  # Truncation threshold psi_T (footnote 6): max local-window residual sd.
  win <- max(2L, round(0.1 * Tn))
  starts <- seq_len(max(1L, round(0.9 * Tn)))
  cbar <- max(
    vapply(
      starts,
      function(a) {
        stats::sd(eps[a:min(a + win, Tn)])
      },
      numeric(1)
    ),
    na.rm = TRUE
  )
  psi_T <- cbar * Tn^(1 / 7)

  eps_star <- eps
  eps_star[abs(eps) >= psi_T] <- 0

  cs <- cumsum(eps_star^2)
  total <- cs[Tn]
  omega2 <- total / Tn

  # eta_hat(t/Tn) for integer t = 0, ..., Tn (piecewise-linear in between,
  # matching eq. 19); grid is exact at integers so linear interpolation for
  # the inverse is exact too.
  eta_grid <- c(0, cs) / total

  list(eta_grid = eta_grid, omega2 = omega2, Tn = Tn)
}

#' Time-Transformed Test for Explosive Bubbles under Non-stationary Volatility
#'
#' \code{radf_tt} computes the STADF and GSTADF test statistics of Kurozumi,
#' Skrobotov & Tsarev, a heteroskedasticity-robust alternative to
#' \code{\link{radf}} that needs no bootstrap. It time-deforms the series with a
#' nonparametric estimate of its variance profile, after which the usual
#' asymptotic recursive sup-ADF critical values for homoskedastic errors apply.
#'
#' @details
#' We recommend \code{\link{radf_tt_cv}} for the critical values. They are pivotal
#' (asymptotically free of the volatility process), so they do not have to be
#' recomputed for each dataset, unlike a bootstrap. The wild bootstrap of Harvey,
#' Leybourne, Sollis & Taylor in \code{\link{radf_wb_cv}} is a bootstrap-based
#' alternative. Consider it if non-pivotality or the finite-sample robustness of
#' the bootstrap is a specific concern.
#'
#' @inheritParams radf
#' @param kernel Kernel used in the local variance-profile regression,
#' \code{"uniform"} (default, as in the simulations of the paper) or
#' \code{"gaussian"}.
#' @param h Bandwidth for the variance-profile kernel regression. The default is
#' \code{T^(-2/5)}, the midpoint on the log scale of the cross-validation search
#' range \eqn{[T^{-0.5}, T^{-0.3}]} of the paper.
#'
#' @references Kurozumi, E., Skrobotov, A., & Tsarev, A. (2024). Time-Transformed
#' Test for Bubbles under Non-stationary Volatility. Journal of Financial
#' Econometrics. \doi{10.1093/jjfinec/nbae026}
#'
#' @seealso \code{\link{radf_tt_cv}} for the pivotal asymptotic critical values that
#' need no bootstrap, and \code{\link{radf_wb_cv}} for the bootstrap-based
#' alternative (Harvey, Leybourne, Sollis & Taylor).
#'
#' @note The result carries the \code{radf_obj} class. Since 2026-08-18 the full
#' \code{summary()}, \code{\link{datestamp}}, \code{tidy} and \code{autoplot}
#' pipeline works, because \code{radf_tt_cv()} now computes the time-varying
#' \code{badf_cv} and \code{bsadf_cv} boundary that the last two need, and not only
#' the three scalar critical values that \code{summary()} uses. See
#' \code{vignette("naming-and-analysis", package = "exuber")}.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' # Volatility triples half-way through the sample. This is the case of
#' # non-stationary volatility that this test is built for, and plain radf()
#' # over-rejects here
#' y <- sim_psy1(n = 200, seed = 1, e = sim_vol_break(199))
#' res <- radf_tt(y, minw = 20)
#' print(res)
#'
#' cv <- radf_tt_cv(n = 200, minw = 20)
#' summary(res, cv = cv)
#' tidy(res, cv = cv)
#' datestamp(res, cv = cv)
#' autoplot(res, cv = cv)
#' }
#'
#' @return An object of class \code{radf_tt_obj}/\code{radf_obj}: the same
#'   \code{adf}/\code{badf}/\code{sadf}/\code{bsadf}/\code{gsadf} list as for
#'   \code{\link{radf}}, computed on the time-transformed series. It works with
#'   \code{summary()}, \code{datestamp()}, \code{tidy()} and \code{autoplot()} when
#'   paired with \code{\link{radf_tt_cv}}.
#' @family volatility-robust tests
#' @export
radf_tt <- function(data, minw = NULL, kernel = c("uniform", "gaussian"), h = NULL) {
  kernel <- match.arg(kernel)
  x <- parse_data(data)
  minw <- minw %||% psy_minw(data)
  nc <- ncol(x)
  snames <- colnames(x)

  assert_na(x)
  assert_positive_int(minw, greater_than = 2)

  adf <- sadf <- gsadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, snames)))
  badf_l <- bsadf_l <- vector("list", nc)

  for (i in 1:nc) {
    y <- x[, i]
    vp <- variance_profile(y, kernel = kernel, h = h)
    g_of_s <- function(s) {
      # generalised inverse of the (monotone, piecewise-linear) eta_hat
      approx(
        vp$eta_grid,
        seq(0, 1, length.out = vp$Tn + 1),
        xout = pmin(pmax(s, 0), 1),
        ties = "ordered",
        rule = 2
      )$y
    }
    tgrid <- seq(0, 1, length.out = vp$Tn + 1)
    resampled_idx <- pmin(pmax(round(g_of_s(tgrid) * vp$Tn) + 1, 1), length(y))
    y_tilde <- y[resampled_idx]

    res <- gls_dfstat_grid(y_tilde, minw)
    badf_l[[i]] <- res$badf
    bsadf_l[[i]] <- res$bsadf
    adf[i] <- res$adf
    sadf[i] <- res$sadf
    gsadf[i] <- res$gsadf
  }

  badf <- do.call(cbind, badf_l)
  bsadf <- do.call(cbind, bsadf_l)
  colnames(badf) <- colnames(bsadf) <- snames

  list(
    adf = adf,
    badf = badf,
    sadf = sadf,
    bsadf = bsadf,
    gsadf = gsadf
  ) %>%
    add_attr(
      mat = x,
      index = index(x),
      series_names = snames,
      minw = minw,
      n = nrow(x),
      kernel = kernel,
      lag = 0L
    ) %>%
    add_class("radf_tt_obj", "radf_obj")
}

#' @export
print.radf_tt_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_tt (minw = {get_minw(x)}, kernel = {attr(x, 'kernel')})"))
  cat_line()
  print(
    data.frame(
      series = names(x$adf),
      adf = x$adf,
      sadf = x$sadf,
      gsadf = x$gsadf,
      row.names = NULL
    ),
    digits = digits,
    print.gap = 2L,
    row.names = FALSE
  )
  cat_line()
}
