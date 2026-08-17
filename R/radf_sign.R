# Sign-based bubble test. Harvey, Leybourne & Zu (2020, Econometric
# Theory, 36(1), 122-169; "HLZ"). See docs/enhancements/volatility-
# robustness.md, "Sign-based sGSADF", for the full evaluation this
# implements.
#
# Exact invariance to (even time-varying) volatility: transform the
# series to the cumulated sign of its first differences,
# C_t := sum_{i<=t} sign(Delta y_i), which strips out all magnitude
# information from Delta y_t and keeps only its sign -- so the recursive
# DF statistic computed on C_t is exactly invariant to the pattern of
# heteroskedasticity, with no bootstrap needed at all (unlike HLST's
# radf_wb_cv() for the standard PSY test). The no-intercept recursive
# t-ratio machinery this needs is exactly gls_dfstat_grid(), already
# implemented and tested for STADF (radf_tt.R) -- reused unchanged here.
#
# Level-shift robustness (Harvey, Leybourne, Tatlow & Zu 2025, OBES
# 87(5), 879-901, doi:10.1111/obes.12668; "HLTZ"). See volatility-
# robustness.md, "Sign-based sGSADF", "Level-shift robustness (HLTZ
# 2025)" for the full re-triage and validation. HLTZ's own Theorems 2-3
# (full PDF read, not the abstract -- rendered pages 3-6, since raw text
# extraction scrambles the theorem statements) show sPWY/sPSY (below)
# and the second, recursively demeaned sign-based analogue (denoted
# s-bar-PWY/s-bar-PSY in the paper; radf_sign_dm(), below) share the
# SAME asymptotic level-shift robustness: both retain their standard
# (no-shift) null distribution whenever the number of level shifts
# grows strictly slower than sqrt(T) (their alpha_n < 1/2),
# *regardless of shift magnitude* -- unlike the standard PSY test,
# which needs a joint restriction on BOTH the number and the magnitude
# of shifts (their Theorem 1, Assumption 3) and, per their Table 1, is
# never correctly sized in their Case 1 DGP (number of shifts ~
# sqrt(T)). Only at the boundary rate (shifts ~ sqrt(T) exactly,
# alpha_n = 1/2) do the sign-based tests also pick up a shift-dependent
# term and become oversized, but HLTZ show (Remark 4) that term is
# bounded independent of shift magnitude, so the over-sizing does not
# grow with it the way PSY's does. No new estimation machinery needed
# for either sign-based test -- both are still gls_dfstat_grid() on a
# transformed series; the only genuinely new piece is
# sign_demean_transform() below.

sign_transform <- function(y) {
  c(0, cumsum(sign(diff(y))))
}

# Second sign-based analogue of HLZ (2020), C-tilde_t := sum_{i=2}^t
# {sign(dy_i) - (i-1)^{-1} sum_{j=2}^i sign(dy_j)} -- a recursive (expanding
# -window, not (r1,r2)-window) demeaning of the sign series, computed once
# up front rather than per candidate window. sum_{j=2}^i sign(dy_j) is just
# C_i (sign_transform()'s own running sum), so the demeaning term at each i
# is simply C_i/(i-1); verified against a brute-force per-i loop, see
# test-sign.R.
sign_demean_transform <- function(y) {
  s <- sign(diff(y))
  n <- length(s)
  cs <- cumsum(s)
  c(0, cumsum(s - cs / seq_len(n)))
}

#' Sign-Based Bubble Test (sPWY / sPSY)
#'
#' \code{radf_sign} computes Harvey, Leybourne & Zu (2020)'s sign-based
#' variant of the recursive right-tailed unit root test: instead of
#' applying the (double-)supremum ADF test directly to the series, it is
#' applied to the cumulated sign of its first differences,
#' \code{C_t = sum(sign(diff(y)))}. Because \code{sign()} strips out all
#' magnitude information, \code{C_t}'s recursive DF statistic is *exactly*
#' invariant to the pattern of (even time-varying) volatility in the
#' innovations -- unlike \code{\link{radf}}, no wild bootstrap is needed to
#' control size under heteroskedasticity; \code{\link{radf_sign_cv}}'s
#' critical values are pivotal, computed once rather than per dataset.
#'
#' The cost of this invariance is power: the paper finds the sign-based
#' test outperforms the standard PSY test for many time-varying-volatility
#' and bubble specifications, but not all -- the standard test can still
#' win for some. The paper's own recommended practical strategy is a
#' bootstrap-based union-of-rejections combining both tests, which is
#' \strong{not} implemented here (see the package's enhancement notes for
#' the cost/benefit reasoning); this function provides the standalone
#' sign-based test only. \code{sadf} is the single-supremum (\code{r1 = 0}
#' fixed) sPWY statistic; \code{gsadf} is the double-supremum sPSY
#' statistic.
#'
#' @inheritParams radf
#'
#' @references Harvey, D. I., Leybourne, S. J., & Zu, Y. (2020). Sign-based
#' unit root tests for explosive financial bubbles in the presence of
#' deterministically time-varying volatility. Econometric Theory, 36(1),
#' 122-169.
#' @references Harvey, D. I., Leybourne, S. J., Tatlow, D., & Zu, Y. (2025).
#' Unit root tests for explosive financial bubbles in the presence of
#' deterministic level shifts. Oxford Bulletin of Economics and Statistics,
#' 87(5), 879-901. \doi{10.1111/obes.12668}
#'
#' @section Level-shift robustness:
#' Harvey, Leybourne, Tatlow & Zu (2025) show this test also retains its
#' standard (no-level-shift) null distribution in the presence of
#' deterministic level shifts, provided the number of shifts grows
#' strictly slower than \code{sqrt(T)} -- regardless of how large the
#' shifts are. This is a materially weaker requirement than the standard
#' PSY test needs for its own size control, which restricts the number
#' \strong{and} the magnitude of shifts jointly; in their simulations the
#' standard test is never correctly sized once the number of shifts grows
#' at rate \code{sqrt(T)}, while this test stays close to nominal size.
#'
#' @seealso \code{\link{radf_sign_cv}} for critical values,
#' \code{\link{radf_sign_dm}} for the recursively demeaned sign-based
#' analogue (sharing the same level-shift robustness), and \code{\link{radf}}
#' for the standard (non-invariant) test.
#'
#' @note Needs \code{\link{radf_sign_cv}} for critical values, not
#' \code{\link{radf_wb_cv}} or any other bootstrap -- the statistic is
#' pivotal (exactly invariant to heteroskedasticity), so its critical
#' values are simulated once, not per dataset.
#'
#' @note Carries the \code{radf_obj} class, so \code{summary()} and
#' \code{tidy()} work, but \code{\link{datestamp}}/\code{autoplot} do not:
#' \code{radf_sign_cv()} only computes the three scalar critical values
#' \code{summary()} needs, not the time-varying boundary those two require.
#' A known gap, not a design choice -- see \code{vignette("naming-and-
#' analysis", package = "exuber")}.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- radf_sign(sim_data, minw = 20)
#' print(res)
#'
#' cv <- radf_sign_cv(n = 100, minw = 20)
#' summary(res, cv = cv)
#' tidy(res, cv = cv)
#' }
#'
#' @export
radf_sign <- function(data, minw = NULL) {
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
    res <- gls_dfstat_grid(sign_transform(y), minw)
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
    adf = adf, badf = badf, sadf = sadf, bsadf = bsadf, gsadf = gsadf
  ) %>%
    add_attr(
      mat = x, index = index(x), series_names = snames, minw = minw, n = nrow(x), lag = 0L
    ) %>%
    add_class("radf_sign_obj", "radf_obj")
}

#' @export
print.radf_sign_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_sign (minw = {get_minw(x)})"))
  cat_line()
  print(
    data.frame(series = names(x$adf), adf = x$adf, sadf = x$sadf, gsadf = x$gsadf,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}

#' Monte Carlo Critical Values for the Sign-Based Test
#'
#' Simulates the asymptotic null distribution of \code{\link{radf_sign}}'s
#' statistic. Per Theorem 2 of Harvey, Leybourne & Zu (2020), this
#' distribution does not depend on the volatility process at all (exact
#' invariance) -- so, like \code{\link{radf_tt_cv}} and unlike
#' \code{\link{radf_wb_cv}}, it does not need to be recomputed per
#' dataset: a large \code{n} with the default \code{nrep} approximates the
#' paper's own \code{T -> Inf} limit.
#'
#' \code{sadf_cv} (single-supremum, \code{r1 = 0} fixed) can be checked
#' against the paper's Table 1 asymptotic (\code{T = Inf}) sPWY values:
#' for \code{minw/n = 0.1}, (10\%, 5\%, 1\%) = (2.410, 2.734, 3.248).
#' \code{gsadf_cv} (double-supremum) corresponds to the sPSY row:
#' (2.933, 3.180, 3.655).
#'
#' @inheritParams radf_mc_cv
#'
#' @references Harvey, D. I., Leybourne, S. J., & Zu, Y. (2020). Sign-based
#' unit root tests for explosive financial bubbles in the presence of
#' deterministically time-varying volatility. Econometric Theory, 36(1),
#' 122-169.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' cv <- radf_sign_cv(n = 100, minw = 20)
#' tidy(cv)
#' }
#'
#' @export
radf_sign_cv <- function(n, minw = NULL, nrep = 2000L, seed = NULL) {
  assert_n(n)
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  set_rng(seed)
  pcnt <- c(0.9, 0.95, 0.99)

  results <- replicate(nrep, {
    y <- cumsum(rnorm(n))
    gls_dfstat_grid(sign_transform(y), minw)
  }, simplify = FALSE)

  adf <- vapply(results, `[[`, numeric(1), "adf")
  sadf <- vapply(results, `[[`, numeric(1), "sadf")
  gsadf <- vapply(results, `[[`, numeric(1), "gsadf")

  list(
    adf_cv = quantile_narm(adf, probs = pcnt, drop = FALSE),
    sadf_cv = quantile_narm(sadf, probs = pcnt, drop = FALSE),
    gsadf_cv = quantile_narm(gsadf, probs = pcnt, drop = FALSE)
  ) %>%
    add_attr(method = "Sign-Based MC", n = n, minw = minw, iter = nrep) %>%
    add_class("radf_cv", "sign_cv", "mc_cv")
}

#' Recursively Demeaned Sign-Based Bubble Test (s-bar-PWY / s-bar-PSY)
#'
#' \code{radf_sign_dm} computes Harvey, Leybourne & Zu (2020)'s second
#' sign-based analogue of the recursive right-tailed unit root test,
#' denoted \eqn{\bar{s}PWY}/\eqn{\bar{s}PSY} in the paper: the same
#' construction as \code{\link{radf_sign}}, but built on a recursively
#' (expanding-window) demeaned cumulated-sign series, \code{Ctilde_t =
#' sum_{i=2}^{t} (sign(diff(y)_i) - mean(sign(diff(y)_{2:i})))}, rather
#' than the raw cumulated sign \code{radf_sign} uses.
#'
#' Harvey, Leybourne, Tatlow & Zu (2025) show this statistic shares
#' \code{\link{radf_sign}}'s asymptotic level-shift robustness (see that
#' function's \verb{Level-shift robustness} section) without requiring
#' Assumption 2 of the underlying HLZ (2020) theory (that the innovations'
#' median is zero) -- a strictly weaker requirement than
#' \code{\link{radf_sign}} needs for its own invariance result. Their finite
#' -sample simulations also find the recursive demeaning tends to further
#' reduce size distortion under level shifts relative to \code{radf_sign},
#' though both are asymptotically level-shift robust under the same
#' condition.
#'
#' @inheritParams radf
#'
#' @references Harvey, D. I., Leybourne, S. J., & Zu, Y. (2020). Sign-based
#' unit root tests for explosive financial bubbles in the presence of
#' deterministically time-varying volatility. Econometric Theory, 36(1),
#' 122-169.
#' @references Harvey, D. I., Leybourne, S. J., Tatlow, D., & Zu, Y. (2025).
#' Unit root tests for explosive financial bubbles in the presence of
#' deterministic level shifts. Oxford Bulletin of Economics and Statistics,
#' 87(5), 879-901. \doi{10.1111/obes.12668}
#'
#' @seealso \code{\link{radf_sign_dm_cv}} for critical values, and
#' \code{\link{radf_sign}} for the non-demeaned sign-based analogue.
#'
#' @note Needs \code{\link{radf_sign_dm_cv}} for critical values (not
#' \code{\link{radf_sign_cv}}, which is calibrated to the non-demeaned
#' \code{\link{radf_sign}} statistic instead) -- pivotal like
#' \code{radf_sign}, so no per-dataset bootstrap is needed.
#'
#' @note Carries the \code{radf_obj} class, so \code{summary()} and
#' \code{tidy()} work, but \code{\link{datestamp}}/\code{autoplot} do not
#' -- same gap as \code{\link{radf_sign}}: \code{radf_sign_dm_cv()} only
#' computes the three scalar critical values, not a time-varying boundary.
#' See \code{vignette("naming-and-analysis", package = "exuber")}.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- radf_sign_dm(sim_data, minw = 20)
#' print(res)
#'
#' cv <- radf_sign_dm_cv(n = 100, minw = 20)
#' summary(res, cv = cv)
#' tidy(res, cv = cv)
#' }
#'
#' @export
radf_sign_dm <- function(data, minw = NULL) {
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
    res <- gls_dfstat_grid(sign_demean_transform(y), minw)
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
    adf = adf, badf = badf, sadf = sadf, bsadf = bsadf, gsadf = gsadf
  ) %>%
    add_attr(
      mat = x, index = index(x), series_names = snames, minw = minw, n = nrow(x), lag = 0L
    ) %>%
    add_class("radf_sign_dm_obj", "radf_obj")
}

#' @export
print.radf_sign_dm_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_sign_dm (minw = {get_minw(x)})"))
  cat_line()
  print(
    data.frame(series = names(x$adf), adf = x$adf, sadf = x$sadf, gsadf = x$gsadf,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}

#' Monte Carlo Critical Values for the Recursively Demeaned Sign-Based Test
#'
#' Simulates the asymptotic null distribution of \code{\link{radf_sign_dm}}'s
#' statistic. Like \code{\link{radf_sign_cv}}, this distribution does not
#' depend on the volatility process (exact invariance, HLZ 2020's Theorem 2
#' analogue for this variant), so it does not need to be recomputed per
#' dataset.
#'
#' @inheritParams radf_mc_cv
#'
#' @references Harvey, D. I., Leybourne, S. J., & Zu, Y. (2020). Sign-based
#' unit root tests for explosive financial bubbles in the presence of
#' deterministically time-varying volatility. Econometric Theory, 36(1),
#' 122-169.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' cv <- radf_sign_dm_cv(n = 100, minw = 20)
#' tidy(cv)
#' }
#'
#' @export
radf_sign_dm_cv <- function(n, minw = NULL, nrep = 2000L, seed = NULL) {
  assert_n(n)
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  set_rng(seed)
  pcnt <- c(0.9, 0.95, 0.99)

  results <- replicate(nrep, {
    y <- cumsum(rnorm(n))
    gls_dfstat_grid(sign_demean_transform(y), minw)
  }, simplify = FALSE)

  adf <- vapply(results, `[[`, numeric(1), "adf")
  sadf <- vapply(results, `[[`, numeric(1), "sadf")
  gsadf <- vapply(results, `[[`, numeric(1), "gsadf")

  list(
    adf_cv = quantile_narm(adf, probs = pcnt, drop = FALSE),
    sadf_cv = quantile_narm(sadf, probs = pcnt, drop = FALSE),
    gsadf_cv = quantile_narm(gsadf, probs = pcnt, drop = FALSE)
  ) %>%
    add_attr(method = "Sign-Based MC (demeaned)", n = n, minw = minw, iter = nrep) %>%
    add_class("radf_cv", "sign_dm_cv", "mc_cv")
}
