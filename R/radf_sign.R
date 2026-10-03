# Sign-based bubble test. Harvey, Leybourne & Zu (2020, Econometric
# Theory, 36(1), 122-169; "HLZ"). See docs/volatility-
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
#' \code{radf_sign} computes the sign-based variant of the recursive right-tailed
#' unit root test of Harvey, Leybourne & Zu (2020). Instead of applying the
#' (double-)supremum ADF test to the series itself, it applies it to the cumulated
#' sign of the first differences, \code{C_t = sum(sign(diff(y)))}. The \code{sign()}
#' function removes all information about magnitudes, so the recursive DF statistic
#' of \code{C_t} is \emph{exactly} invariant to the pattern of volatility in the
#' innovations, even when volatility changes over time. Unlike \code{\link{radf}},
#' the test needs no wild bootstrap to control its size under heteroskedasticity.
#' The critical values in \code{\link{radf_sign_cv}} are pivotal, so they are
#' computed once and not for each dataset.
#'
#' The price of this invariance is power. The paper finds that the sign-based test
#' outperforms the standard PSY test for many specifications of time-varying
#' volatility and bubbles, but not for all of them, and the standard test can still
#' win in some. The strategy that the paper recommends in practice is a
#' bootstrap-based union of rejections that combines both tests. We have not
#' implemented it (see the package's enhancement notes for the cost and benefit
#' considerations), and this function provides the standalone sign-based test only.
#' \code{sadf} is the single-supremum sPWY statistic (\code{r1 = 0} fixed), and
#' \code{gsadf} is the double-supremum sPSY statistic.
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
#' Harvey, Leybourne, Tatlow & Zu (2025) show that this test keeps its standard
#' null distribution, the one without level shifts, in the presence of
#' deterministic level shifts, provided that the number of shifts grows strictly
#' more slowly than \code{sqrt(T)}. The size of the shifts does not matter. This
#' is a materially weaker requirement than the one the standard PSY test needs for
#' size control, which restricts the number \strong{and} the magnitude of the shifts
#' jointly. In their simulations the standard test is never correctly sized once
#' the number of shifts grows at rate \code{sqrt(T)}, while this test stays close to
#' its nominal size.
#'
#' @seealso \code{\link{radf_sign_cv}} for critical values,
#' \code{\link{radf_sign_dm}} for the recursively demeaned sign-based analogue,
#' which has the same level-shift robustness, and \code{\link{radf}} for the
#' standard test, which is not invariant.
#'
#' @note The test needs the critical values from \code{\link{radf_sign_cv}}, and
#' neither \code{\link{radf_wb_cv}} nor any other bootstrap applies. The statistic is
#' pivotal (exactly invariant to heteroskedasticity), so its critical values are
#' simulated once and not for each dataset.
#'
#' @note The result carries the \code{radf_obj} class. Since 2026-08-18 the full
#' \code{summary()}, \code{\link{datestamp}}, \code{tidy} and \code{autoplot}
#' pipeline works, because \code{radf_sign_cv()} now computes the time-varying
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
#' res <- radf_sign(y, minw = 20)
#' print(res)
#'
#' cv <- radf_sign_cv(n = 200, minw = 20)
#' summary(res, cv = cv)
#' tidy(res, cv = cv)
#' datestamp(res, cv = cv)
#' autoplot(res, cv = cv)
#' }
#'
#' @return An object of class \code{radf_sign_obj}/\code{radf_obj}. It is the same
#'   \code{adf}/\code{badf}/\code{sadf}/\code{bsadf}/\code{gsadf} list as for
#'   \code{\link{radf}}, computed on the sign-transformed series, and it pairs with
#'   \code{\link{radf_sign_cv}}.
#' @family volatility-robust tests
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
#' Simulates the asymptotic null distribution of the statistic of
#' \code{\link{radf_sign}}. By Theorem 2 of Harvey, Leybourne & Zu (2020), this
#' distribution does not depend on the volatility process at all (exact
#' invariance). Like \code{\link{radf_tt_cv}}, and unlike \code{\link{radf_wb_cv}},
#' it therefore does not have to be recomputed for each dataset. A large \code{n}
#' with the default \code{nrep} approximates the \code{T -> Inf} limit of the
#' paper.
#'
#' You can check \code{sadf_cv} (single-supremum, \code{r1 = 0} fixed) against the
#' asymptotic (\code{T = Inf}) sPWY values in Table 1 of the paper. For
#' \code{minw/n = 0.1}, the values at (10\%, 5\%, 1\%) are (2.410, 2.734, 3.248).
#' \code{gsadf_cv} (double-supremum) corresponds to the sPSY row: (2.933, 3.180,
#' 3.655).
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
#' cv <- radf_sign_cv(n = 200, minw = 20)
#' tidy(cv)
#' }
#'
#' @return An object of class \code{radf_cv}/\code{sign_cv}/\code{mc_cv} with
#'   the same structure as \code{\link{radf_mc_cv}}.
#' @family critical values
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

  # badf/bsadf critical values, same construction as radf_tt_cv(): each
  # replicate's gls_dfstat_grid() already returns the genuine sup-over-all-
  # window-starts bsadf at each point (no cummax shortcut needed, unlike
  # radf_mc_cv()'s own bsadf_cv), so just the per-time-point quantile
  # across replicates.
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
    add_attr(method = "Sign-Based MC", n = n, minw = minw, iter = nrep) %>%
    add_class("radf_cv", "sign_cv", "mc_cv")
}

#' Recursively Demeaned Sign-Based Bubble Test (s-bar-PWY / s-bar-PSY)
#'
#' \code{radf_sign_dm} computes the second sign-based analogue of the recursive
#' right-tailed unit root test of Harvey, Leybourne & Zu (2020), which the paper
#' denotes \eqn{\bar{s}PWY}/\eqn{\bar{s}PSY}. The construction is the same as in
#' \code{\link{radf_sign}}, but it is built on a recursively (expanding-window)
#' demeaned cumulated-sign series, \code{Ctilde_t = sum_{i=2}^{t}
#' (sign(diff(y)_i) - mean(sign(diff(y)_{2:i})))}, and not on the raw cumulated
#' sign that \code{radf_sign} uses.
#'
#' Harvey, Leybourne, Tatlow & Zu (2025) show that this statistic shares the
#' asymptotic level-shift robustness of \code{\link{radf_sign}} (see the
#' \verb{Level-shift robustness} section of that function). It does not need
#' Assumption 2 of the underlying HLZ (2020) theory, that the median of the
#' innovations is zero, which is a strictly weaker requirement than the one
#' \code{\link{radf_sign}} needs for its own invariance result. Their finite-sample
#' simulations also find that the recursive demeaning tends to reduce the size
#' distortion under level shifts further than \code{radf_sign} does, although both
#' are asymptotically robust to level shifts under the same condition.
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
#' @note The test needs the critical values from \code{\link{radf_sign_dm_cv}}, and
#' \code{\link{radf_sign_cv}} does not apply, because it is calibrated to the
#' non-demeaned \code{\link{radf_sign}} statistic. The statistic is pivotal like
#' \code{radf_sign}, so no bootstrap is needed for each dataset.
#'
#' @note The result carries the \code{radf_obj} class. Since 2026-08-18 the full
#' \code{summary()}, \code{\link{datestamp}}, \code{tidy} and \code{autoplot}
#' pipeline works, with the same fix as for \code{\link{radf_sign}}:
#' \code{radf_sign_dm_cv()} now also computes \code{badf_cv} and \code{bsadf_cv},
#' and not only the three scalar critical values. See
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
#' res <- radf_sign_dm(y, minw = 20)
#' print(res)
#'
#' cv <- radf_sign_dm_cv(n = 200, minw = 20)
#' summary(res, cv = cv)
#' tidy(res, cv = cv)
#' datestamp(res, cv = cv)
#' autoplot(res, cv = cv)
#' }
#'
#' @return An object of class \code{radf_sign_dm_obj}/\code{radf_obj}. It is the same
#'   \code{adf}/\code{badf}/\code{sadf}/\code{bsadf}/\code{gsadf} list as for
#'   \code{\link{radf}}, and it pairs with \code{\link{radf_sign_dm_cv}}.
#' @family volatility-robust tests
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
#' Simulates the asymptotic null distribution of the statistic of
#' \code{\link{radf_sign_dm}}. As for \code{\link{radf_sign_cv}}, this distribution
#' does not depend on the volatility process (exact invariance, the analogue for
#' this variant of Theorem 2 of HLZ 2020), so it does not have to be recomputed for
#' each dataset.
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
#' cv <- radf_sign_dm_cv(n = 200, minw = 20)
#' tidy(cv)
#' }
#'
#' @return An object of class \code{radf_cv}/\code{sign_dm_cv}/\code{mc_cv}
#'   with the same structure as \code{\link{radf_mc_cv}}.
#' @family critical values
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

  # badf/bsadf critical values -- see radf_sign_cv()'s identical comment.
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
    add_attr(method = "Sign-Based MC (demeaned)", n = n, minw = minw, iter = nrep) %>%
    add_class("radf_cv", "sign_dm_cv", "mc_cv")
}
