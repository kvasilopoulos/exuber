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

sign_transform <- function(y) {
  c(0, cumsum(sign(diff(y))))
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
#'
#' @seealso \code{\link{radf_sign_cv}} for critical values, and
#' \code{\link{radf}} for the standard (non-invariant) test.
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
      index = index(x), series_names = snames, minw = minw, n = nrow(x)
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
    add_class("radf_cv", "sign_cv")
}
