# Common-bubble detection via PCA + PSY. Chen, Y., Phillips, P.C.B. & Shi, S.
# (2023, J. Financial Econometrics 21(4), 989-1063; open working paper:
# Cowles Foundation DP 2251).
#
# Theorem 4.3 of the paper states the PSY statistic computed on the panel's
# first principal component has a limiting null distribution "identical to
# that of the original PSY statistic" -- so, unlike a genuinely new
# statistic (SBZ, STADF), this needs no new critical-value machinery at
# all: PCA down to one series, then call exuber's own radf()/radf_mc_cv()
# unmodified. (The paper's own finite-sample simulations still use
# simulated, not asymptotic, critical values for small N -- exactly what
# radf_mc_cv() already computes for any given (n, minw), so this carries
# over directly, not just asymptotically.)

#' Common-Bubble Detection via PCA + PSY
#'
#' \code{radf_common} tests for a bubble common to a panel of series
#' (Chen, Phillips & Shi, 2023): it extracts the panel's first principal
#' component and runs the ordinary \code{\link{radf}} test on it. Per the
#' paper's Theorem 4.3, the resulting statistic's null limiting distribution
#' is identical to the standard PSY/GSADF one, so \code{\link{radf_mc_cv}}
#' (or \code{\link{radf_wb_cv}}, for heteroskedasticity robustness) applies
#' directly to the result with no modification -- and every downstream
#' method (\code{tidy()}, \code{autoplot()}, \code{datestamp()}, ...) works
#' on it for free, since the output is an ordinary \code{radf_obj}.
#'
#' @inheritParams radf
#' @param r Number of principal components to extract (default 1, the
#' paper's own recommendation: "sufficient... for the purpose of bubble
#' identification"). Only the first is used for detection; the rest are
#' returned for inspection via the \code{"prcomp"} attribute.
#'
#' @return A \code{radf_obj} (see \code{\link{radf}}) computed on the panel's
#' first principal component, with the fitted \code{prcomp} object attached
#' as an attribute (\code{attr(x, "prcomp")}).
#'
#' @references Chen, Y., Phillips, P. C. B., & Shi, S. (2023). Common
#' Bubble Detection in Large Dimensional Financial Systems. Journal of
#' Financial Econometrics, 21(4), 989-1063.
#'
#' @seealso \code{\link{radf}} for the underlying (unmodified) test, and
#' \code{\link{radf_mc_cv}} for its critical values.
#'
#' @export
radf_common <- function(data, minw = NULL, r = 1) {
  x <- parse_data(data)
  assert_na(x)
  if (ncol(x) < 2) {
    stop_glue("radf_common needs a panel of at least 2 series.")
  }

  pca <- stats::prcomp(x, center = TRUE, scale. = FALSE)
  factor_series <- pca$x[, 1]

  radf(factor_series, minw = minw) %>%
    add_attr(prcomp = pca)
}

# Independent validation (2026-08-09, see docs/enhancements/multivariate.md)
# found that Theorem 4.3's asymptotic null-distribution identity with plain
# univariate GSADF does NOT hold at practical panel widths N -- and the gap
# *grows* with N (more than doubling by N=100), because PCA on a panel of
# merely independent (non-cointegrated) I(1) series doesn't behave like a
# single random walk once there are more series to draw transient
# co-movement from. radf_mc_cv() has no N argument at all, so it cannot be
# a correct critical value for radf_common() at any realistic N -- this
# function simulates the null radf_common() actually needs: an N-column
# panel of independent random walks, same PCA + GSADF procedure.

#' Critical Values for the Common-Bubble (PCA + PSY) Test
#'
#' \code{radf_common_cv} simulates critical values for \code{\link{radf_common}}
#' under its own null (no common explosive factor): an \code{N}-column panel
#' of \emph{independent} random walks, extracted to one principal component
#' and tested exactly as \code{\link{radf_common}} does. Unlike
#' \code{\link{radf_mc_cv}} -- which has no dependence on panel width and was
#' shown by independent validation to be badly undersized as a stand-in for
#' \code{radf_common}'s own null once \code{N} grows past a handful of
#' series -- this null distribution does depend on \code{N}, so \code{N}
#' must match the panel \code{\link{radf_common}} was actually run on.
#'
#' @param n A positive integer. The sample size (number of time periods).
#' @param N A positive integer, at least 2. The panel width (number of
#' series) that \code{\link{radf_common}} will be run on -- the critical
#' value depends on this, unlike \code{\link{radf_mc_cv}}.
#' @inheritParams radf_mc_cv
#'
#' @return A list with \code{adf_cv}, \code{sadf_cv}, \code{gsadf_cv},
#' \code{badf_cv}, \code{bsadf_cv} -- the same shape as \code{\link{radf_mc_cv}}'s
#' return value, so it can be used as a drop-in \code{cv} argument for
#' \code{\link{datestamp}}/\code{tidy}/\code{autoplot} on a \code{\link{radf_common}}
#' result.
#'
#' @references Chen, Y., Phillips, P. C. B., & Shi, S. (2023). Common
#' Bubble Detection in Large Dimensional Financial Systems. Journal of
#' Financial Econometrics, 21(4), 989-1063.
#'
#' @seealso \code{\link{radf_common}}, \code{\link{radf_mc_cv}}
#'
#' @importFrom foreach foreach
#' @importFrom doFuture `%dofuture%`
#' @importFrom progressr progressor
#' @export
radf_common_cv <- function(n, N, minw = NULL, nrep = 1000L, seed = NULL) {
  assert_n(n)
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(N, greater_than = 1)
  assert_positive_int(nrep)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  pcnt <- c(0.9, 0.95, 0.99)
  do_par <- getOption("exuber.parallel")

  set_rng(seed)
  results <- with_backend({
    p <- progressor(steps = nrep)
    foreach(
      i = 1:nrep,
      .combine = "cbind",
      .options.future = list(seed = TRUE, globals = structure(TRUE, add = c("radf_common", "radf", "rls_gsadf", "unroot", "parse_data"))),
      .inorder = FALSE
    ) %dofuture% {
      p()
      panel <- matrix(cumsum(rnorm(n * N)), nrow = n, ncol = N)
      res <- radf_common(panel, minw = minw)
      c(res$adf, res$sadf, res$gsadf, res$badf, res$bsadf)
    }
  })

  n_minw <- n - minw
  adf_crit <- quantile(results[1, ], probs = pcnt, drop = FALSE)
  sadf_crit <- quantile(results[2, ], probs = pcnt, drop = FALSE)
  gsadf_crit <- quantile(results[3, ], probs = pcnt, drop = FALSE)

  badf_mat <- results[3 + seq_len(n_minw), , drop = FALSE]
  bsadf_mat <- results[3 + n_minw + seq_len(n_minw), , drop = FALSE]

  bsadf_crit <- apply(bsadf_mat, 2, cummax) %>%
    apply(1, quantile, probs = pcnt) %>% t()
  asy_adf_crit <- rep(c(-0.44, -0.08, 0.6), each = nrow(bsadf_crit))
  badf_crit <- matrix(
    asy_adf_crit, ncol = 3,
    dimnames = list(NULL, paste0(pcnt * 100, "%"))
  )

  list(
    adf_cv = adf_crit,
    sadf_cv = sadf_crit,
    gsadf_cv = gsadf_crit,
    badf_cv = badf_crit,
    bsadf_cv = bsadf_crit
  ) %>%
    add_attr(
      index = 1:n,
      method = "Monte Carlo",
      n = n,
      N = N,
      minw = minw,
      iter = nrep,
      lag = 0,
      seed = get_rng_state(seed),
      parallel = do_par
    ) %>%
    add_class("radf_cv", "mc_cv")
}
