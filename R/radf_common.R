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
