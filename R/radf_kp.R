# Kernel-purged heteroskedasticity-robust PSY test. Harvey, Leybourne,
# Taylor & Zu (2024, J. Time Series Analysis, DOI 10.1111/jtsa.12784, CC-BY
# open access).
#
# Rather than resampling (radf_wb_cv) or time-deforming (radf_tt), this
# purges unconditional heteroskedasticity by dividing each first difference
# by a kernel spot-volatility estimate and cumulating the result (eq. 4-5),
# then runs the *unmodified* PSY/GSADF test on the purged series. Theorem 1
# / Remark 3.2 shows the purged (with-intercept) statistic's null
# distribution is IDENTICAL to the standard homoskedastic GSADF null -- so,
# unlike SBZ, this needs no new critical-value machinery at all: exuber's
# existing radf_mc_cv() already applies.

# Kernel-purged transform (eq. 4-5): x_t = cumsum(Delta y_t / sigma_hat_t),
# sigma_hat_t from kernel_spot_vol() (already built for SBZ; same weighting
# convention: kernel argument is a normalized-time distance divided by h).
# Returns a levels-like series of length T - 1 (t = 2, ..., T), suitable to
# feed straight into radf().
kernel_purge <- function(y, kernel = c("gaussian", "uniform"), h = NULL) {
  kernel <- match.arg(kernel)
  Tn <- length(y) - 1L
  h <- h %||% 0.1 * Tn^(-0.25)
  vol <- kernel_spot_vol(y, kernel = kernel, h = h)
  cumsum(diff(y) / sqrt(vol$sigma2))
}

#' Kernel-Purged Heteroskedasticity-Robust PSY Test
#'
#' \code{radf_kp} implements the bootstrap-free heteroskedasticity-robust
#' PSY test of Harvey, Leybourne, Taylor & Zu (2024): it "purges" unconditional
#' heteroskedasticity by cumulating the series' first differences after
#' dividing each by a kernel spot-volatility estimate (eq. 4-5), then runs
#' the ordinary (with-intercept) \code{\link{radf}} on the purged series.
#'
#' Because the purged statistic's null limiting distribution is proven
#' (Theorem 1 / Remark 3.2) to be identical to the standard homoskedastic
#' GSADF null, \code{\link{radf_mc_cv}} -- exuber's existing, already-fast
#' Monte Carlo critical values -- applies directly to the result; no new
#' bootstrap or simulation machinery is needed, unlike \code{\link{radf_wb_cv}}
#' or \code{\link{radf_sbz_cv}}.
#'
#' Only the with-intercept variant (\eqn{PSY_\sigma} in the paper) is
#' implemented. The paper also proposes a without-intercept variant and a
#' union-of-rejections test combining both; these are not implemented here
#' (see the package's enhancement notes for the cost/benefit reasoning).
#'
#' @inheritParams radf
#' @param kernel Kernel for the spot-volatility estimator, \code{"gaussian"}
#' (default, as in the paper) or \code{"uniform"}.
#' @param h Bandwidth for the spot-volatility estimator. Default
#' \code{0.1 * T^(-0.25)}, the paper's own setting (Table I, Section 6).
#'
#' @return A \code{radf_obj}, identical in structure to \code{\link{radf}}'s
#' output (so \code{\link{radf_mc_cv}}, \code{tidy()} etc. all apply
#' directly), computed on the volatility-purged series.
#'
#' @references Harvey, D. I., Leybourne, S. J., Taylor, A. M. R., & Zu, Y.
#' (2024). A new heteroskedasticity-robust test for explosive bubbles.
#' Journal of Time Series Analysis. \doi{10.1111/jtsa.12784}
#'
#' @seealso \code{\link{radf_mc_cv}} for this test's (unmodified) critical
#' values, \code{\link{radf_wb_cv}} for a bootstrap-based alternative, and
#' \code{\link{radf_tt}} for another bootstrap-free alternative.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- radf_kp(sim_data, minw = 20)
#' print(res)
#'
#' # radf_mc_cv() applies unmodified -- see Details
#' cv <- radf_mc_cv(n = attr(res, "n"), minw = 20)
#' summary(res, cv = cv)
#' }
#'
#' @export
radf_kp <- function(data, minw = NULL, kernel = c("gaussian", "uniform"), h = NULL) {
  kernel <- match.arg(kernel)
  x <- parse_data(data)
  nc <- ncol(x)
  snames <- colnames(x)

  purged <- vector("list", nc)
  for (i in 1:nc) {
    purged[[i]] <- kernel_purge(x[, i], kernel = kernel, h = h)
  }
  purged <- do.call(cbind, purged)
  colnames(purged) <- snames

  radf(purged, minw = minw)
}
