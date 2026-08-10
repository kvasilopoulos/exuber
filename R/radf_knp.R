# Kejriwal, Nguyen & Perron (2025, JTSA 46(5), "An Improved Procedure
# for Retrospectively Dating the Emergence and Collapse of Bubbles";
# "KNP"). See docs/enhancements/dating-and-root-inference.md, "SSR/BIC
# dating vs. PSY recursive dating", for the full evaluation this
# implements.
#
# KNP's own DGP/estimating model (their eq. 1-3, confirmed via rendered
# PDF pages) is structurally identical to HLS (2017)'s Model 2: an
# unfitted unit-root regime, then an intercept+slope-fitted explosive
# regime, then an unfitted unit-root regime resuming from a shifted
# level (an instantaneous, not gradual, collapse). Their Theorem 1
# proves the plain-OLS joint SSR minimiser over this model is
# INCONSISTENT: the origination-date estimate T_hat_1 converges to the
# true COLLAPSE date T2^0, not the true origination date T1^0 -- a
# strictly worse failure than PSY's own well-known "late" bias. Their
# fix (Theorem 2) is small: omit the single squared residual at the
# collapse-date observation (Delta y_{T2+1})^2 from the objective before
# minimising, which restores consistency for both dates (and for the
# explosive-coefficient estimate). A footnote proves this is numerically
# equivalent to a one-time-dummy modification of HLS's own Model 4
# regression, but the omission formulation is the cheap one to compute:
# it needs no new regression at all, only subtracting a single already-
# available squared term from radf_hls.R's existing Model-2-style
# closed-form SSR (hls_segment_ssr()), reused directly here.
#
# KNP's Section 3 also develops a Bai-Perron/Perron-Qu-style dynamic
# programming algorithm for the multi-bubble case, avoiding HLS's own
# brute-force combinatorial grid search. Not implemented here -- KNP's
# single-bubble omission fix is a small, self-contained addition to
# already-shipped machinery; the multi-bubble DP algorithm is a
# genuinely new piece of algorithmic infrastructure exuber has no
# analogue of, left as a documented follow-on (the same scoping decision
# already made for HLW's own multi-bubble extension elsewhere in this
# file).

# Jointly searches (tau1, tau2) minimising KNP's SSR (eq. 3, `omit =
# FALSE`) or their omission-corrected SSR (eq. 4, `omit = TRUE`,
# default) -- both reuse radf_hls.R's hls_prefix_sums()/hls_segment_ssr()
# directly, since KNP's model has the same three-segment (unfitted,
# intercept+slope, unfitted) shape as HLS's Model 2. Unlike
# hls_model23(), KNP's own candidate set T_epsilon(2) imposes no
# directional sign constraint on the fitted "peak".
knp_find_break <- function(y, trim = 0.05, omit = TRUE) {
  n1 <- length(y) - 1L
  ps <- hls_prefix_sums(y)
  k_min <- max(2L, ceiling(trim * n1))
  tau1_max <- n1 - 2L * k_min
  best <- list(ssr = Inf, tau1 = NA_integer_, tau2 = NA_integer_)
  if (tau1_max < k_min) {
    return(best)
  }
  for (tau1 in k_min:tau1_max) {
    tau2 <- (tau1 + k_min):(n1 - k_min)
    ssr <- hls_segment_ssr(ps, 0L, tau1, FALSE) +
      hls_segment_ssr(ps, tau1, tau2, TRUE) +
      hls_segment_ssr(ps, tau2, n1, FALSE)
    if (omit) {
      z2_single <- ps$cz2[tau2 + 2L] - ps$cz2[tau2 + 1L]
      ssr <- ssr - z2_single
    }
    j <- which.min(ssr)
    if (ssr[j] < best$ssr) best <- list(tau1 = tau1, tau2 = tau2[j], ssr = ssr[j])
  }
  best
}

#' Bias-Corrected Single-Bubble Dating (Kejriwal, Nguyen & Perron 2025)
#'
#' \code{radf_knp} dates a single bubble episode (origination, collapse)
#' by minimising a residual-omission-corrected sum of squared residuals
#' over a three-regime model (unit root, explosive, unit root resuming
#' from a shifted level after an instantaneous collapse). Plain OLS over
#' this model is provably inconsistent -- the origination-date estimate
#' converges to the true \emph{collapse} date, not the origination date
#' -- which \code{omit = TRUE} (the default) fixes by dropping the
#' single squared residual at the candidate collapse date from the
#' objective before minimising.
#'
#' @inheritParams radf
#' @param trim Minimum fraction of the (differenced) sample required in
#' each regime (default 0.05).
#' @param omit Use Kejriwal, Nguyen & Perron's consistency-restoring
#' correction (default \code{TRUE}). \code{FALSE} gives the plain,
#' provably inconsistent OLS estimator (their Theorem 1) -- kept mainly
#' to demonstrate the correction's effect, not for practical dating.
#'
#' @return An object of class \code{radf_knp_obj}: a list with
#' \code{origination}, \code{collapse} (dates) and \code{delta} (the
#' fitted explosive AR coefficient).
#'
#' @references Kejriwal, M., Nguyen, L., & Perron, P. (2025). An
#' improved procedure for retrospectively dating the emergence and
#' collapse of bubbles. Journal of Time Series Analysis, 46(5), 867-883.
#'
#' @seealso \code{\link{radf_hls}}, \code{\link{radf_pdc}} for related
#' SSR-based dating approaches.
#'
#' @export
radf_knp <- function(data, trim = 0.05, omit = TRUE) {
  x <- parse_data(data)
  n <- nrow(x)
  snames <- colnames(x)
  idx <- attr(x, "index")
  nc <- ncol(x)

  origination <- collapse <- setNames(rep(NA_character_, nc), snames)
  delta <- setNames(rep(NA_real_, nc), snames)

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    ps <- hls_prefix_sums(y)
    fit <- knp_find_break(y, trim, omit)

    origination[j] <- as.character(idx[fit$tau1 + 1L])
    collapse[j] <- as.character(idx[fit$tau2 + 1L])
    coef <- hls_segment_coef(ps, fit$tau1, fit$tau2)
    delta[j] <- unname(coef["slope"]) + 1
  }

  list(origination = origination, collapse = collapse, delta = delta) %>%
    add_attr(index = idx, series_names = snames, n = n, trim = trim, omit = omit) %>%
    add_class("radf_knp_obj")
}

#' @export
print.radf_knp_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_knp (n = {attr(x, 'n')}, trim = {attr(x, 'trim')}, omit = {attr(x, 'omit')})"))
  cat_line()
  print(
    data.frame(
      series = names(x$origination), origination = x$origination,
      collapse = x$collapse, delta = x$delta, row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
