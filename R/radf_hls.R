# Harvey, Leybourne & Sollis (2017, Journal of Empirical Finance,
# "Improving the accuracy of asset price bubble start and end date
# estimators"; "HLS"). See docs/enhancements/dating-and-root-inference.md,
# "SSR/BIC dating vs. PSY recursive dating", section 1, for the full
# evaluation this implements -- all four model/BIC formulas below were
# re-verified there against rendered PDF pages, not just pdftotext.
#
# Replaces PSY's threshold-crossing dating rule with a model-based
# SSR-minimisation + BIC rule: four candidate regime-dummy regressions
# (Delta y_t on regime-indicator dummies and dummy-interacted y_{t-1}),
# each fit by residual-sum-of-squares minimisation over candidate break
# fractions, with BIC selecting among the four.
#
# A structural simplification that makes this tractable in pure R with
# no repeated lm() calls: because the four models' dummy windows never
# overlap, each candidate partition's TOTAL SSR is exactly the SUM of
# independent per-segment OLS fits (a segment with no active dummy has
# zero fitted parameters, SSR = sum(Delta y_t^2); a segment with an
# active dummy is a plain intercept+slope OLS fit of Delta y_t on
# y_{t-1}). Every segment's SSR is then a closed-form ratio of
# cumulative sums (Sx, Sxx, Sz, Szz, Sxz), the same style of O(1)-per-
# candidate lookup radf_pdc.R's pdc_find_break() already uses for its
# own (differently-specified, no-intercept) breakpoint search -- so the
# joint grid search over 1-3 breakpoints needs no new-per-candidate
# regression fit at all, only prefix-sum differences.

hls_prefix_sums <- function(y) {
  n1 <- length(y) - 1L
  x <- y[1:n1]
  z <- y[2:(n1 + 1L)] - y[1:n1]
  list(
    cx = c(0, cumsum(x)), cx2 = c(0, cumsum(x^2)),
    cz = c(0, cumsum(z)), cz2 = c(0, cumsum(z^2)),
    cxz = c(0, cumsum(x * z)), n1 = n1
  )
}

# Vectorized SSR of the segment(s) with i-index in (lo, hi], where lo/hi
# may be vectors of equal length (or one a scalar recycled against the
# other). `fit = FALSE`: no active dummy, zero fitted parameters,
# SSR = sum(z^2). `fit = TRUE`: intercept + slope OLS of z on x.
hls_segment_ssr <- function(ps, lo, hi, fit) {
  Sx <- ps$cx[hi + 1L] - ps$cx[lo + 1L]
  Sxx <- ps$cx2[hi + 1L] - ps$cx2[lo + 1L]
  Sz <- ps$cz[hi + 1L] - ps$cz[lo + 1L]
  Szz <- ps$cz2[hi + 1L] - ps$cz2[lo + 1L]
  if (!fit) {
    return(Szz)
  }
  Sxz <- ps$cxz[hi + 1L] - ps$cxz[lo + 1L]
  n_seg <- hi - lo
  b <- (n_seg * Sxz - Sx * Sz) / (n_seg * Sxx - Sx^2)
  a <- (Sz - b * Sx) / n_seg
  Szz - a * Sz - b * Sxz
}

hls_model1 <- function(y, ps, trim) {
  n1 <- ps$n1
  k_min <- max(2L, ceiling(trim * n1))
  taus <- k_min:(n1 - k_min)
  # HLS's sign constraint: the series must end above where the bubble
  # started (y_T > y_{tau1}) -- an upward-explosive-to-sample-end reading.
  taus <- taus[y[n1 + 1L] > y[taus + 1L]]
  if (length(taus) == 0L) {
    return(list(tau1 = NA_integer_, ssr = Inf))
  }
  ssr <- hls_segment_ssr(ps, 0L, taus, FALSE) + hls_segment_ssr(ps, taus, n1, TRUE)
  best <- which.min(ssr)
  list(tau1 = taus[best], ssr = ssr[best])
}

# Shared by Model 2 (post-bubble tail unfitted) and Model 3 (post-bubble
# tail fitted as its own collapse regime) -- `right_fit` toggles which.
hls_model23 <- function(y, ps, trim, right_fit) {
  n1 <- ps$n1
  k_min <- max(2L, ceiling(trim * n1))
  best <- list(ssr = Inf, tau1 = NA_integer_, tau2 = NA_integer_)
  tau1_max <- n1 - 2L * k_min
  if (tau1_max < k_min) {
    return(best)
  }
  for (tau1 in k_min:tau1_max) {
    tau2 <- (tau1 + k_min):(n1 - k_min)
    # HLS's own sign constraint (confirmed via HLW's restatement, their
    # eq. for Model 3): the "peak" y_{tau2} must exceed both the bubble's
    # own starting level AND wherever the series ends up after the fitted
    # collapse regime -- Model 2 has no such terminal point to compare
    # against (its post-tau2 segment is unfitted drift, not a collapse
    # regime with its own endpoint), so only right_fit=TRUE (Model 3) gets
    # the second constraint.
    valid <- y[tau2 + 1L] > y[tau1 + 1L]
    if (right_fit) valid <- valid & (y[tau2 + 1L] > y[n1 + 1L])
    tau2 <- tau2[valid]
    if (length(tau2) == 0L) next
    ssr <- hls_segment_ssr(ps, 0L, tau1, FALSE) +
      hls_segment_ssr(ps, tau1, tau2, TRUE) +
      hls_segment_ssr(ps, tau2, n1, right_fit)
    j <- which.min(ssr)
    if (ssr[j] < best$ssr) best <- list(tau1 = tau1, tau2 = tau2[j], ssr = ssr[j])
  }
  best
}

hls_model4 <- function(y, ps, trim) {
  n1 <- ps$n1
  k_min <- max(2L, ceiling(trim * n1))
  best <- list(ssr = Inf, tau1 = NA_integer_, tau2 = NA_integer_, tau3 = NA_integer_)
  tau1_max <- n1 - 3L * k_min
  if (tau1_max < k_min) {
    return(best)
  }
  for (tau1 in k_min:tau1_max) {
    tau2_max <- n1 - 2L * k_min
    tau2_seq <- (tau1 + k_min):tau2_max
    for (tau2 in tau2_seq) {
      if (y[tau2 + 1L] <= y[tau1 + 1L]) next
      tau3 <- (tau2 + k_min):(n1 - k_min)
      # As in Model 3, the "peak" y_{tau2} must also exceed the endpoint
      # of the fitted collapse regime, y_{tau3} -- otherwise it is not a
      # genuine peak (see hls_model23()'s comment on the same constraint).
      tau3 <- tau3[y[tau2 + 1L] > y[tau3 + 1L]]
      if (length(tau3) == 0L) next
      ssr <- hls_segment_ssr(ps, 0L, tau1, FALSE) +
        hls_segment_ssr(ps, tau1, tau2, TRUE) +
        hls_segment_ssr(ps, tau2, tau3, TRUE) +
        hls_segment_ssr(ps, tau3, n1, FALSE)
      j <- which.min(ssr)
      if (ssr[j] < best$ssr) {
        best <- list(tau1 = tau1, tau2 = tau2, tau3 = tau3[j], ssr = ssr[j])
      }
    }
  }
  best
}

hls_bic <- function(ssr, n, df) n * log(ssr / n) + df * log(n)

#' SSR/BIC Bubble Dating (Harvey, Leybourne & Sollis 2017)
#'
#' \code{radf_hls} dates a single bubble episode by fitting four
#' candidate regime-dummy regressions of \code{Delta y_t} on
#' \code{y_{t-1}} (unit-root-to-end, unit-root-bubble-unit-root,
#' unit-root-bubble-collapse, and unit-root-bubble-collapse-unit-root),
#' each by residual-sum-of-squares minimisation over candidate break
#' fractions, and selects among them by BIC.
#'
#' Unlike \code{\link{datestamp}} (threshold-crossing on the recursive
#' BSADF statistic) or \code{\link{radf_pdc}} (a fixed 3/4-regime
#' structure with sequentially, not jointly, estimated breaks), this
#' jointly searches breakpoints within each of four candidate regime
#' structures and lets BIC pick the structure itself -- so it can
#' distinguish "bubble that collapses to a new stationary regime"
#' (Model 3) from "bubble that fully reverts to a unit root" (Model 4)
#' from "bubble ongoing at the sample end" (Model 1), which
#' \code{radf_pdc}'s fixed regime count cannot. The cost is a genuine
#' joint grid search rather than \code{radf_pdc}'s sequential one-break-
#' at-a-time scan.
#'
#' @inheritParams radf
#' @param trim Minimum fraction of the (differenced) sample required in
#' every regime (default 0.05, following Harvey, Leybourne & Sollis's own
#' empirical-application choice; their simulations use 0.1).
#'
#' @return An object of class \code{radf_hls_obj}: a list with the
#' selected model (\code{model}, one of \code{1:4}), its breakpoint date(s)
#' (\code{origination}, \code{collapse}, \code{recovery} -- \code{NA} for
#' breakpoints the selected model doesn't have), and the BIC value of
#' every candidate model (\code{bic}, for inspecting how close the
#' selection was).
#'
#' @references Harvey, D. I., Leybourne, S. J., & Sollis, R. (2017).
#' Improving the accuracy of asset price bubble start and end date
#' estimators. Journal of Empirical Finance, 40, 121-138.
#'
#' @seealso \code{\link{radf_pdc}} for the cheaper sequential-splitting
#' alternative this complements, and \code{\link{datestamp}} for PSY's
#' original threshold-crossing rule.
#'
#' @export
radf_hls <- function(data, trim = 0.05) {
  x <- parse_data(data)
  n <- nrow(x)
  snames <- colnames(x)
  idx <- attr(x, "index")
  nc <- ncol(x)

  model <- setNames(rep(NA_integer_, nc), snames)
  origination <- collapse <- recovery <- setNames(rep(NA_character_, nc), snames)
  bic_mat <- matrix(NA_real_, nrow = nc, ncol = 4, dimnames = list(snames, paste0("model", 1:4)))

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    ps <- hls_prefix_sums(y)

    m1 <- hls_model1(y, ps, trim)
    m2 <- hls_model23(y, ps, trim, right_fit = FALSE)
    m3 <- hls_model23(y, ps, trim, right_fit = TRUE)
    m4 <- hls_model4(y, ps, trim)

    bics <- c(
      hls_bic(m1$ssr, n, 3), hls_bic(m2$ssr, n, 4),
      hls_bic(m3$ssr, n, 6), hls_bic(m4$ssr, n, 7)
    )
    bic_mat[j, ] <- bics
    jopt <- which.min(bics)
    model[j] <- jopt

    breaks <- switch(jopt,
      `1` = c(tau1 = m1$tau1),
      `2` = c(tau1 = m2$tau1, tau2 = m2$tau2),
      `3` = c(tau1 = m3$tau1, tau2 = m3$tau2),
      `4` = c(tau1 = m4$tau1, tau2 = m4$tau2, tau3 = m4$tau3)
    )
    dates <- vapply(breaks, function(b) as.character(idx[b + 1L]), character(1))
    origination[j] <- unname(dates["tau1"])
    if ("tau2" %in% names(dates)) collapse[j] <- unname(dates["tau2"])
    if ("tau3" %in% names(dates)) recovery[j] <- unname(dates["tau3"])
  }

  list(
    model = model, origination = origination, collapse = collapse,
    recovery = recovery, bic = bic_mat
  ) %>%
    add_attr(index = idx, series_names = snames, n = n, trim = trim) %>%
    add_class("radf_hls_obj")
}

#' @export
print.radf_hls_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_hls (n = {attr(x, 'n')}, trim = {attr(x, 'trim')})"))
  cat_line()
  print(
    data.frame(
      series = names(x$model), model = x$model, origination = x$origination,
      collapse = x$collapse, recovery = x$recovery, row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
