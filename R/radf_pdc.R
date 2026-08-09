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
# of the (differenced) sample on either side of the break.
pdc_find_break <- function(y, trim = 0.05) {
  n1 <- length(y) - 1L
  ylag <- y[1:n1]
  ycur <- y[2:(n1 + 1)]

  csxx <- c(0, cumsum(ylag^2))
  csxy <- c(0, cumsum(ylag * ycur))
  csyy <- c(0, cumsum(ycur^2))

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
#' @inheritParams radf
#' @param regimes Either \code{3} (PDC: unit-root, explosive, stationary
#' collapse) or \code{4} (KS: adds a final unit-root recovery regime after
#' the collapse).
#' @param trim Minimum fraction of the (differenced) sample required on
#' either side of each breakpoint search (default 0.05, as in KS's
#' empirical application; PDC use 0.05-0.1 in their simulations).
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
#' @seealso \code{\link{datestamp}} for the PSY threshold-crossing
#' alternative.
#'
#' @export
radf_pdc <- function(data, regimes = 3L, trim = 0.05) {
  regimes <- as.integer(regimes)
  if (!regimes %in% c(3L, 4L)) {
    stop_glue("Argument 'regimes' should be 3 or 4.")
  }
  x <- parse_data(data)
  nc <- ncol(x)
  snames <- colnames(x)
  idx <- index(x)

  to_date <- function(i) if (is.null(i)) NA else idx[i]

  rows <- vector("list", nc)
  for (j in 1:nc) {
    y <- x[, j]
    b2 <- pdc_find_break(y, trim)$break_idx # collapse: split of the full sample
    b1 <- pdc_find_break(y[1:b2], trim)$break_idx # origination: split of the pre-collapse subsample

    row <- list(origination = to_date(b1), collapse = to_date(b2))
    if (regimes == 4L) {
      y_right <- y[(b2 + 1):length(y)]
      b3_rel <- pdc_find_break(y_right, trim)$break_idx # recovery: split of the post-collapse subsample
      row$recovery <- to_date(b2 + b3_rel)
    }
    rows[[j]] <- row
  }

  out <- do.call(rbind.data.frame, rows)
  rownames(out) <- snames
  out
}
