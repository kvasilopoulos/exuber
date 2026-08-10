# Sarkar, A. & Wells, M.T. (2026, arXiv:2604.12062, "Is There an AI
# Bubble? Robust Date-Stamping for Periods of Exuberance"; "SW"). See
# docs/enhancements/volatility-robustness.md, "SV-ADF", for the full
# evaluation this implements.
#
# A non-peer-reviewed preprint (flagged explicitly, a different bar than
# every other source implemented in this project). Re-triaged 2026-08-10
# after the earlier pass's own conditional cost note ("if the practical
# procedure is just radf()'s existing statistic compared to a specific
# boundary, this could be close to free") -- re-reading rendered pages
# 20-22 directly confirmed the favorable branch: the point statistic
# genuinely is radf()'s own `badf` sequence (already established in the
# earlier pass, from the proof appendix's eq. A.13-A.14: the "feasible"
# variance estimator is literally the standard within-window OLS
# residual variance radf()'s own recursive ADF t-statistic already
# uses), and the "moderate-deviation-calibrated boundary function" the
# abstract advertises turns out, in the paper's own actual applied
# methodology (their Section 5.1), to be two simple, closed-form,
# sample-size-only formulas -- no persistence parameter, no nuisance
# quantity from the companion "Double Local-to-Unity" paper needed at
# all:
#
#   origination threshold: log(t)/10  (their own 1,000-rep-per-n
#     calibration exercise under H0, approximated by this formula and
#     adopted directly -- "which is adopted as the origination
#     threshold for speculative bubbles")
#   collapse threshold:    log(t)/2   (same exercise under H1 averaged
#     over randomly drawn nuisance parameters -- "most closely
#     approximated by log(n)/2, which we therefore use as the collapse
#     threshold")
#
# where `t` is the current recursive window's own sample size (their
# Figure 12's x-axis, "proportion of samples for recursive SV-ADF test",
# times n) -- i.e. exactly badf's own natural index, no adaptation
# needed. The genuinely new feature relative to PWY/PSY's own dating
# (`datestamp()`) is that origination and collapse compare against
# DIFFERENT thresholds (SW's own Remark 1: a unit-root-based threshold
# suits collapse, not origination, "our SV-ADF procedure therefore
# calibrates the two thresholds separately") -- datestamp()'s own S3
# dispatch assumes one shared critical value throughout, so this ships
# as a small, self-contained dating routine reusing `stamp()`'s existing
# contiguous-run detection rather than extending `datestamp()` itself.

svadf_threshold <- function(t, type = c("origination", "collapse")) {
  type <- match.arg(type)
  if (type == "origination") log(t) / 10 else log(t) / 2
}

#' SV-ADF Asymmetric-Threshold Bubble Dating (Sarkar & Wells 2026)
#'
#' \code{radf_svadf} implements Sarkar & Wells (2026)'s SV-ADF
#' date-stamping procedure: \code{radf()}'s own recursive (backward) ADF
#' t-statistic (\code{badf}), which the paper's own asymptotic theory
#' (their Theorem 3.1) justifies under substantially weaker volatility
#' conditions than PWY/PSY's original derivation (nearly-nonstationary
#' \emph{stochastic} volatility, not just deterministic time-varying
#' volatility), compared against two different closed-form,
#' sample-size-only thresholds: \code{log(t)/10} for origination and
#' \code{log(t)/2} for collapse (\code{t} the current recursive window's
#' own sample size) -- both from the paper's own calibration exercise
#' (their Section 5.1), not new simulation.
#'
#' Origination is dated at the first run of at least \code{min_duration}
#' consecutive points with \code{badf} above the origination threshold;
#' collapse is dated (searching only after the origination date) at the
#' first run of at least \code{min_duration} consecutive points with
#' \code{badf} below the (lower) collapse threshold.
#'
#' @section Caveats:
#' \code{Sarkar & Wells (2026)} is a non-peer-reviewed preprint, a
#' different bar than every other source implemented in this package. The
#' same note is emitted as a message when this function is called (see
#' \code{\link{message}}/\code{\link{suppressMessages}} to silence it) and
#' stored as \code{attr(x, "caveat")} on the returned object.
#'
#' @inheritParams radf
#' @param min_duration Minimum number of consecutive periods a threshold
#' crossing must persist to be dated (default \code{\link{psy_ds}(n)}).
#'
#' @return An object of class \code{radf_svadf_obj}: a list with the
#' \code{badf} statistic path, the \code{origination}/\code{collapse}
#' threshold paths, and \code{origination}/\code{collapse} date indices
#' (\code{NA} if not detected).
#'
#' @references Sarkar, A., & Wells, M. T. (2026). Is there an AI bubble?
#' Robust date-stamping for periods of exuberance. arXiv:2604.12062.
#'
#' @seealso \code{\link{datestamp}} for the symmetric-threshold PWY/PSY
#' dating this complements.
#'
#' @export
radf_svadf <- function(data, minw = NULL, min_duration = NULL) {
  caveat <- "Sarkar & Wells (2026) is a non-peer-reviewed preprint; see ?radf_svadf, Caveats section."
  message_glue(caveat)

  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(n)
  min_duration <- min_duration %||% psy_ds(n)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(min_duration, strictly = FALSE)

  full <- radf(x, minw = minw, lag = 0L)
  badf <- full$badf
  pointer <- nrow(badf)
  t_idx <- minw + seq_len(pointer)

  orig_thresh <- svadf_threshold(t_idx, "origination")
  coll_thresh <- svadf_threshold(t_idx, "collapse")

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  origination <- collapse <- setNames(rep(NA_integer_, nc), snames)
  for (j in seq_len(nc)) {
    above_idx <- which(badf[, j] > orig_thresh)
    if (length(above_idx) == 0L) next
    runs <- stamp(above_idx)
    runs <- runs[runs$Duration >= min_duration, ]
    if (nrow(runs) == 0L) next
    r_start_row <- runs$Start[1]
    origination[j] <- t_idx[r_start_row]

    below_idx <- which(badf[, j] < coll_thresh)
    below_idx <- below_idx[below_idx > r_start_row]
    if (length(below_idx) == 0L) next
    runs2 <- stamp(below_idx)
    runs2 <- runs2[runs2$Duration >= min_duration, ]
    if (nrow(runs2) > 0L) {
      collapse[j] <- t_idx[runs2$Start[1]]
    }
  }

  origination_date <- vapply(origination, function(i) {
    if (is.na(i)) NA_character_ else as.character(idx[i])
  }, character(1))
  collapse_date <- vapply(collapse, function(i) {
    if (is.na(i)) NA_character_ else as.character(idx[i])
  }, character(1))

  list(
    badf = badf, t_idx = t_idx,
    origination_threshold = orig_thresh, collapse_threshold = coll_thresh,
    origination = origination, collapse = collapse,
    origination_date = origination_date, collapse_date = collapse_date
  ) %>%
    add_attr(index = idx, series_names = snames, n = n, minw = minw, min_duration = min_duration, caveat = caveat) %>%
    add_class("radf_svadf_obj")
}

#' @export
print.radf_svadf_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "radf_svadf (n = {attr(x, 'n')}, minw = {attr(x, 'minw')}, ",
    "min_duration = {attr(x, 'min_duration')})"
  ))
  cat_line()
  cat_caveat(x)
  print(
    data.frame(
      series = names(x$origination),
      origination = x$origination, origination_date = x$origination_date,
      collapse = x$collapse, collapse_date = x$collapse_date,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
