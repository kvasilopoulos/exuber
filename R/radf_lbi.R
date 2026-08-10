# Breitung & Diegel (2025, JTSA, "A Locally Best Invariant Sequential
# Test for Explosive Behavior in the Presence of Nonstationary
# Volatility"; "BD"). See docs/enhancements/monitoring.md for the full
# evaluation this implements -- their static (Section 3, known bubble
# start date at the beginning of the sample) locally best invariant
# (LBI) test only. Their actual headline contribution is a sequential/
# exponentially-weighted extension (Section 4) whose exact weighting
# scheme and boundary constant are not implemented here -- the static
# form (this file) needed no such detail: it is fully pinned down by
# their eq. 4-5 (confirmed via rendered PDF page, not the raw text
# extraction, which badly scrambles the sigma/summation notation).
#
# The whole point statistic reduces to eq. 4's telescoping identity:
# 2*sum(Delta y_t * y_{t-1}) = y_T^2 - T*sigma_tilde^2, sigma_tilde^2 :=
# T^{-1}*sum(Delta y_t^2) the sample variance of first differences under
# H0 (a unit root). Substituting gives eq. 5, LBI_T^2 = y_T^2 /
# (sigma_tilde^2 * T) -- i.e. the (squared, standardized) sample
# endpoint itself, no regression, no recursion, no bootstrap. Their
# Assumption 1 explicitly allows heteroskedastic innovations (the
# statistic is invariant to sigma^2's exact form, hence "locally best
# invariant"), and the paper states directly that under H0 the (signed,
# one-sided -- BD target positive bubbles only) statistic has a
# STANDARD NORMAL limiting null distribution -- no simulated or
# published table needed at all, genuinely the cheapest statistic
# validated in this whole project.

#' Locally Best Invariant Test for a Bubble (Breitung & Diegel 2025)
#'
#' \code{radf_lbi} implements the static locally best invariant (LBI)
#' test of Breitung & Diegel (2025) for a bubble known (or assumed) to
#' span the entire sample: \code{LBI = (y_T - y_1) / (sigma_tilde *
#' sqrt(T - 1))}, with \code{sigma_tilde^2} the sample variance of first
#' differences. Heteroskedasticity-robust by construction (the
#' statistic's invariance property does not depend on the exact form of
#' the innovation variance), with a standard normal null distribution --
#' no bootstrap, no simulation, no published table.
#'
#' Only the static (single, full-sample window) test is implemented.
#' Breitung & Diegel's own headline contribution is a sequential/
#' exponentially-weighted extension for monitoring an unknown start
#' date, whose exact weighting scheme and boundary constant are not
#' pinned down here and are not implemented.
#'
#' @inheritParams radf
#' @param level Nominal confidence level for the (one-sided, right-tailed
#' -- positive bubbles only) test (default \code{0.95}).
#'
#' @return An object of class \code{radf_lbi_obj}: a list with the test
#' statistic \code{stat}, the standard-normal critical value \code{crit},
#' and \code{detected} (logical, \code{stat > crit}).
#'
#' @references Breitung, J., & Diegel, M. (2025). A locally best
#' invariant sequential test for explosive behavior in the presence of
#' nonstationary volatility. Journal of Time Series Analysis.
#'
#' @seealso \code{\link{radf}} for the recursive ADF-family alternative
#' this complements.
#'
#' @importFrom stats qnorm
#' @export
radf_lbi <- function(data, level = 0.95) {
  stopifnot(level > 0 && level < 1)
  x <- parse_data(data)
  n <- nrow(x)
  snames <- colnames(x)
  nc <- ncol(x)

  stat <- setNames(rep(NA_real_, nc), snames)
  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    dy <- diff(y)
    sigma2_tilde <- mean(dy^2)
    stat[j] <- (y[n] - y[1]) / sqrt(sigma2_tilde * (n - 1))
  }
  crit <- unname(stats::qnorm(level))
  detected <- setNames(stat > crit, snames)

  list(stat = stat, crit = crit, detected = detected) %>%
    add_attr(series_names = snames, n = n, level = level) %>%
    add_class("radf_lbi_obj")
}

#' @export
print.radf_lbi_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_lbi (n = {attr(x, 'n')}, level = {attr(x, 'level') * 100}%)"))
  cat_line()
  print(
    data.frame(
      series = names(x$stat), stat = x$stat, crit = x$crit, detected = x$detected,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
