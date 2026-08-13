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
#' \code{lbi_test} implements the static locally best invariant (LBI)
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
#' @note The critical value is closed-form: the standard normal
#' (\code{qnorm}) quantile at \code{level} -- no bootstrap, no
#' simulation, no table needed.
#'
#' @inheritParams radf
#' @param level Nominal confidence level for the (one-sided, right-tailed
#' -- positive bubbles only) test (default \code{0.95}).
#'
#' @return An object of class \code{lbi_test_obj}: a list with the test
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
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- lbi_test(sim_data$sim_psy1)
#' print(res)
#' }
#'
#' @importFrom stats qnorm
#' @export
lbi_test <- function(data, level = 0.95) {
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
    add_class("lbi_test_obj")
}

#' @export
print.lbi_test_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("lbi_test (n = {attr(x, 'n')}, level = {attr(x, 'level') * 100}%)"))
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

# Breitung & Diegel's own sequential extension (their Section 4.1),
# resolved by re-reading rendered PDF pages 4-7: the "unknown break date"
# problem is solved by adopting the classical CUSUM approach directly on
# the LBI statistic -- a running partial sum of first differences over the
# monitoring period, normalized by the FIXED monitoring-horizon length
# T_m (not sqrt(t), unlike monitor_cusum()'s Chu-Stinchcombe-White boundary),
# tested against a CONSTANT boundary. Their eq. 15:
# LBI_[rT] = (1/(sigma*sqrt(T))) * sum_{t=1}^{[rT]} Delta y_t => W(r), a
# standard Brownian motion on [0,1] under H0 -- so a constant boundary
# b_alpha (their Table 1) controls size uniformly over the whole
# monitoring window, structurally the "mCUSUM" (constant-boundary) variant
# they show is more powerful than both the classical time-varying-boundary
# CUSUM (Brown et al. 1975, also tabulated in Table 1 but not implemented
# here -- superseded by mCUSUM/wCUSUM, the paper's own recommendation) and
# monitor_cusum()'s existing CSW-style growing boundary.
#
# Their eq. 12 additionally allows exponentially up-weighting later
# (more bubble-like) observations via a single parameter c_bar >= 0:
# w_r^{c_bar} = sqrt(2*c_bar/T_m) / sqrt(exp(2*c_bar)-1) * exp(c_bar*r).
# c_bar = 0 recovers flat weights (mCUSUM); c_bar > 0 is "wCUSUM". Table 1
# gives ONE set of critical values covering both, because (their own
# argument, eq. above Table 1) sup_r{W*(eta(r))} =_d sup_r{W(r)} -- the
# running max of the time-changed process has the same distribution
# regardless of c_bar. Table 1's own suggestion is c_bar ~ 2 for a
# moderate power boost when a bubble partway through the monitoring
# window is plausible; c_bar = 0 (flat/mCUSUM) is the conservative
# default matching a bubble equally likely at any point.
#
# sigma_tilde^2 is estimated from the TRAINING window only (their Section
# 4.2's own text: "training set ... is used for estimating nuisance
# parameters such as sigma^2"), consistently with lbi_test()'s own
# training-free full-sample sigma_tilde^2 for the static test.

# Breitung & Diegel (2025) Table 1 (page 7 in the rendered PDF): one-sided
# asymptotic critical values for sequential bubble detectors, 1,000,000
# Monte Carlo reps at T=10,000. Only the constant-boundary row (used by
# both mCUSUM and wCUSUM, per the invariance argument above) is
# transcribed -- the classical time-varying-boundary CUSUM row is the
# paper's own point of comparison, not its contribution, and is not
# implemented.
bd_cusum_table <- data.frame(
  level = c(0.90, 0.95, 0.975, 0.99, 0.995),
  b_alpha = c(1.64, 1.95, 2.24, 2.57, 2.80)
)

bd_cusum_q <- function(level) {
  match_idx <- which(abs(level - bd_cusum_table$level) < 1e-8)
  if (length(match_idx) == 0L) {
    stop_glue(
      "'level' must be one of {paste(bd_cusum_table$level, collapse = ', ')} ",
      "(Breitung & Diegel (2025)'s Table 1 only tabulates these ",
      "significance levels)."
    )
  }
  bd_cusum_table$b_alpha[match_idx]
}

# eq. 12's weight function, evaluated at the monitoring period's own
# discrete relative positions r_j = j / T_m, j = 1, ..., T_m. c_bar = 0
# (mCUSUM) short-circuits to the flat weight 1/sqrt(T_m) directly (eq.
# 12's own limit as c_bar -> 0 is 0/0; the flat weight is the paper's own
# stated closed-form for that case, not a numerical approximation of it).
bd_cusum_weights <- function(T_m, c_bar) {
  if (c_bar == 0) {
    return(rep(1 / sqrt(T_m), T_m))
  }
  r <- seq_len(T_m) / T_m
  sqrt(2 * c_bar / T_m) / sqrt(exp(2 * c_bar) - 1) * exp(c_bar * r)
}

#' Sequential LBI Monitoring for an Unknown Bubble Start Date (Breitung &
#' Diegel 2025)
#'
#' \code{monitor_lbi} implements the sequential (constant-boundary)
#' extension of \code{\link{lbi_test}}'s locally best invariant statistic,
#' for monitoring a series in real time when the bubble's start date is
#' unknown: after a training window \code{[1, T*]} assumed free of
#' exuberance, the (optionally exponentially weighted) partial sum of
#' post-training first differences is compared against a constant
#' boundary, flagging the first monitoring date it is breached.
#'
#' Their eq. 15 shows this partial sum, normalized by the fixed monitoring
#' horizon length (not \code{sqrt(t)}, unlike \code{\link{monitor_cusum}}'s
#' Chu-Stinchcombe-White-style boundary), converges to a standard Brownian
#' motion on \code{[0, 1]} under the null -- so a single constant boundary
#' controls size uniformly across the whole monitoring window. The paper
#' shows this constant-boundary detector ("mCUSUM" at \code{c_bar = 0},
#' "wCUSUM" at \code{c_bar > 0}) is more powerful than the classical
#' time-varying-boundary CUSUM test it is compared against.
#'
#' @note The critical value is a published constant boundary (Breitung &
#' Diegel (2025)'s Table 1) -- a table lookup, no simulation.
#'
#' @inheritParams monitor_cusum
#' @param c_bar Exponential up-weighting parameter for later (more
#' bubble-like) monitoring observations (their eq. 12), \code{>= 0}.
#' \code{0} (default) is the flat-weight "mCUSUM" variant, appropriate
#' when a bubble is equally likely to start at any point in the
#' monitoring window; the paper's own suggested value for a moderate power
#' boost when a bubble partway through is more plausible is \code{2}.
#' Critical values (\code{level}) are the same for every \code{c_bar}.
#' @param level Nominal confidence level, one of \code{0.90}, \code{0.95},
#' \code{0.975}, \code{0.99}, \code{0.995} (Breitung & Diegel's Table 1
#' only tabulates these).
#'
#' @return An object of class \code{monitor_lbi_obj}: a list with the
#' monitoring-region statistic path (\code{stat}), the constant
#' \code{boundary}, the training window length \code{T_star}, and
#' \code{alarm}/\code{alarm_date} (the first breach, \code{NA} if none).
#'
#' @references Breitung, J., & Diegel, M. (2025). A locally best invariant
#' sequential test for explosive behavior in the presence of nonstationary
#' volatility. Journal of Time Series Analysis.
#'
#' @seealso \code{\link{lbi_test}} for the static (known, full-sample
#' bubble window) version. \code{\link{monitor_cusum}} and
#' \code{\link{radf_monitor}} for structurally different monitoring
#' detectors.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- monitor_lbi(sim_data$sim_psy1, r_star = 0.5)
#' print(res)
#' }
#'
#' @export
monitor_lbi <- function(data, r_star = 0.5, c_bar = 0, level = 0.95) {
  stopifnot(c_bar >= 0)
  b_alpha <- bd_cusum_q(level)
  x <- parse_data(data)
  n <- nrow(x)

  T_star <- if (r_star < 1) round(r_star * n) else as.integer(r_star)
  if (T_star < 3L) {
    stop_glue("Training window ('r_star') is too short.")
  }
  if (T_star >= n) {
    stop_glue("Training window ('r_star') must leave at least one monitoring observation.")
  }
  T_m <- n - T_star
  w <- bd_cusum_weights(T_m, c_bar)

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  stat_path <- matrix(NA_real_, T_m, nc, dimnames = list(NULL, snames))
  alarm <- setNames(rep(NA_integer_, nc), snames)

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    dy <- diff(y)
    sigma2_tilde <- mean(dy[seq_len(T_star - 1L)]^2)
    dy_mon <- dy[T_star:(n - 1L)]
    phi <- cumsum(w * dy_mon) / sqrt(sigma2_tilde)
    stat_path[, j] <- phi
    breach <- which(phi > b_alpha)
    if (length(breach) > 0L) alarm[j] <- T_star + breach[1L]
  }

  alarm_date <- vapply(alarm, function(i) {
    if (is.na(i)) NA_character_ else as.character(idx[i])
  }, character(1))

  list(
    stat = stat_path, boundary = b_alpha, T_star = T_star,
    alarm = alarm, alarm_date = alarm_date
  ) %>%
    add_attr(
      index = idx, series_names = snames, n = n, c_bar = c_bar, level = level
    ) %>%
    add_class("monitor_lbi_obj")
}

#' @export
print.monitor_lbi_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "monitor_lbi (T* = {x$T_star} / {attr(x, 'n')}, c_bar = {attr(x, 'c_bar')}, b_alpha = {x$boundary})"
  ))
  cat_line()
  print(
    data.frame(
      series = names(x$alarm), alarm = x$alarm, alarm_date = x$alarm_date,
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
