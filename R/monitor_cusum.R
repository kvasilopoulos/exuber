# CUSUM real-time monitoring. Homm & Breitung (2012, Journal of Financial
# Econometrics, 10(1), 198-231; "HB"), Section 3, eq. 26-30. See
# docs/enhancements/monitoring.md, "CUSUM/Page-CUSUM detector family", for
# the full evaluation this implements -- Family B: a structurally
# different statistic from Family A's recursive-ADF training-max
# (radf_monitor()), a standardized running sum of first differences
# compared against a closed-form asymptotic boundary. No wild bootstrap,
# no simulation, no new dependency -- the whole detector is a cumsum() and
# a boundary formula, exactly matching this project's "why not exubercore"
# precedent for cheap closed-form statistics (STADF, sign-based).
#
# HB propose two monitoring statistics, CUSUM (here) and FLUC
# (radf_monitor(..., boundary = "fluc")) -- both now implemented.
#
# `boundary = "finite"` adds HB's own finite-sample CUSUM boundary
# constant (their Table 8, "without drift estimation" -- matching this
# file's own raw-first-difference construction, no mean removal), used
# in place of the fixed asymptotic `b_alpha = 4.6`. Same shape and same
# lookup-and-snap pattern as radf_monitor.R's hb_fluc_q()/hb_fluc_table
# for FLUC's own Table 7 -- a published constant, no new simulation.
# Applied to both `type = "standard"` and `type = "kernel"` (CUSUMV):
# Astill et al.'s own Corollary 1 (already implemented/validated above)
# establishes the *same* boundary function works unchanged for both
# statistics, so extending that same finite-sample table to CUSUMV is a
# direct, not a new, claim.

# Homm & Breitung (2012) Table 8(i): CUSUM boundary constant b_{k,alpha},
# without drift estimation, transcribed from a rendered PDF page. Same
# (n, alpha, k) grid as FLUC's Table 7.
hb_cusum_finite_table <- local({
  k_grid <- c(2, 3, 4, 5, 6, 8, 10)
  n_grid <- c(100, 50, 20)
  alpha_grid <- c(0.10, 0.05, 0.01)
  q <- rbind(
    c(0.92, 1.39, 1.62, 1.80, 1.92, 2.08, 2.19), # n=100, alpha=0.10
    c(1.51, 2.14, 2.47, 2.73, 2.88, 3.20, 3.36), # n=100, alpha=0.05
    c(2.86, 3.92, 4.57, 4.94, 5.30, 5.72, 6.02), # n=100, alpha=0.01
    c(0.87, 1.31, 1.55, 1.70, 1.82, 1.97, 2.06), # n=50,  alpha=0.10
    c(1.43, 2.03, 2.34, 2.62, 2.80, 3.03, 3.12), # n=50,  alpha=0.05
    c(2.85, 3.87, 4.25, 4.77, 5.03, 5.47, 5.94), # n=50,  alpha=0.01
    c(0.81, 1.18, 1.43, 1.61, 1.75, 1.91, 2.02), # n=20,  alpha=0.10
    c(1.27, 1.96, 2.35, 2.53, 2.72, 3.00, 3.13), # n=20,  alpha=0.05
    c(2.61, 3.91, 4.49, 4.97, 5.16, 5.52, 5.74)  # n=20,  alpha=0.01
  )
  tbl <- data.frame(n = rep(n_grid, each = 3), alpha = rep(alpha_grid, times = 3), q)
  colnames(tbl) <- c("n", "alpha", paste0("k", k_grid))
  tbl
})

# Same lookup-and-snap convention as radf_monitor.R's hb_fluc_q().
hb_cusum_finite_q <- function(level, n_train, k) {
  beta <- 1 - level
  beta_choices <- c(0.10, 0.05, 0.01)
  match_idx <- which(abs(beta - beta_choices) < 1e-8)
  if (length(match_idx) == 0L) {
    stop_glue(
      "'level' must be one of {paste(1 - beta_choices, collapse = ', ')} ",
      "for boundary = 'finite' (Homm & Breitung (2012)'s Table 8 only ",
      "tabulates these significance levels)."
    )
  }
  n_grid <- c(20, 50, 100)
  k_grid <- c(2, 3, 4, 5, 6, 8, 10)
  n_snap <- n_grid[which.min(abs(n_train - n_grid))]
  k_snap <- k_grid[which.min(abs(k - k_grid))]
  row <- hb_cusum_finite_table[hb_cusum_finite_table$n == n_snap & abs(hb_cusum_finite_table$alpha - beta) < 1e-8, ]
  row[[paste0("k", k_snap)]]
}

# HB's CUSUM statistic (eq. 26) and boundary (eq. 29) evaluated at every
# monitoring point t = T_star+1, ..., n for a single series y. sigma_hat_t^2
# is HB's "consistent estimator of the residual variance based on the
# sample {y_0, ..., y_t}" (eq. 26's own text): the recursive (growing)
# sample variance of first differences up to t, re-estimated as new data
# arrives -- legitimate in real-time monitoring since only past/current
# data is used at each t, unlike radf_monitor()'s training-only wild
# bootstrap which has to guard against a fixed critical value leaking
# future information.
cusum_stat_path <- function(y, T_star, b_alpha) {
  n <- length(y)
  dy <- diff(y)
  cs_dy2 <- cumsum(dy^2)

  t_idx <- (T_star + 1L):n
  sigma2_t <- cs_dy2[t_idx - 1L] / (t_idx - 1L)
  S_t <- (y[t_idx] - y[T_star]) / sqrt(sigma2_t)
  c_t <- sqrt(b_alpha + log(t_idx / T_star))
  boundary_t <- c_t * sqrt(t_idx)

  list(t = t_idx, S = S_t, boundary = boundary_t)
}

# One-sided (backward-looking) Nadaraya-Watson kernel spot-variance
# estimator, Astill, Harvey, Leybourne, Taylor & Zu (2023, JFEC 21(1),
# 187-227; "AHLTZ")'s eq. 6-7: sigma2_j = sum_{s=0}^{N} w_s * dy_{j-s}^2,
# w_s = K(s/N) / sum_s K(s/N) -- a FIXED weight vector (depends only on
# s, not j), applied as a one-sided moving weighted average of squared
# first differences. Deliberately one-sided/causal (only current and past
# lags), unlike the two-sided smoother already used for SBZ/kernel-purge
# (radf_sbz.R's nw_spot_vol()): a real-time monitoring statistic at time j
# can only use data up to j. `stats::filter(..., sides = 1)` computes
# exactly this weighted sum directly. AHLTZ's own convention (eq. 7's own
# text) is sigma2_j := 1 for j <= N (monitoring only meaningfully starts
# once T* > N in practice, so this affects at most the first N
# observations of a much longer series).
one_sided_kernel_spot_vol <- function(dy, N = 20, kernel = c("gaussian", "uniform")) {
  kernel <- match.arg(kernel)
  kern <- switch(kernel,
    gaussian = function(u) dnorm(u),
    uniform = function(u) as.numeric(abs(u) <= 1) / 2
  )
  w <- kern((0:N) / N)
  w <- w / sum(w)
  sigma2 <- as.numeric(stats::filter(dy^2, filter = w, sides = 1))
  sigma2[is.na(sigma2)] <- 1
  sigma2
}

# AHLTZ's modified (volatility-robust) CUSUM statistic, eq. 6: SV_t :=
# sum_{j=T*+1}^{t} Delta y_j / sigma_hat_{j,N}, using the one-sided
# kernel spot-variance estimator above in place of HB's single running
# variance. Their Corollary 1 establishes the SAME boundary function
# c_t*sqrt(t) (same b_alpha) delivers a controlled asymptotic FPR for
# this modified statistic even under time-varying volatility -- so this
# reuses cusum_stat_path()'s boundary formula unchanged, only the
# numerator's construction differs.
cusum_stat_path_kernel <- function(y, T_star, b_alpha, N, kernel) {
  n <- length(y)
  dy <- diff(y)
  sigma2_dy <- one_sided_kernel_spot_vol(dy, N = N, kernel = kernel)
  weighted <- dy / sqrt(sigma2_dy)
  cs <- c(0, cumsum(weighted))

  t_idx <- (T_star + 1L):n
  SV_t <- cs[t_idx] - cs[T_star]
  c_t <- sqrt(b_alpha + log(t_idx / T_star))
  boundary_t <- c_t * sqrt(t_idx)

  list(t = t_idx, S = SV_t, boundary = boundary_t)
}

#' CUSUM Real-Time Monitoring for Explosive Bubbles
#'
#' \code{monitor_cusum} implements Homm & Breitung (2012)'s CUSUM real-time
#' monitoring procedure: fix a training window \code{[1, T*]} assumed free
#' of exuberance, then compare the standardized cumulative sum of
#' post-training first differences, \code{S_t = (y_t - y_{T*}) /
#' sigma_hat_t}, against a closed-form boundary
#' \code{c_t * sqrt(t)}, \code{c_t = sqrt(b_alpha + log(t / T*))}, flagging
#' the first date it is breached.
#'
#' @note The boundary is closed-form throughout: a fixed asymptotic
#' constant (\code{boundary = "asymptotic"}, \code{b_alpha = 4.6}) or a
#' published finite-sample table lookup (\code{boundary = "finite"}, Homm
#' & Breitung (2012)'s Table 8) -- no simulation, no separate cv function.
#'
#' Unlike \code{\link{radf_monitor}} (Family A, a recursive ADF-family
#' statistic requiring a wild bootstrap to calibrate its boundary), this
#' is a structurally different statistic -- a standardized running sum,
#' not a recursive regression -- with an asymptotic closed-form boundary
#' (Chu, Stinchcombe & White 1996's inequality, HB's eq. 28): no
#' bootstrap, no simulation, no dependence on the data beyond the running
#' variance estimate itself.
#'
#' \code{type = "kernel"} instead uses Astill, Harvey, Leybourne, Taylor &
#' Zu (2023)'s volatility-robust modification ("CUSUMV"): each first
#' difference is standardized by its own one-sided kernel spot-variance
#' estimate (their eq. 6-7) instead of a single running variance, before
#' cumulating. Their Corollary 1 establishes the \emph{same} boundary
#' function delivers a controlled asymptotic false-alarm rate even under
#' time-varying volatility, unlike the standard CUSUM statistic, which
#' requires homoskedasticity for its own size-control result to hold.
#'
#' @inheritParams radf_monitor
#' @param b_alpha The boundary constant (HB's eq. 29). Default \code{4.6},
#' HB's own one-sided asymptotic calibration for a 5\% significance level
#' (their Section 3); this is an asymptotic upper bound on the false-
#' alarm probability (Chu, Stinchcombe & White 1996), not an exact size,
#' so it is typically conservative in finite samples. Ignored when
#' \code{boundary = "finite"}.
#' @param boundary \code{"asymptotic"} (default) uses \code{b_alpha}
#' directly. \code{"finite"} instead looks up HB's own finite-sample
#' boundary constant (their Table 8) from \code{level} and the realized
#' training length/monitoring-horizon ratio -- \code{level} must then be
#' one of \code{0.90}, \code{0.95}, \code{0.99}.
#' @param level Nominal confidence level when \code{boundary = "finite"}
#' (default \code{0.95}); ignored when \code{boundary = "asymptotic"}.
#' @param type \code{"standard"} (default) for Homm & Breitung (2012)'s
#' original CUSUM statistic, or \code{"kernel"} for Astill, Harvey,
#' Leybourne, Taylor & Zu (2023)'s volatility-robust "CUSUMV" variant.
#' @param N Bandwidth/window length for the one-sided kernel spot-variance
#' estimator when \code{type = "kernel"}. Default \code{20}, the authors'
#' own empirically-recommended value (their Section 3: "setting H = 20
#' delivered a procedure with the best trade-off" between false-alarm
#' robustness and power). Ignored when \code{type = "standard"}.
#' @param kernel Kernel for the spot-variance estimator when
#' \code{type = "kernel"}, \code{"gaussian"} (default) or \code{"uniform"}.
#' Ignored when \code{type = "standard"}.
#'
#' @return An object of class \code{monitor_cusum_obj}: a list with the
#' monitoring-region statistic path (\code{S}) and \code{boundary}, the
#' training window length \code{T_star}, and \code{alarm}/\code{alarm_date}
#' (the first breach, \code{NA} if none).
#'
#' @references Homm, U., & Breitung, J. (2012). Testing for speculative
#' bubbles in stock markets: A comparison of alternative methods. Journal
#' of Financial Econometrics, 10(1), 198-231.
#'
#' @references Chu, C. S. J., Stinchcombe, M., & White, H. (1996).
#' Monitoring structural change. Econometrica, 64(5), 1045-1065.
#'
#' @references Astill, S., Harvey, D. I., Leybourne, S. J., Taylor, A.
#' M. R., & Zu, Y. (2023). CUSUM-based monitoring for explosive episodes
#' in financial data in the presence of time-varying volatility. Journal
#' of Financial Econometrics, 21(1), 187-227.
#'
#' @seealso \code{\link{radf_monitor}} for the recursive-ADF (Family A)
#' monitoring alternative.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- monitor_cusum(sim_data$psy1, r_star = 0.5)
#' print(res)
#' }
#'
#' @export
monitor_cusum <- function(data, r_star = 0.5, b_alpha = 4.6,
                        boundary = c("asymptotic", "finite"), level = 0.95,
                        type = c("standard", "kernel"), N = 20,
                        kernel = c("gaussian", "uniform")) {
  type <- match.arg(type)
  kernel <- match.arg(kernel)
  boundary <- match.arg(boundary)
  x <- parse_data(data)
  n <- nrow(x)

  T_star <- if (r_star < 1) round(r_star * n) else as.integer(r_star)
  if (T_star < 3L) {
    stop_glue("Training window ('r_star') is too short.")
  }
  if (T_star >= n) {
    stop_glue("Training window ('r_star') must leave at least one monitoring observation.")
  }
  if (boundary == "finite") {
    b_alpha <- hb_cusum_finite_q(level, T_star, n / T_star)
  }

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  S_path <- boundary_path <- matrix(NA_real_, n - T_star, nc, dimnames = list(NULL, snames))
  alarm <- setNames(rep(NA_integer_, nc), snames)

  for (j in seq_len(nc)) {
    path <- if (type == "kernel") {
      cusum_stat_path_kernel(x[, j], T_star, b_alpha, N, kernel)
    } else {
      cusum_stat_path(x[, j], T_star, b_alpha)
    }
    S_path[, j] <- path$S
    boundary_path[, j] <- path$boundary
    breach <- which(path$S > path$boundary)
    if (length(breach) > 0L) alarm[j] <- path$t[breach[1L]]
  }

  alarm_date <- vapply(alarm, function(i) {
    if (is.na(i)) NA_character_ else as.character(idx[i])
  }, character(1))

  list(
    S = S_path, boundary = boundary_path, T_star = T_star,
    alarm = alarm, alarm_date = alarm_date
  ) %>%
    add_attr(
      index = idx, series_names = snames, n = n, b_alpha = b_alpha
    ) %>%
    add_class("monitor_cusum_obj")
}

#' @export
print.monitor_cusum_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "monitor_cusum (T* = {x$T_star} / {attr(x, 'n')}, b_alpha = {attr(x, 'b_alpha')})"
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
