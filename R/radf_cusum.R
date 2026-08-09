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
# HB propose two monitoring statistics (CUSUM and FLUC); only CUSUM is
# implemented here. FLUC's boundary constant b_{k,alpha} has no closed
# form and needs its own Monte Carlo calibration (HB's own text: "we
# determine the critical value b_{k,alpha} by means of simulation") --
# structurally closer to Family A's cost profile, not attempted this pass.

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

#' CUSUM Real-Time Monitoring for Explosive Bubbles
#'
#' \code{radf_cusum} implements Homm & Breitung (2012)'s CUSUM real-time
#' monitoring procedure: fix a training window \code{[1, T*]} assumed free
#' of exuberance, then compare the standardized cumulative sum of
#' post-training first differences, \code{S_t = (y_t - y_{T*}) /
#' sigma_hat_t}, against a closed-form boundary
#' \code{c_t * sqrt(t)}, \code{c_t = sqrt(b_alpha + log(t / T*))}, flagging
#' the first date it is breached.
#'
#' Unlike \code{\link{radf_monitor}} (Family A, a recursive ADF-family
#' statistic requiring a wild bootstrap to calibrate its boundary), this
#' is a structurally different statistic -- a standardized running sum,
#' not a recursive regression -- with an asymptotic closed-form boundary
#' (Chu, Stinchcombe & White 1996's inequality, HB's eq. 28): no
#' bootstrap, no simulation, no dependence on the data beyond the running
#' variance estimate itself.
#'
#' @inheritParams radf_monitor
#' @param b_alpha The boundary constant (HB's eq. 29). Default \code{4.6},
#' HB's own one-sided asymptotic calibration for a 5\% significance level
#' (their Section 3); this is an asymptotic upper bound on the false-
#' alarm probability (Chu, Stinchcombe & White 1996), not an exact size,
#' so it is typically conservative in finite samples.
#'
#' @return An object of class \code{radf_cusum_obj}: a list with the
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
#' @seealso \code{\link{radf_monitor}} for the recursive-ADF (Family A)
#' monitoring alternative.
#'
#' @export
radf_cusum <- function(data, r_star = 0.5, b_alpha = 4.6) {
  x <- parse_data(data)
  n <- nrow(x)

  T_star <- if (r_star < 1) round(r_star * n) else as.integer(r_star)
  if (T_star < 3L) {
    stop_glue("Training window ('r_star') is too short.")
  }
  if (T_star >= n) {
    stop_glue("Training window ('r_star') must leave at least one monitoring observation.")
  }

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  S_path <- boundary_path <- matrix(NA_real_, n - T_star, nc, dimnames = list(NULL, snames))
  alarm <- setNames(rep(NA_integer_, nc), snames)

  for (j in seq_len(nc)) {
    path <- cusum_stat_path(x[, j], T_star, b_alpha)
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
    add_class("radf_cusum_obj")
}

#' @export
print.radf_cusum_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "radf_cusum (T* = {x$T_star} / {attr(x, 'n')}, b_alpha = {attr(x, 'b_alpha')})"
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
