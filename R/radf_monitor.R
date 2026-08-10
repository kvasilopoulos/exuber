# Real-time monitoring. Phillips & Shi (2020, in Handbook of Statistics
# vol. 42, "Real time monitoring of asset markets: Bubbles and crises";
# "PS"). See docs/enhancements/monitoring.md, "Cost/feasibility note", for
# the full evaluation this implements -- Family A/PSY-style monitoring:
# fix a training window assumed free of exuberance, calibrate a critical
# value on it, then walk the sample forward comparing the running
# recursive statistic against that fixed boundary.
#
# This is an orchestration layer, not a new statistic: radf_wb_cv2()
# already implements the PS wild bootstrap and already has a `tb`
# parameter for exactly this training-critical-value use (its own roxygen
# docs cite PS 2020); radf()'s own BSADF sequence at time t depends only
# on data up to t by construction (verified: radf(y)$bsadf[t] is
# bit-identical to a fresh radf(y[1:t])$bsadf's last value), so a single
# full-sample radf() call gives the whole monitoring path with no
# per-point re-fitting. What's missing (built here) is just: slice the
# training window, calibrate, walk forward, flag the first breach.
#
# `boundary = "kurozumi"` adds a second, bootstrap-free calibration:
# Kurozumi (2020, Econometric Reviews 39(5), 510-538)'s SADF(k) detector
# is exactly radf()'s `badf` sequence (verified: bit-identical to a
# from-scratch OLS ADF t-stat with a fixed start at t=1), compared
# against a closed-form/published constant boundary from his Table 1 --
# no bootstrap, no simulation. His GSADF_{s0}(k) detector (s0 > 0) is
# NOT simply radf()'s `bsadf`: its window-start search range is capped at
# a FIXED fraction of the training length (floor(m*s0)), whereas
# radf()'s own bsadf search range grows with the current monitoring
# point -- a genuinely different (and not currently implemented) double
# recursion. Only the s0 = 0 (SADF) case is implemented here.

# Kurozumi (2020) Table 1: SADF/GSADF/CS boundary scaling constants,
# transcribed from a rendered PDF page (the raw text extraction badly
# scrambled this table's sub/superscripts and numeric alignment), for
# significance level beta and monitoring-horizon ratio s_bar = k_bar/m,
# tabulated only at s_bar in {1, 3, 5}. Only q0_df (the SADF boundary
# constant, s0 = 0) is used by radf_monitor(); the other columns are kept
# for completeness/future use (q04_df, q08_df for GSADF_{s0}, q025_cs/
# q045_cs for HB's CS/CUSUM detector at gamma = 0.25/0.45).
kurozumi_table1 <- data.frame(
  sbar    = c(1, 1, 1, 3, 3, 3, 5, 5, 5),
  beta    = c(0.10, 0.05, 0.01, 0.10, 0.05, 0.01, 0.10, 0.05, 0.01),
  q0_df   = c(0.6946, 1.0381, 1.6474, 1.0299, 1.3330, 1.8978, 1.1308, 1.4255, 1.9735),
  q04_df  = c(1.3969, 1.8081, 2.5927, 1.7088, 2.0737, 2.7677, 1.7988, 2.1480, 2.8276),
  q08_df  = c(1.9369, 2.3330, 3.0941, 2.1315, 2.4944, 3.2136, 2.1794, 2.5369, 3.2616),
  q025_cs = c(1.5071, 1.7646, 2.2405, 1.6772, 1.9619, 2.4955, 1.7326, 2.0182, 2.5884),
  q045_cs = c(2.1300, 2.3948, 2.9265, 2.1958, 2.4638, 3.0163, 2.2057, 2.4844, 3.0476)
)

# Look up q_0^df (SADF boundary constant) for a given confidence `level`
# and monitoring-horizon ratio `s_bar`, snapping `s_bar` to the nearest of
# Kurozumi's three tabulated values {1, 3, 5}. `level` must correspond
# exactly to one of the table's three significance levels (0.10, 0.05,
# 0.01) -- no interpolation across significance levels is attempted.
kurozumi_sadf_q <- function(level, s_bar) {
  beta <- 1 - level
  beta_choices <- c(0.10, 0.05, 0.01)
  match_idx <- which(abs(beta - beta_choices) < 1e-8)
  if (length(match_idx) == 0L) {
    stop_glue(
      "'level' must be one of {paste(1 - beta_choices, collapse = ', ')} ",
      "for boundary = 'kurozumi' (Kurozumi (2020)'s Table 1 only tabulates ",
      "these significance levels)."
    )
  }
  sbar_snap <- c(1, 3, 5)[which.min(abs(s_bar - c(1, 3, 5)))]
  row <- kurozumi_table1[
    kurozumi_table1$sbar == sbar_snap & abs(kurozumi_table1$beta - beta) < 1e-8,
  ]
  row$q0_df
}

# Homm & Breitung (2012, J. Financial Econometrics 10(1), 198-231)'s
# FLUC monitoring detector. Their eq. 27 (confirmed via rendered PDF
# page): Z_t = (rho_hat_t - 1)/sigma_hat_{rho_t} = DF_{t/n} -- the
# ordinary recursive/expanding-window OLS ADF t-statistic on the sample
# {y_0, ..., y_t}, i.e. exactly radf()'s existing `badf` sequence (the
# same statistic already confirmed, earlier in this file, to equal
# Kurozumi's SADF(k)). Their eq. 29/31 rejection rule, DF_{t/n} >
# kappa_t with kappa_t = sqrt(b_{k,alpha} + log(t/n)), has the same
# functional form as their own CUSUM boundary (this file's `radf_cusum`
# sibling) but a DIFFERENT calibration constant b_{k,alpha} that (their
# own text) "we determine... by means of simulation" -- no closed form
# available. Table 7 (part i, "without detrending", matching radf()'s
# own no-trend default) publishes exactly this constant, indexed by
# training length n, significance level alpha, and monitoring-horizon
# ratio k = N/n (N the total sample including training) -- used
# directly here, no new simulation needed on exuber's end.

# Homm & Breitung (2012) Table 7(i): FLUC boundary constant b_{k,alpha},
# without detrending, transcribed from a rendered PDF page (the raw text
# extraction badly scrambled this table). Tabulated at training length
# n in {20, 50, 100}, significance level alpha in {0.10, 0.05, 0.01},
# and horizon ratio k in {2, 3, 4, 5, 6, 8, 10}.
hb_fluc_table <- local({
  k_grid <- c(2, 3, 4, 5, 6, 8, 10)
  n_grid <- c(100, 50, 20)
  alpha_grid <- c(0.10, 0.05, 0.01)
  q <- rbind(
    c(3.05, 3.60, 3.93, 4.15, 4.31, 4.48, 4.57), # n=100, alpha=0.10
    c(4.50, 5.14, 5.55, 5.69, 5.89, 6.05, 6.26), # n=100, alpha=0.05
    c(7.76, 8.59, 9.06, 9.48, 9.62, 9.79, 9.99), # n=100, alpha=0.01
    c(2.80, 3.33, 3.62, 3.80, 3.96, 4.14, 4.27), # n=50,  alpha=0.10
    c(4.19, 4.80, 5.11, 5.34, 5.50, 5.72, 5.81), # n=50,  alpha=0.05
    c(7.30, 8.11, 8.43, 8.82, 8.86, 9.25, 9.49), # n=50,  alpha=0.01
    c(2.49, 3.12, 3.44, 3.65, 3.78, 3.99, 4.12), # n=20,  alpha=0.10
    c(3.88, 4.56, 4.86, 5.06, 5.19, 5.38, 5.52), # n=20,  alpha=0.05
    c(7.00, 7.84, 8.26, 8.49, 8.66, 9.12, 9.19)  # n=20,  alpha=0.01
  )
  tbl <- data.frame(n = rep(n_grid, each = 3), alpha = rep(alpha_grid, times = 3), q)
  colnames(tbl) <- c("n", "alpha", paste0("k", k_grid))
  tbl
})

# Look up b_{k,alpha} for a given confidence `level`, training length
# `n_train`, and monitoring-horizon ratio `k`, snapping `n_train` to the
# nearest of {20, 50, 100} and `k` to the nearest of {2,...,10}. `level`
# must correspond exactly to one of the table's three significance
# levels.
hb_fluc_q <- function(level, n_train, k) {
  beta <- 1 - level
  beta_choices <- c(0.10, 0.05, 0.01)
  match_idx <- which(abs(beta - beta_choices) < 1e-8)
  if (length(match_idx) == 0L) {
    stop_glue(
      "'level' must be one of {paste(1 - beta_choices, collapse = ', ')} ",
      "for boundary = 'fluc' (Homm & Breitung (2012)'s Table 7 only ",
      "tabulates these significance levels)."
    )
  }
  n_grid <- c(20, 50, 100)
  k_grid <- c(2, 3, 4, 5, 6, 8, 10)
  n_snap <- n_grid[which.min(abs(n_train - n_grid))]
  k_snap <- k_grid[which.min(abs(k - k_grid))]
  row <- hb_fluc_table[hb_fluc_table$n == n_snap & abs(hb_fluc_table$alpha - beta) < 1e-8, ]
  row[[paste0("k", k_snap)]]
}

#' Real-Time Monitoring for Explosive Bubbles
#'
#' \code{radf_monitor} implements real-time monitoring: fix a training
#' window \code{[1, T*]} assumed free of exuberance, calibrate a critical
#' value on it, then compare the running recursive statistic at each
#' subsequent point \code{T*+1, ..., T} against that fixed boundary,
#' flagging the first date it is breached.
#'
#' \code{boundary = "bootstrap"} (default) implements Phillips & Shi
#' (2020): the boundary is a wild-bootstrap quantile of the GSADF-type
#' statistic (\code{\link{radf_wb_cv2}}, its \code{tb} parameter),
#' compared against \code{radf()}'s \code{bsadf} sequence. Deliberately
#' calibrates on the training window \emph{only} (\code{data[1:T*]}), not
#' the full series: \code{\link{radf_wb_cv2}}'s underlying null-model fit
#' (\code{adf_res()}) uses whatever data it is given in full, with no
#' internal truncation to \code{tb} -- passing post-\code{T*} (possibly
#' explosive) data to it directly would leak future information into the
#' null calibration.
#'
#' \code{boundary = "kurozumi"} implements Kurozumi (2020)'s closed-form
#' alternative: no bootstrap at all, just a published constant (his Table 1)
#' compared against \code{radf()}'s \code{badf} sequence (his
#' \code{SADF(k)} detector -- the \code{s0 = 0}, fixed-start-at-1 case;
#' his \code{GSADF_{s0}(k)} generalization for \code{s0 > 0} is not
#' implemented, see Details). \code{level} must be one of \code{0.90},
#' \code{0.95}, or \code{0.99} (the levels his table tabulates).
#'
#' \code{boundary = "fluc"} implements Homm & Breitung (2012)'s FLUC
#' detector: their \code{DF_{t/n}} is likewise exactly \code{radf()}'s
#' \code{badf} sequence, compared against a published constant from
#' their Table 7 (no detrending case) rather than a simulated one.
#' \code{level} must be one of \code{0.90}, \code{0.95}, \code{0.99}.
#'
#' @inheritParams radf
#' @param r_star The end of the training window: a fraction in
#' \code{(0, 1)} of the sample (default \code{0.5}), or an integer
#' observation count if \code{>= 1}.
#' @param nboot Number of wild bootstrap replications for the training
#' critical value. Ignored unless \code{boundary = "bootstrap"}.
#' @param level Nominal confidence level for the monitoring boundary
#' (default \code{0.95}). When \code{boundary} is \code{"kurozumi"} or
#' \code{"fluc"}, must be one of \code{0.90}, \code{0.95}, \code{0.99}.
#' @param adflag,type Passed to \code{\link{radf_wb_cv2}} (lag length /
#' selection for the wild bootstrap DGP). Ignored unless
#' \code{boundary = "bootstrap"}.
#' @param seed Optional seed for the bootstrap draws. Ignored unless
#' \code{boundary = "bootstrap"}.
#' @param boundary \code{"bootstrap"} (default, Phillips & Shi 2020),
#' \code{"kurozumi"} (Kurozumi 2020's closed-form SADF boundary), or
#' \code{"fluc"} (Homm & Breitung 2012's FLUC boundary).
#'
#' @return An object of class \code{radf_monitor_obj}: a list with the
#' full-sample statistic path (\code{stat} -- \code{bsadf} for
#' \code{boundary = "bootstrap"}, \code{badf} for \code{"kurozumi"}/
#' \code{"fluc"}), the calibrated \code{boundary} (one flat value per
#' series), the training window length \code{T_star}, and
#' \code{alarm}/\code{alarm_date} (the first monitoring-period
#' observation/date at which \code{stat} breaches the boundary,
#' \code{NA} if never).
#'
#' @references Phillips, P. C., & Shi, S. (2020). Real time monitoring of
#' asset markets: Bubbles and crises. In Handbook of Statistics (Vol. 42,
#' pp. 61-80). Elsevier.
#'
#' @references Kurozumi, E. (2020). Asymptotic properties of bubble
#' monitoring tests. Econometric Reviews, 39(5), 510-538.
#'
#' @references Homm, U., & Breitung, J. (2012). Testing for speculative
#' bubbles in stock markets: A comparison of alternative methods.
#' Journal of Financial Econometrics, 10(1), 198-231.
#'
#' @seealso \code{\link{radf_wb_cv2}} for the underlying wild bootstrap,
#' and \code{\link{datestamp}} for the (non-monitoring, full-sample)
#' origination/collapse dating that already exists.
#'
#' @export
radf_monitor <- function(data, r_star = 0.5, minw = NULL, nboot = 500L,
                          level = 0.95, adflag = 0,
                          type = c("fixed", "aic", "bic"), seed = NULL,
                          boundary = c("bootstrap", "kurozumi", "fluc")) {
  type <- match.arg(type)
  boundary <- match.arg(boundary)
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)

  T_star <- if (r_star < 1) round(r_star * n) else as.integer(r_star)
  if (T_star <= minw + adflag) {
    stop_glue("Training window ('r_star') must exceed 'minw' (+ 'adflag').")
  }
  if (T_star >= n) {
    stop_glue("Training window ('r_star') must leave at least one monitoring observation.")
  }

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  full <- radf(x, minw = minw, lag = adflag)
  mon_from <- max(T_star - minw - adflag + 1L, 1L)
  mon_rows <- mon_from:nrow(full$bsadf)

  if (boundary == "kurozumi") {
    s_bar <- (n - T_star) / T_star
    q <- kurozumi_sadf_q(level, s_bar)
    stat_path <- full$badf
    boundary_vec <- setNames(rep(q, nc), snames)
    iter <- NA_integer_
  } else if (boundary == "fluc") {
    k <- n / T_star
    q <- hb_fluc_q(level, T_star, k)
    stat_path <- full$badf
    boundary_vec <- setNames(rep(q, nc), snames)
    iter <- NA_integer_
  } else {
    lvl_lab <- paste0(level * 100, "%")
    cv <- radf_wb_cv2(x[1:T_star, , drop = FALSE], minw = minw, nboot = nboot,
                       adflag = adflag, type = type, tb = T_star, seed = seed)
    boundary_vec <- setNames(cv$gsadf_cv[, lvl_lab], snames)
    stat_path <- full$bsadf
    iter <- nboot
  }

  alarm <- setNames(rep(NA_integer_, nc), snames)
  for (j in seq_len(nc)) {
    breach <- which(stat_path[mon_rows, j] > boundary_vec[j])
    if (length(breach) > 0L) {
      alarm[j] <- mon_rows[breach[1L]] + minw + adflag
    }
  }
  alarm_date <- vapply(alarm, function(i) {
    if (is.na(i)) NA_character_ else as.character(idx[i])
  }, character(1))

  list(
    stat = stat_path, boundary = boundary_vec, T_star = T_star,
    alarm = alarm, alarm_date = alarm_date
  ) %>%
    add_attr(
      index = idx, series_names = snames, minw = minw, lag = adflag,
      n = n, level = level, iter = iter, boundary_type = boundary
    ) %>%
    add_class("radf_monitor_obj")
}

#' @export
print.radf_monitor_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "radf_monitor (T* = {x$T_star} / {attr(x, 'n')}, minw = {get_minw(x)}, ",
    "level = {attr(x, 'level') * 100}%, boundary = {attr(x, 'boundary_type')})"
  ))
  cat_line()
  print(
    data.frame(
      series = names(x$boundary), boundary = x$boundary,
      alarm = x$alarm, alarm_date = x$alarm_date, row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
