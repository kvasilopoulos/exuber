# Real-time monitoring. Phillips & Shi (2020, in Handbook of Statistics
# vol. 42, "Real time monitoring of asset markets: Bubbles and crises";
# "PS"). See docs/monitoring.md, "Cost/feasibility note", for
# the full evaluation this implements -- Family A/PSY-style monitoring:
# fix a training window assumed free of exuberance, calibrate a critical
# value on it, then walk the sample forward comparing the running
# recursive statistic against that fixed boundary.
#
# This is an orchestration layer, not a new statistic: radf_wb_ps_cv()
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
# no bootstrap, no simulation.
#
# `s0 > 0` (his GSADF_{s0}(k) detector) -- re-triaged (2026-08-10) after
# initially being scoped out as needing "new recursion code": his own
# window-start range, floor(m*s0), is a FIXED cap tied to the training
# length only, not growing with the current monitoring point the way
# radf()'s own bsadf search range does -- so it is NOT a reindexing of
# bsadf, but it also does NOT need a new double-recursive C++ routine.
# ADF_{k1}^{t} for a fixed (small) band of k1 in [1, floor(m*s0)] and t
# ranging over the monitoring window is exactly the same closed-form
# cumulative-sum OLS t-statistic construction already used throughout
# this project (dating_hls.R's hls_segment_ssr(), radf_tt.R's
# gls_dfstat_grid()) -- just a WITH-INTERCEPT version (verified against
# radf()$badf to machine precision at k1_max = 1, and against brute-force
# lm() fits at k1_max > 1) restricted to a bounded band instead of the
# full recursive grid. `q_{0.4}^df`/`q_{0.8}^df` and the boundary
# function's `a/b/c` constants were already transcribed from his Table 1
# and eq. below it; only the statistic itself was missing.

# Kurozumi (2020) Table 1: SADF/GSADF/CS boundary scaling constants,
# transcribed from a rendered PDF page (the raw text extraction badly
# scrambled this table's sub/superscripts and numeric alignment), for
# significance level beta and monitoring-horizon ratio s_bar = k_bar/m,
# tabulated only at s_bar in {1, 3, 5}. Only q0_df (the SADF boundary
# constant, s0 = 0) is used by monitor(); the other columns are kept
# for completeness/future use (q04_df, q08_df for GSADF_{s0}, q025_cs/
# q045_cs for HB's CS/CUSUM detector at gamma = 0.25/0.45).
kurozumi_table1 <- data.frame(
  sbar = c(1, 1, 1, 3, 3, 3, 5, 5, 5),
  beta = c(0.10, 0.05, 0.01, 0.10, 0.05, 0.01, 0.10, 0.05, 0.01),
  q0_df = c(0.6946, 1.0381, 1.6474, 1.0299, 1.3330, 1.8978, 1.1308, 1.4255, 1.9735),
  q04_df = c(1.3969, 1.8081, 2.5927, 1.7088, 2.0737, 2.7677, 1.7988, 2.1480, 2.8276),
  q08_df = c(1.9369, 2.3330, 3.0941, 2.1315, 2.4944, 3.2136, 2.1794, 2.5369, 3.2616),
  q025_cs = c(1.5071, 1.7646, 2.2405, 1.6772, 1.9619, 2.4955, 1.7326, 2.0182, 2.5884),
  q045_cs = c(2.1300, 2.3948, 2.9265, 2.1958, 2.4638, 3.0163, 2.2057, 2.4844, 3.0476)
)

# Look up q_0^df (SADF boundary constant) for a given `sig_lvl`
# and monitoring-horizon ratio `s_bar`, snapping `s_bar` to the nearest of
# Kurozumi's three tabulated values {1, 3, 5}. `sig_lvl` must correspond
# exactly to one of the table's three significance levels (90, 95, 99 on
# the package's 0-100 scale) -- no interpolation across levels is attempted.
kurozumi_sadf_q <- function(sig_lvl, s_bar) {
  choices <- c(90, 95, 99)
  match_idx <- which(abs(sig_lvl - choices) < 1e-8)
  beta <- c(0.10, 0.05, 0.01)[match_idx] # the tables index by test size
  if (length(match_idx) == 0L) {
    stop_glue(
      "'sig_lvl' must be one of {paste(choices, collapse = ', ')} ",
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

# Same lookup, for the GSADF_{s0} boundary constant (q04_df/q08_df
# columns), s0 snapped to the nearest of Kurozumi's two tabulated cases
# {0.4, 0.8}.
kurozumi_gsadf_q <- function(sig_lvl, s_bar, s0) {
  choices <- c(90, 95, 99)
  match_idx <- which(abs(sig_lvl - choices) < 1e-8)
  beta <- c(0.10, 0.05, 0.01)[match_idx] # the tables index by test size
  if (length(match_idx) == 0L) {
    stop_glue(
      "'sig_lvl' must be one of {paste(choices, collapse = ', ')} ",
      "for boundary = 'kurozumi' (Kurozumi (2020)'s Table 1 only tabulates ",
      "these significance levels)."
    )
  }
  sbar_snap <- c(1, 3, 5)[which.min(abs(s_bar - c(1, 3, 5)))]
  s0_snap <- c(0.4, 0.8)[which.min(abs(s0 - c(0.4, 0.8)))]
  row <- kurozumi_table1[
    kurozumi_table1$sbar == sbar_snap & abs(kurozumi_table1$beta - beta) < 1e-8,
  ]
  if (s0_snap == 0.4) row$q04_df else row$q08_df
}

# GSADF_{s0}(k)'s boundary function, g_{s0}^df(k/m) := q_{s0}^df * (a_{s0}
# + b_{s0} * log(c_{s0} + k/m)) -- confirmed via rendered PDF page.
kurozumi_gsadf_abc <- data.frame(
  s0 = c(0.4, 0.8),
  a = c(0.76, 0.73),
  b = c(0.02, 0.03),
  c = c(0.34, 0.90)
)

# Closed-form GSADF_{s0}(k) statistic path for a single series: max over
# window starts k1 in [1, floor(T_star*s0)] of the WITH-INTERCEPT ADF
# t-statistic on window [k1, t], for every monitoring-period t = T_star+1,
# ..., n. Same cumulative-sum-difference construction as
# radf_tt.R's gls_dfstat_grid()/dating_hls.R's hls_segment_ssr(), but (a)
# with an intercept (verified to match radf()$badf exactly at k1_max = 1)
# and (b) restricted to a bounded k1 band instead of the full grid, since
# GSADF_{s0}'s own window-start range never grows past floor(T_star*s0).
kurozumi_gsadf_stat <- function(y, T_star, s0) {
  n <- length(y)
  dy <- diff(y)
  ylag <- y[1:(n - 1L)]
  cs_x <- c(0, cumsum(ylag))
  cs_x2 <- c(0, cumsum(ylag^2))
  cs_y <- c(0, cumsum(dy))
  cs_y2 <- c(0, cumsum(dy^2))
  cs_xy <- c(0, cumsum(ylag * dy))

  k1_max <- max(floor(T_star * s0), 1L)
  a_idx <- seq_len(k1_max)
  b_idx <- T_star:(n - 1L)

  Sx <- outer(cs_x[b_idx + 1L], cs_x[a_idx], "-")
  Sx2 <- outer(cs_x2[b_idx + 1L], cs_x2[a_idx], "-")
  Sy <- outer(cs_y[b_idx + 1L], cs_y[a_idx], "-")
  Sy2 <- outer(cs_y2[b_idx + 1L], cs_y2[a_idx], "-")
  Sxy <- outer(cs_xy[b_idx + 1L], cs_xy[a_idx], "-")
  L <- outer(b_idx, a_idx, "-") + 1L

  Sxx_c <- Sx2 - Sx^2 / L
  Sxy_c <- Sxy - Sx * Sy / L
  Syy_c <- Sy2 - Sy^2 / L
  beta <- Sxy_c / Sxx_c
  ssr <- Syy_c - beta * Sxy_c
  sigma2 <- ssr / (L - 2)
  tstat <- beta / sqrt(sigma2 / Sxx_c)

  apply(tstat, 1, max, na.rm = TRUE)
}

# Homm & Breitung (2012, J. Financial Econometrics 10(1), 198-231)'s
# FLUC monitoring detector. Their eq. 27 (confirmed via rendered PDF
# page): Z_t = (rho_hat_t - 1)/sigma_hat_{rho_t} = DF_{t/n} -- the
# ordinary recursive/expanding-window OLS ADF t-statistic on the sample
# {y_0, ..., y_t}, i.e. exactly radf()'s existing `badf` sequence (the
# same statistic already confirmed, earlier in this file, to equal
# Kurozumi's SADF(k)). Their eq. 29/31 rejection rule, DF_{t/n} >
# kappa_t with kappa_t = sqrt(b_{k,alpha} + log(t/n)), has the same
# functional form as their own CUSUM boundary (this file's `monitor_cusum`
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
    c(7.00, 7.84, 8.26, 8.49, 8.66, 9.12, 9.19) # n=20,  alpha=0.01
  )
  tbl <- data.frame(n = rep(n_grid, each = 3), alpha = rep(alpha_grid, times = 3), q)
  colnames(tbl) <- c("n", "alpha", paste0("k", k_grid))
  tbl
})

# Look up b_{k,alpha} for a given `sig_lvl`, training length
# `n_train`, and monitoring-horizon ratio `k`, snapping `n_train` to the
# nearest of {20, 50, 100} and `k` to the nearest of {2,...,10}. `sig_lvl`
# must correspond exactly to one of the table's three significance
# levels.
hb_fluc_q <- function(sig_lvl, n_train, k) {
  choices <- c(90, 95, 99)
  match_idx <- which(abs(sig_lvl - choices) < 1e-8)
  beta <- c(0.10, 0.05, 0.01)[match_idx] # the tables index by test size
  if (length(match_idx) == 0L) {
    stop_glue(
      "'sig_lvl' must be one of {paste(choices, collapse = ', ')} ",
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
#' \code{monitor} implements real-time monitoring. You fix a training window
#' \code{[1, T*]} that is assumed free of exuberance and calibrate a critical value
#' on it. The function then compares the running recursive statistic at each
#' subsequent point \code{T*+1, ..., T} with that fixed boundary and flags the first
#' date at which the boundary is breached.
#'
#' \code{boundary = "bootstrap"} (the default) implements Phillips & Shi (2020).
#' The boundary is a wild-bootstrap quantile of the GSADF-type statistic (see the
#' \code{tb} parameter of \code{\link{radf_wb_ps_cv}}), and it is compared with the
#' \code{bsadf} sequence of \code{radf()}. The function calibrates on the training
#' window \emph{only} (\code{data[1:T*]}) and not on the full series. The null-model
#' fit inside \code{\link{radf_wb_ps_cv}} (\code{adf_res()}) uses all the data it
#' is given and does not truncate them to \code{tb}, so passing data after
#' \code{T*}, which may be explosive, directly to it would leak future information
#' into the calibration of the null.
#'
#' \code{boundary = "kurozumi"} implements the closed-form alternative of Kurozumi
#' (2020). It needs no bootstrap and compares a published constant (his Table 1)
#' with the \code{badf} sequence of \code{radf()}. The default \code{s0 = 0} gives
#' his \code{SADF(k)} detector, where the window start is fixed at 1. Setting
#' \code{s0} to \code{0.4} or \code{0.8} switches to his \code{GSADF_{s0}(k)}
#' generalization. The window start then ranges over \code{[1, floor(T* * s0)]}
#' and is not fixed at \code{1}, and the comparison uses his boundary function,
#' which varies with \code{k} and is not constant, together with its own published
#' scaling constant. \code{sig_lvl} must be one of \code{90}, \code{95} or
#' \code{99}, the levels that his table tabulates.
#'
#' \code{boundary = "fluc"} implements the FLUC detector of Homm & Breitung (2012).
#' Their \code{DF_{t/n}} is also exactly the \code{badf} sequence of \code{radf()},
#' and it is compared with a published constant from their Table 7 (the case
#' without detrending) and not with a simulated one. \code{sig_lvl} must be one of
#' \code{90}, \code{95} or \code{99}.
#'
#' @inheritParams radf
#' @param r_star The end of the training window: a fraction in \code{(0, 1)} of the
#' sample (default \code{0.5}), or an integer number of observations if
#' \code{>= 1}.
#' @param nboot Number of wild bootstrap replications for the training critical
#' value. It is ignored unless \code{boundary = "bootstrap"}.
#' @param sig_lvl Significance level for the monitoring boundary on the 0 to 100
#' scale used throughout the package, one of \code{90}, \code{95} (default) or
#' \code{99}.
#' @param type Lag selection for the wild bootstrap process, passed to
#' \code{\link{radf_wb_ps_cv}}. It is ignored unless \code{boundary = "bootstrap"}.
#' @param seed Optional seed for the bootstrap draws. It is ignored unless
#' \code{boundary = "bootstrap"}.
#' @param boundary \code{"bootstrap"} (default, Phillips & Shi 2020),
#' \code{"kurozumi"} (the closed-form SADF/GSADF boundary of Kurozumi 2020) or
#' \code{"fluc"} (the FLUC boundary of Homm & Breitung 2012).
#' @param s0 The range of window starts of Kurozumi (2020), as a fraction of the
#' training length. It is used only when \code{boundary = "kurozumi"}. The default
#' \code{0} is the \code{SADF} case, with the window start fixed at \code{1}.
#' \code{0.4} or \code{0.8} switches to the \code{GSADF_{s0}} case, where the window
#' start ranges over \code{[1, floor(T* * s0)]}. These are the only two values for
#' which the scaling constants of his boundary function are tabulated.
#'
#' @return An object of class \code{monitor_obj}: a list with the full-sample
#' statistic path (\code{stat}, which is \code{bsadf} for
#' \code{boundary = "bootstrap"} and \code{badf} for \code{"kurozumi"} and
#' \code{"fluc"}), the calibrated \code{boundary} (one flat value for each series),
#' the length of the training window \code{T_star}, and \code{alarm} and
#' \code{alarm_date} (the first observation or date in the monitoring period at
#' which \code{stat} breaches the boundary, \code{NA} if it never does).
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
#' @seealso \code{\link{radf_wb_ps_cv}} for the underlying wild bootstrap, and
#' \code{\link{datestamp}} for the existing full-sample dating of origination and
#' collapse, which is not a monitoring procedure.
#'
#' @note The function returns its own class and not `radf_obj`, so it does not work
#' with `summary()`, `\link{datestamp}` and `tidy`. It has its own `print()` and
#' `autoplot()` methods instead. `print()` shows the boundary and the alarm. See
#' `vignette("naming-and-analysis", package = "exuber")` for which functions fit
#' the shared pipeline and which do not.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' # A bubble-free training window (first half), explosive from t = 150 on
#' y <- sim_psy1(n = 200, te = 150, tf = 200, seed = 7)
#' # Default: Phillips & Shi (2020) wild bootstrap boundary
#' mon <- monitor(y, r_star = 0.5, nboot = 200)
#' print(mon)
#' autoplot(mon)
#'
#' # Closed-form boundary of Kurozumi (2020), which needs no bootstrap
#' mon_kz <- monitor(y, r_star = 0.5, boundary = "kurozumi")
#' autoplot(mon_kz)
#'
#' # Homm & Breitung (2012) FLUC boundary
#' autoplot(monitor(y, r_star = 0.5, boundary = "fluc"))
#' }
#'
#' @family monitoring
#' @export
monitor <- function(
  data,
  r_star = 0.5,
  minw = NULL,
  nboot = 500L,
  sig_lvl = 95,
  lag = 0L,
  type = c("fixed", "aic", "bic"),
  seed = NULL,
  boundary = c("bootstrap", "kurozumi", "fluc"),
  s0 = 0
) {
  type <- match.arg(type)
  boundary <- match.arg(boundary)
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)

  T_star <- training_window(r_star, n, min_len = minw + lag + 1L)

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)

  if (boundary == "kurozumi" && s0 > 0) {
    s_bar <- (n - T_star) / T_star
    q <- kurozumi_gsadf_q(sig_lvl, s_bar, s0)
    abc <- kurozumi_gsadf_abc[which.min(abs(kurozumi_gsadf_abc$s0 - s0)), ]
    k_seq <- seq_len(n - T_star)
    boundary_path <- q * (abc$a + abc$b * log(abc$c + k_seq / T_star))

    stat_path <- matrix(NA_real_, length(k_seq), nc, dimnames = list(NULL, snames))
    for (j in seq_len(nc)) {
      stat_path[, j] <- kurozumi_gsadf_stat(as.numeric(x[, j]), T_star, s0)
    }

    alarm <- setNames(rep(NA_integer_, nc), snames)
    for (j in seq_len(nc)) {
      breach <- which(stat_path[, j] > boundary_path)
      if (length(breach) > 0L) alarm[j] <- T_star + breach[1L]
    }
    alarm_date <- vapply(
      alarm,
      function(i) {
        if (is.na(i)) NA_character_ else as.character(idx[i])
      },
      character(1)
    )

    return(
      list(
        stat = stat_path,
        boundary = boundary_path,
        T_star = T_star,
        alarm = alarm,
        alarm_date = alarm_date
      ) %>%
        add_attr(
          index = idx,
          series_names = snames,
          minw = minw,
          lag = lag,
          n = n,
          sig_lvl = sig_lvl,
          iter = NA_integer_,
          boundary_type = "kurozumi",
          s0 = s0,
          q = q,
          stat_offset = T_star
        ) %>%
        add_class("monitor_obj")
    )
  }

  full <- radf(x, minw = minw, lag = lag)
  mon_from <- max(T_star - minw - lag + 1L, 1L)
  mon_rows <- mon_from:nrow(full$bsadf)

  if (boundary == "kurozumi") {
    s_bar <- (n - T_star) / T_star
    q <- kurozumi_sadf_q(sig_lvl, s_bar)
    stat_path <- full$badf
    boundary_vec <- setNames(rep(q, nc), snames)
    iter <- NA_integer_
  } else if (boundary == "fluc") {
    k <- n / T_star
    q <- hb_fluc_q(sig_lvl, T_star, k)
    stat_path <- full$badf
    boundary_vec <- setNames(rep(q, nc), snames)
    iter <- NA_integer_
  } else {
    assert_sig_lvl(sig_lvl)
    lvl_lab <- paste0(sig_lvl, "%")
    cv <- radf_wb_ps_cv(
      x[1:T_star, , drop = FALSE],
      minw = minw,
      nboot = nboot,
      adflag = lag,
      type = type,
      tb = T_star,
      seed = seed
    )
    boundary_vec <- setNames(cv$gsadf_cv[, lvl_lab], snames)
    stat_path <- full$bsadf
    iter <- nboot
  }

  alarm <- setNames(rep(NA_integer_, nc), snames)
  for (j in seq_len(nc)) {
    breach <- which(stat_path[mon_rows, j] > boundary_vec[j])
    if (length(breach) > 0L) {
      alarm[j] <- mon_rows[breach[1L]] + minw + lag
    }
  }
  alarm_date <- vapply(
    alarm,
    function(i) {
      if (is.na(i)) NA_character_ else as.character(idx[i])
    },
    character(1)
  )

  list(
    stat = stat_path,
    boundary = boundary_vec,
    T_star = T_star,
    alarm = alarm,
    alarm_date = alarm_date
  ) %>%
    add_attr(
      index = idx,
      series_names = snames,
      minw = minw,
      lag = lag,
      n = n,
      sig_lvl = sig_lvl,
      iter = iter,
      boundary_type = boundary,
      s0 = 0,
      stat_offset = minw + lag
    ) %>%
    add_class("monitor_obj")
}

#' Plot method for monitor() output
#'
#' Plots the monitoring statistic against its boundary, with one panel for each series and vertical markers at the end of the training sample and at the alarm date.
#'
#' @param object An object of class \code{monitor_obj}, the output of \code{\link{monitor}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{monitor}}
#' @export
autoplot.monitor_obj <- function(object, ...) {
  offset <- attr(object, "stat_offset")
  pos <- seq_len(nrow(object$stat)) + offset
  snames <- colnames(object$stat)
  vlines <- bind_rows(
    tibble(id = snames, label = "training end", at = object$T_star),
    tibble(id = names(object$alarm), label = "alarm", at = object$alarm) %>% tidyr::drop_na(at)
  )
  autoplot_stat_boundary(pos, object$stat, object$boundary, vlines = vlines)
}

#' @export
print.monitor_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  s0 <- attr(x, "s0") %||% 0
  header <- if (s0 > 0) {
    glue(
      "monitor (T* = {x$T_star} / {attr(x, 'n')}, minw = {get_minw(x)}, ",
      "sig_lvl = {attr(x, 'sig_lvl')}%, boundary = kurozumi, s0 = {s0}, ",
      "q = {round(attr(x, 'q'), 4)})"
    )
  } else {
    glue(
      "monitor (T* = {x$T_star} / {attr(x, 'n')}, minw = {get_minw(x)}, ",
      "sig_lvl = {attr(x, 'sig_lvl')}%, boundary = {attr(x, 'boundary_type')})"
    )
  }
  cat_rule(left = header)
  cat_line()
  if (s0 > 0) {
    print(
      data.frame(
        series = names(x$alarm),
        alarm = x$alarm,
        alarm_date = x$alarm_date,
        row.names = NULL
      ),
      digits = digits,
      print.gap = 2L,
      row.names = FALSE
    )
  } else {
    print(
      data.frame(
        series = names(x$boundary),
        boundary = x$boundary,
        alarm = x$alarm,
        alarm_date = x$alarm_date,
        row.names = NULL
      ),
      digits = digits,
      print.gap = 2L,
      row.names = FALSE
    )
  }
  cat_line()
}
