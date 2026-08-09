# Real-time monitoring. Phillips & Shi (2020, in Handbook of Statistics
# vol. 42, "Real time monitoring of asset markets: Bubbles and crises";
# "PS"). See docs/enhancements/monitoring.md, "Cost/feasibility note", for
# the full evaluation this implements -- Family A/PSY-style monitoring:
# fix a training window assumed free of exuberance, calibrate a critical
# value on it via wild bootstrap, then walk the sample forward comparing
# the running BSADF statistic against that fixed boundary.
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

#' Real-Time Monitoring for Explosive Bubbles
#'
#' \code{radf_monitor} implements Phillips & Shi (2020)'s real-time
#' monitoring procedure: fix a training window \code{[1, T*]} assumed free
#' of exuberance, calibrate a critical value on it via wild bootstrap
#' (\code{\link{radf_wb_cv2}}), then compare the running BSADF statistic
#' at each subsequent point \code{T*+1, ..., T} against that fixed
#' boundary, flagging the first date it is breached.
#'
#' Deliberately calibrates the critical value on the training window
#' \emph{only} (\code{data[1:T*]}), not the full series: \code{\link{radf_wb_cv2}}'s
#' underlying null-model fit (\code{adf_res()}) uses whatever data it is
#' given in full, with no internal truncation to \code{tb} -- passing
#' post-\code{T*} (possibly explosive) data to it directly would leak
#' future information into the null calibration.
#'
#' @inheritParams radf
#' @param r_star The end of the training window: a fraction in
#' \code{(0, 1)} of the sample (default \code{0.5}), or an integer
#' observation count if \code{>= 1}.
#' @param nboot Number of wild bootstrap replications for the training
#' critical value.
#' @param level Nominal confidence level for the monitoring boundary
#' (default \code{0.95}).
#' @param adflag,type Passed to \code{\link{radf_wb_cv2}} (lag length /
#' selection for the wild bootstrap DGP).
#' @param seed Optional seed for the bootstrap draws.
#'
#' @return An object of class \code{radf_monitor_obj}: a list with the
#' full-sample BSADF path (\code{bsadf}), the training-calibrated
#' \code{boundary} (one flat value per series), the training window length
#' \code{T_star}, and \code{alarm}/\code{alarm_date} (the first monitoring
#' -period observation/date at which \code{bsadf} breaches the boundary,
#' \code{NA} if never).
#'
#' @references Phillips, P. C., & Shi, S. (2020). Real time monitoring of
#' asset markets: Bubbles and crises. In Handbook of Statistics (Vol. 42,
#' pp. 61-80). Elsevier.
#'
#' @seealso \code{\link{radf_wb_cv2}} for the underlying wild bootstrap,
#' and \code{\link{datestamp}} for the (non-monitoring, full-sample)
#' origination/collapse dating that already exists.
#'
#' @export
radf_monitor <- function(data, r_star = 0.5, minw = NULL, nboot = 500L,
                          level = 0.95, adflag = 0,
                          type = c("fixed", "aic", "bic"), seed = NULL) {
  type <- match.arg(type)
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
  lvl_lab <- paste0(level * 100, "%")

  cv <- radf_wb_cv2(x[1:T_star, , drop = FALSE], minw = minw, nboot = nboot,
                     adflag = adflag, type = type, tb = T_star, seed = seed)
  boundary <- setNames(cv$gsadf_cv[, lvl_lab], snames)

  full <- radf(x, minw = minw, lag = adflag)

  # bsadf row k corresponds to calendar time (minw + adflag + k); the
  # monitoring region starts at the first row whose calendar time exceeds
  # T_star.
  mon_from <- max(T_star - minw - adflag + 1L, 1L)
  mon_rows <- mon_from:nrow(full$bsadf)

  nc <- ncol(x)
  alarm <- setNames(rep(NA_integer_, nc), snames)
  for (j in seq_len(nc)) {
    breach <- which(full$bsadf[mon_rows, j] > boundary[j])
    if (length(breach) > 0L) {
      alarm[j] <- mon_rows[breach[1L]] + minw + adflag
    }
  }
  alarm_date <- vapply(alarm, function(i) {
    if (is.na(i)) NA_character_ else as.character(idx[i])
  }, character(1))

  list(
    bsadf = full$bsadf, boundary = boundary, T_star = T_star,
    alarm = alarm, alarm_date = alarm_date
  ) %>%
    add_attr(
      index = idx, series_names = snames, minw = minw, lag = adflag,
      n = n, level = level, iter = nboot
    ) %>%
    add_class("radf_monitor_obj")
}

#' @export
print.radf_monitor_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "radf_monitor (T* = {x$T_star} / {attr(x, 'n')}, minw = {get_minw(x)}, ",
    "level = {attr(x, 'level') * 100}%)"
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
