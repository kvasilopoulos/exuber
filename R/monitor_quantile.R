# Wu, Shi & Wu (2025, JTSA 46(5), "Quantile analysis for financial bubble
# detection and surveillance", "WSW") -- the QPWY and QPSY recursive
# monitoring strategies of their Section 3.2 (eq. 25-26, 28-29). See
# docs/alternative-paradigms.md, "Quantile-based detection", for the full
# evaluation this implements.
#
# QPWY_r(tau) := t_T^{0,r}(tau), the quantile-regression t-ratio on the
# EXPANDING window [1, r] (radf()'s own badf shape); QPSY_r(tau, r0) :=
# sup_{r1} t_T^{r1,r}(tau), additionally sup'ing over every window start
# (radf()'s own bsadf shape). The point statistic needs genuine QR fits
# (no closed-form recursive update the way OLS has): O(T) for QPWY,
# O(T^2) for QPSY.
#
# Critical values come from Theorem 1 / Corollary 1-2: under the null,
# t_T^{r1,r2}(tau) => int W~ dB_psi / sqrt(int W~^2), with B_psi a
# Brownian motion correlated delta(tau) with W. Writing B_psi = delta*W +
# sqrt(1-delta^2)*V (V independent of W) gives
#   U_{r1,r2} = delta*Q_{r1,r2} + sqrt(1-delta^2)*Z_{r1,r2},
#   Q = int W~ dW / sqrt(int W~^2),  Z = int W~ dV / sqrt(int W~^2).
# Z_{r1,r2} is N(0,1) for any ONE window (the "z" of Corollary 1, all
# quantile_test() needs) but varies across windows. A monitoring boundary
# is a functional of the whole path, so Z must be simulated as a process
# -- the original QPWY boundary drew one z per replicate and reused it for
# every r, which understates sup_r U and oversizes the test (7% at
# delta = 0.8, 12% at 0.5, 22% at 0.2 for a nominal 5%, n = 200; found
# 2026-09-29, see docs/alternative-paradigms.md). quantile_boundary_sim()
# simulates discretized Q and Z for every window via prefix sums, O(1) per
# window, so the QPSY boundary costs no QR fits at all.

# QR t-ratio on one window `yy` (WSW's QUr statistic, eq. 18 / Section
# 3.2): density estimated from the window's own first differences.
quantile_window_stat <- function(yy, tau) {
  m <- length(yy)
  ylag <- yy[-m]
  yresp <- yy[-1L]
  alpha_hat <- quantreg::rq.fit(cbind(1, ylag), yresp, tau = tau, method = "br")$coefficients[2L]
  f_hat <- quantile_check_density(yresp - ylag, tau)$f_hat
  yPzy <- sum((ylag - mean(ylag))^2)
  unname((f_hat / sqrt(tau * (1 - tau))) * sqrt(yPzy) * (alpha_hat - 1))
}

# QPWY_r(tau) for every window-end r in `r_idx` -- window fixed at [1, r].
qpwy_stat_path <- function(y, tau, r_idx) {
  vapply(r_idx, function(r) quantile_window_stat(y[1:r], tau), numeric(1))
}

# QPSY_r(tau, r0) for every window-end r in `r_idx` -- sup over window
# starts r1 = 1, ..., r - minw (every window keeps >= minw regression
# observations, the same floor QPWY's first window has).
qpsy_stat_path <- function(y, tau, r_idx, minw) {
  vapply(
    r_idx,
    function(r) {
      max(vapply(1:(r - minw), function(r1) quantile_window_stat(y[r1:r], tau), numeric(1)))
    },
    numeric(1)
  )
}

# Simulated null path suprema, one column per entry of `delta`: for each
# replicate, sup over the monitoring path of U = delta*Q + sqrt(1-delta^2)*Z
# (see header). Windows are over the n - 1 regression pairs
# (y_{t-1}, Delta y_t); window (lo, hi] of pairs <-> y[(lo + 1):(hi + 1)],
# so QPWY's window [1, r] is (0, r - 1] and QPSY's [r1, r] is
# (r1 - 1, r - 1].
quantile_boundary_sim <- function(n, minw, nrep, delta, type = "qpwy", seed = NULL) {
  set_rng(seed)
  np <- n - 1L
  hi <- minw:np
  lo <- if (type == "qpwy") 0L else 0:(np - minw)
  valid <- outer(lo, hi, function(l, h) h - l >= minw)
  mk <- function(v) c(0, cumsum(v))
  win <- function(cs) outer(lo, hi, function(l, h) cs[h + 1L] - cs[l + 1L])
  L <- outer(lo, hi, function(l, h) h - l)
  L[!valid] <- NA
  out <- matrix(NA_real_, nrep, length(delta))
  for (i in seq_len(nrep)) {
    e <- stats::rnorm(np)
    v <- stats::rnorm(np)
    x <- c(0, cumsum(e))[1:np] # y_{t-1}
    Sx <- win(mk(x))
    sxx <- win(mk(x^2)) - Sx^2 / L
    Q <- (win(mk(x * e)) - Sx * win(mk(e)) / L) / sqrt(sxx)
    Z <- (win(mk(x * v)) - Sx * win(mk(v)) / L) / sqrt(sxx)
    for (j in seq_along(delta)) {
      U <- delta[j] * Q + sqrt(1 - delta[j]^2) * Z
      out[i, j] <- max(U[valid])
    }
  }
  out
}

# Path suprema from Algorithm 1 of WSW (i.i.d. residual bootstrap): resample
# the centred first differences with replacement, cumulate them into a null
# random walk, and recompute the whole QPWY/QPSY path on it. The paper
# draws T + b = T + 100 values and discards the first b to remove the
# initialisation effect. The statistic regresses with an intercept, so it
# does not depend on the starting level of the series, and with i.i.d.
# draws the last T values already have the same law as any other T; the
# burn-in would change nothing and is left out. Each replicate costs one
# full statistic path (O(T) QR fits for QPWY, O(T^2) for QPSY).
quantile_boot_max <- function(u, tau, minw, type) {
  n <- length(u) + 1L
  ystar <- cumsum(c(0, sample(u, n - 1L, replace = TRUE)))
  r_idx <- (minw + 1L):n
  max(
    if (type == "qpwy") {
      qpwy_stat_path(ystar, tau, r_idx)
    } else {
      qpsy_stat_path(ystar, tau, r_idx, minw)
    }
  )
}

quantile_boundary_boot <- function(y, tau, minw, nrep, type = "qpwy", seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  u <- diff(y)
  u <- u - mean(u)
  with_backend({
    p <- progressor(steps = nrep)
    res <- foreach(
      i = seq_len(nrep),
      .options.future = list(
        seed = TRUE,
        globals = structure(
          TRUE,
          add = c(
            "quantile_boot_max",
            "qpwy_stat_path",
            "qpsy_stat_path",
            "quantile_window_stat",
            "quantile_check_density",
            "quantile_narm"
          )
        )
      ),
      .inorder = FALSE
    ) %dofuture%
      {
        p()
        quantile_boot_max(u, tau, minw, type)
      }
  })
  unlist(res)
}

#' QPWY/QPSY Recursive Quantile Monitoring (Wu, Shi & Wu 2025)
#'
#' \code{monitor_quantile} implements the QPWY and QPSY real-time monitoring
#' strategies of Wu, Shi & Wu (2025). They are quantile-regression (QR) analogues
#' of the recursive ADF t-statistics of PWY and PSY, and they test at a chosen
#' conditional quantile \code{tau}, in contrast to the single full-sample test in
#' \code{\link{quantile_test}}. \code{type = "qpwy"} uses the expanding window
#' \code{[1, r]}, which has the shape of \code{badf} in \code{\link{radf}}.
#' \code{type = "qpsy"} also takes the supremum over every window start, which has
#' the shape of \code{bsadf}.
#'
#' The point statistic needs genuine QR fits, because there is no closed-form
#' recursive update as there is for OLS. QPWY needs \code{O(T)} fits and QPSY needs
#' \code{O(T^2)}, so QPSY takes seconds for each series at \code{n = 200} and the
#' time grows quadratically.
#'
#' The boundary is a single \strong{flat} value and not one value for each \code{r}.
#' It is the quantile of the supremum of each null path, constructed like the
#' \code{sadf_cv} of \code{\link{radf_mc_cv}}, which controls the first-crossing
#' false-alarm rate. \code{boundary} chooses how the null paths are generated.
#'
#' With \code{boundary = "asymptotic"} (the default) the function simulates the
#' limiting null distribution \code{delta * Q_{r1,r2} + sqrt(1 - delta^2) * Z_{r1,r2}}
#' in each call. Here \code{delta} is a correlation coefficient estimated from the
#' data (as in \code{\link{quantile_test}}), \code{Q} is the Dickey-Fuller t
#' functional and \code{Z} is its counterpart driven by an independent Brownian
#' motion. Both are simulated for every window, so no QR fits are needed and the
#' boundary is cheap.
#'
#' With \code{boundary = "bootstrap"} the function applies Algorithm 1 of Wu, Shi &
#' Wu (2025) to the whole path. It resamples the centred first differences of the
#' series with replacement, cumulates them into a null random walk and recomputes
#' the QPWY or QPSY path on it, \code{nrep} times. The boundary is the quantile of
#' the \code{nrep} path maxima. It follows the finite-sample distribution of the
#' statistic in the data at hand, so it removes most of the size distortion of the
#' asymptotic boundary described below. Each replicate costs a full statistic path,
#' which is \code{O(T)} QR fits for QPWY and \code{O(T^2)} for QPSY. The paper also
#' discards the first 100 draws of each resample. We omit this step because the
#' statistic has an intercept, so it does not depend on the level of the series, and
#' the draws are i.i.d., so the retained values already have the same distribution.
#' Set \code{options(exuber.parallel = TRUE)} to spread the replicates over
#' workers.
#'
#' @section Caveats:
#' The asymptotic boundary is well sized near the median in finite samples, with a
#' false-alarm rate of 3.5 to 4.0\% at a nominal 5\% (Gaussian and \eqn{t_3}
#' innovations, \code{tau = 0.5}). Away from the median the small early windows make
#' both statistics oversized, and QPSY badly so even with Gaussian innovations. The
#' false-alarm rate of QPSY is 35\% at \code{tau = 0.9} (Gaussian). With \eqn{t_3}
#' innovations it is 21\% at \code{tau = 0.8} and 44\% at \code{tau = 0.9}
#' (\code{n = 100}). With \eqn{t_3} innovations the rate for QPWY is 7.5 to 8.5\% at
#' \code{tau = 0.2} and \code{0.8} and 12.5\% at \code{tau = 0.9} (\code{n = 150}).
#' Wu, Shi & Wu advise against extreme quantiles in small samples and use bootstrap
#' critical values for monitoring, which is what \code{boundary = "bootstrap"}
#' provides. With it the false-alarm rate was between 4.5 and 6.5\% in all but one of
#' the cases above (\code{n = 100} for QPWY, \code{n = 60} for QPSY), including QPSY at
#' \code{tau = 0.8} and \code{0.9}. The exception is QPSY with \eqn{t_3} innovations at
#' \code{tau = 0.9}, which stays at 10\% when \code{nrep = 99}. A larger \code{nrep} and a
#' less extreme \code{tau} help. For \code{type = "qpsy"} with \code{tau} away from 0.5 and the
#' asymptotic boundary, the function emits a short pointer as a message (see
#' \code{\link{suppressMessages}}) and stores it as \code{attr(x, "caveat")}. The
#' numbers for both boundaries are in docs/alternative-paradigms.md.
#'
#' @note The function simulates the critical value (the boundary) internally in each
#' call, with an unexported helper, \code{quantile_boundary_sim}. There is currently
#' no reusable exported cv counterpart for this function. This gap is tracked
#' separately and is not addressed here.
#'
#' @inheritParams radf
#' @param type \code{"qpwy"} (expanding window) or \code{"qpsy"} (also the supremum
#' over window starts).
#' @param tau Quantile to test at, in \code{(0, 1)}. It is fixed, in contrast to the
#' \code{"optimal"} grid search in \code{\link{quantile_test}}, because eq. 25 of WSW
#' takes \code{tau} as a given parameter of the monitoring statistic and does not
#' reselect it at each recursion point.
#' @param nrep Number of replications for the boundary: Monte Carlo draws of the
#' limit with \code{boundary = "asymptotic"}, bootstrap resamples with
#' \code{boundary = "bootstrap"}.
#' @param boundary \code{"asymptotic"} (simulated limit, the default) or
#' \code{"bootstrap"} (Algorithm 1 of Wu, Shi & Wu 2025).
#' @param sig_lvl Significance level, one of \code{90}, \code{95}, \code{99}.
#' @param seed Optional seed for the Monte Carlo draws.
#'
#' @return An object of class \code{monitor_quantile_obj}: a list with the statistic
#' path \code{stat}, the flat \code{boundary}, the estimated \code{delta} (reported for
#' both boundaries, used only by the asymptotic one), and
#' \code{alarm} and \code{alarm_date} (the first breach, \code{NA} if there is
#' none).
#'
#' @references Wu, R., Shi, S., & Wu, J. (2025). Quantile analysis for
#' financial bubble detection and surveillance. Journal of Time Series
#' Analysis, 46(5), 908-931.
#'
#' @seealso \code{\link{quantile_test}} for the static, full-sample version of this
#' test, and \code{\link{monitor}} for the monitoring alternative based on OLS.
#'
#' @note The function returns its own class and not `radf_obj`, so it does not work
#' with `summary()`, `\link{datestamp}` and `tidy`. It has its own `print()` and
#' `autoplot()` methods instead. `print()` shows the statistic, the boundary and delta. See
#' `vignette("naming-and-analysis", package = "exuber")` for which functions fit
#' the shared pipeline and which do not.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' # Heavy-tailed (t3) innovations, explosive from t = 150 to the sample end
#' y <- sim_psy1(n = 200, te = 150, tf = 200, seed = 7,
#'   e = sim_innov(199, dist = "t", df = 3))
#' res <- monitor_quantile(y, tau = 0.5, nrep = 100, seed = 1)
#' print(res)
#' autoplot(res)
#'
#' # Monitoring an upper quantile is typically more powerful for right-tailed
#' # bubbles, but see the Caveats section on extreme quantiles
#' autoplot(monitor_quantile(y, tau = 0.8, nrep = 100, seed = 1))
#'
#' # QPSY: supremum over window starts too (O(n^2) QR fits, slower)
#' monitor_quantile(y[101:200], tau = 0.5, nrep = 100, seed = 1, type = "qpsy")
#'
#' # Bootstrap boundary (Algorithm 1 of Wu, Shi & Wu): one full statistic path per
#' # replicate, so keep nrep small in a first run
#' monitor_quantile(y, tau = 0.8, nrep = 49, seed = 1, boundary = "bootstrap")
#' }
#'
#' @family monitoring
#' @export
monitor_quantile <- function(
  data,
  tau = 0.5,
  minw = NULL,
  nrep = 500L,
  sig_lvl = 95,
  seed = NULL,
  type = c("qpwy", "qpsy"),
  boundary = c("asymptotic", "bootstrap")
) {
  type <- match.arg(type)
  boundary_type <- match.arg(boundary)
  stopifnot(tau > 0 && tau < 1)
  assert_sig_lvl(sig_lvl)
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)

  snames <- colnames(x)
  idx <- index(x)
  nc <- ncol(x)
  r_idx <- (minw + 1L):n

  stat_path <- matrix(NA_real_, length(r_idx), nc, dimnames = list(NULL, snames))
  delta <- setNames(rep(NA_real_, nc), snames)
  alarm <- setNames(rep(NA_integer_, nc), snames)

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    stat_path[, j] <- if (type == "qpwy") {
      qpwy_stat_path(y, tau, r_idx)
    } else {
      qpsy_stat_path(y, tau, r_idx, minw)
    }
    dy_full <- diff(y)
    psi <- tau - as.numeric(dy_full < quantile_narm(dy_full, probs = tau, names = FALSE))
    delta[j] <- max(min(stats::cor(dy_full, psi), 1), -1)
  }

  caveat <- NULL
  if (boundary_type == "asymptotic" && type == "qpsy" && abs(tau - 0.5) > 0.05) {
    caveat <- paste(
      "QPSY's asymptotic boundary is oversized away from the median in small samples",
      "(21% at tau = 0.8 with t3 data, n = 100, nominal 5%). Use boundary = \"bootstrap\" for a boundary that follows the finite-sample distribution; see ?monitor_quantile, Caveats section."
    )
    message_glue(caveat)
  }

  boundary <- setNames(rep(NA_real_, nc), snames)
  if (boundary_type == "asymptotic") {
    sup_U <- quantile_boundary_sim(n, minw, nrep, delta, type = type, seed = seed)
    boundary[] <- apply(sup_U, 2, quantile_narm, probs = sig_lvl / 100, names = FALSE)
  } else {
    set_rng(seed)
    for (j in seq_len(nc)) {
      sup_U <- quantile_boundary_boot(as.numeric(x[, j]), tau, minw, nrep, type = type)
      boundary[j] <- quantile_narm(sup_U, probs = sig_lvl / 100, names = FALSE)
    }
  }
  for (j in seq_len(nc)) {
    breach <- which(stat_path[, j] > boundary[j])
    if (length(breach) > 0L) alarm[j] <- r_idx[breach[1L]]
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
    boundary = boundary,
    delta = delta,
    alarm = alarm,
    alarm_date = alarm_date
  ) %>%
    add_attr(
      index = idx,
      series_names = snames,
      n = n,
      minw = minw,
      tau = tau,
      sig_lvl = sig_lvl,
      iter = nrep,
      type = type,
      boundary_type = boundary_type,
      caveat = caveat
    ) %>%
    add_class("monitor_quantile_obj")
}

#' Plot method for monitor_quantile() output
#'
#' Plots the quantile monitoring statistic against its boundary, with one panel for each series and a vertical marker at the alarm date.
#'
#' @param object An object of class \code{monitor_quantile_obj}, the output of \code{\link{monitor_quantile}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{monitor_quantile}}
#' @export
autoplot.monitor_quantile_obj <- function(object, ...) {
  minw <- attr(object, "minw")
  pos <- (minw + 1L):(minw + nrow(object$stat))
  vlines <- tibble(id = names(object$alarm), label = "alarm", at = object$alarm) %>%
    tidyr::drop_na(at)
  autoplot_stat_boundary(
    pos,
    object$stat,
    object$boundary,
    vlines = vlines,
    ylab = paste(toupper(attr(object, "type") %||% "qpwy"), "statistic")
  )
}

#' @export
print.monitor_quantile_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(
    left = glue(
      "monitor_quantile ({toupper(attr(x, 'type') %||% 'qpwy')}, n = {attr(x, 'n')}, minw = {attr(x, 'minw')}, ",
      "tau = {attr(x, 'tau')}, sig_lvl = {attr(x, 'sig_lvl')}%, {attr(x, 'boundary_type') %||% 'asymptotic'} boundary)"
    )
  )
  cat_line()
  print(
    data.frame(
      series = names(x$alarm),
      delta = round(x$delta, 3),
      boundary = round(x$boundary, 3),
      alarm = x$alarm,
      alarm_date = x$alarm_date,
      row.names = NULL
    ),
    digits = digits,
    print.gap = 2L,
    row.names = FALSE
  )
  cat_line()
  cat_caveat(x)
}
