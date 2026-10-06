# Phillips & Shi (2014, Econometric Theory), "Financial Bubble Implosion
# and Reverse Regression" ("PS14"). See docs/
# dating-and-root-inference.md, "Reverse-regression recovery dating", for
# the full evaluation this implements.
#
# Mechanism (their eq. 7-9): reverse the series (X*_t := X_{T+1-t}), run
# the same double-recursive sup-ADF (BSADF) scan radf() already computes
# on X*, and map the crossing-time fractions back to the original time
# index. A mildly-explosive-then-collapsing process, reversed, turns its
# collapse regime into an explosive regime in reverse time and vice versa
# -- so "detect the crisis origination and market recovery dates" becomes
# "detect explosiveness in the reversed series," reusing radf()'s
# existing bsadf recursion with no new point statistic. Their g_hat_e
# (first reverse-time up-crossing) maps to the market RECOVERY date
# (f_hat_r = 1 - g_hat_e); their g_hat_c (the following down-crossing,
# searched only after g_hat_e) maps to the ORIGINAL series' crisis
# ORIGINATION/collapse-onset date (f_hat_c = 1 - g_hat_c) -- a
# reverse-regression-derived alternative to the collapse date PSY's
# forward test already dates, not a second/later event.
#
# Their Theorem 1 derives a DIFFERENT null limiting distribution for the
# reverse statistic than the forward one: reversing a cumulative sum
# makes the reverse regression's lagged level at reverse-time s-1
# (X*_{s-1} = X_{T+2-s}) already contain the shock that appears, negated,
# as its own "current innovation" X*_s - X*_{s-1} = -eps_{T+2-s} -- an
# endogeneity with no forward-regression analogue. Verified empirically
# (paired Monte Carlo, same underlying draws, n=100/minw=20/5000 reps):
# reversing simulated H0 paths before running radf()'s existing recursive
# computation gives measurably different bsadf-type critical values than
# radf_mc_cv() on the same unreversed paths (differences up to ~0.10 at
# the quantiles/positions checked) -- consistent with, not just assumed
# from, the paper's own theorem. radf_recovery_cv() computes this
# reversal-calibrated critical value directly via Monte Carlo, mirroring
# radf_mc_cv()'s own simulate-then-quantile construction (including its
# cummax(badf)-as-bsadf-boundary shortcut) with one added rev() before
# the recursive computation, rather than reusing forward critical values
# as an approximation of unknown quality.

#' @importFrom doFuture `%dofuture%`
radf_recovery_ <- function(n, minw, nrep, seed = NULL, lag = 0) {
  assert_n(n)
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  do_par <- getOption("exuber.parallel")

  set_rng(seed)
  results <- with_backend({
    p <- progressor(steps = nrep)
    foreach(
      i = 1:nrep,
      .options.future = list(
        seed = TRUE,
        globals = structure(TRUE, add = c("rls_gsadf", "unroot"))
      ),
      .inorder = FALSE
    ) %dofuture%
      {
        p()
        y <- rev(cumsum(rnorm(n)))
        yxmat <- unroot(y, lag = lag)
        rls_gsadf(yxmat, min_win = minw, lag = lag)
      }
  })
  results <- do.call(cbind, results)

  n_minw <- n - minw - lag
  badf_crit <- results[1:n_minw, ]

  list(badf = badf_crit) %>%
    add_attr(
      index = 1:n,
      method = "Monte Carlo (reverse)",
      n = n,
      minw = minw,
      iter = nrep,
      lag = lag,
      seed = get_rng_state(seed),
      parallel = do_par
    )
}

#' Monte Carlo Critical Values for Reverse-Regression Recovery Dating
#'
#' Computes critical values for the reverse-regression BSADF statistic that
#' \code{\link{radf_recovery}} uses. They are calibrated to the null limiting
#' distribution of that statistic (Theorem 1 of Phillips & Shi 2014) and not to the
#' standard forward boundary of \code{\link{radf_mc_cv}}. The simulated null path is
#' reversed before the recursive computation, because reversal induces an
#' endogeneity that has no analogue in the forward regression (see the Details of
#' \code{\link{radf_recovery}}).
#'
#' @inheritParams radf_mc_cv
#'
#' @return A list of class \code{radf_cv} with a single element, \code{bsadf_cv}. It
#' is a matrix of critical values (columns \code{90\%}, \code{95\%}, \code{99\%})
#' with one row for each reverse-time position, aligned in the same way as the
#' \code{bsadf_cv} of \code{\link{radf_mc_cv}} is aligned to \code{radf()$bsadf}.
#'
#' @note \code{print()} and \code{tidy()} are not yet implemented for the class of
#' this object. \code{recovery_cv} has no \code{tidy_radf_cv} method. The objects
#' from \code{radf_sign_cv()} and \code{radf_tt_cv()} fall back to the method of
#' \code{mc_cv}, but that fallback does not apply here, because this object carries
#' only \code{bsadf_cv} and not the \code{adf_cv}, \code{sadf_cv} and
#' \code{gsadf_cv} fields that the method expects. Inspect \code{cv$bsadf_cv}
#' directly. Calling \code{print(cv)} or \code{tidy(cv)} currently gives an error.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @seealso \code{\link{radf_recovery}}, \code{\link{radf_mc_cv}}
#'
#' @importFrom stats quantile rnorm
#'
#' @examples
#' \donttest{
#' cv <- radf_recovery_cv(n = 100, minw = 20, nrep = 200)
#' range(cv$bsadf_cv)
#' }
#'
#' @family critical values
#' @export
radf_recovery_cv <- function(n, minw = NULL, nrep = 1000L, seed = NULL, lag = 0) {
  pcnt <- c(0.9, 0.95, 0.99)

  results <- radf_recovery_(n, minw = minw, nrep = nrep, seed = seed, lag = lag)

  bsadf_crit <- apply(results$badf, 2, cummax) %>%
    apply(1, quantile, probs = pcnt) %>%
    t()
  colnames(bsadf_crit) <- paste0(pcnt * 100, "%")

  list(bsadf_cv = bsadf_crit) %>%
    inherit_attrs(results) %>%
    add_class("radf_cv", "recovery_cv")
}

#' Reverse-Regression Dating of Crisis Origination and Market Recovery
#'
#' \code{radf_recovery} implements the reverse-regression dating of Phillips & Shi
#' (2014). It reverses the series and runs the existing bsadf recursion of
#' \code{radf()} on it. It then locates the first up-crossing of a critical-value
#' boundary calibrated for the reversal, which is the market recovery date, and the
#' next down-crossing, which is the crisis (collapse) origination date in the
#' original series. Both are mapped back to the original time index.
#'
#' @details
#' The function returns two dates for each series. \code{f_c} is the crisis
#' origination (collapse-onset) date, a reverse-regression alternative to the
#' collapse date that \code{\link{datestamp}} already dates from the forward test.
#' \code{f_r} is the market recovery date, and \code{f_c <= f_r} always holds by
#' construction, because the down-crossing is searched only after the
#' up-crossing. If no up-crossing is found, neither date is identified (\code{NA},
#' \code{detected = FALSE}). If an up-crossing is found but no later down-crossing
#' occurs before the reverse-time sample ends, \code{f_c} is \code{NA} and
#' \code{censored = TRUE}, which means that the crisis origination predates the
#' observed sample.
#'
#' @section Caveats:
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Validation status (2026-08-10).} \code{f_r} (the recovery date)
#' validates well against synthetic collapse-then-recovery data. Its bias is in the
#' same range as in the Monte Carlo study of the paper, a few observations early.
#' \code{f_c} (the crisis origination date) shows a materially larger residual bias
#' in Monte Carlo checks. The empirical false-detection rate under a pure
#' random-walk null (n = 100, minw = 20, 95\% level) is around 29\%, which is higher
#' than the comparable numbers for the forward tests elsewhere in this package. We
#' found and fixed one artifact of the synthetic process during validation, where a
#' level jump at a regime boundary produced a spurious spike. The remaining bias in
#' \code{f_c} and the elevated false-detection rate are not fully explained. They
#' may be genuine finite-sample noise from the literal first-down-crossing rule of
#' the paper. The \code{inf} in its eq. 9 has no persistence requirement, so a
#' transient dip below the boundary is enough to trigger a premature \code{f_c}. We
#' have not ruled out a subtler implementation issue. Treat \code{f_c} and the
#' overall detection rate as exploratory until they are validated further, and see
#' docs/dating-and-root-inference.md for the full numbers. The function emits the
#' same short pointer as a message when it is called (use
#' \code{\link{suppressMessages}} to silence it) and stores it as
#' \code{attr(x, "caveat")} on the returned object.
#'
#' @inheritParams radf
#' @param nrep Number of Monte Carlo replications for the critical value in
#' \code{\link{radf_recovery_cv}}.
#' @param sig_lvl Significance level, one of \code{90}, \code{95}, \code{99}.
#' @param seed Optional seed for the Monte Carlo draws.
#'
#' @return An object of class \code{radf_recovery_obj}: a list with \code{f_c} and
#' \code{f_r} (the estimated dates, \code{NA} if not identified), \code{detected}
#' (logical, whether an up-crossing was found at all) and \code{censored} (logical,
#' whether \code{f_c} is left-censored by the start of the reverse-time sample).
#'
#' @references Phillips, P. C. B., & Shi, S. (2014). Financial Bubble
#' Implosion and Reverse Regression. Cowles Foundation Discussion Paper
#' No. 1967, Yale University. Published in Econometric Theory.
#'
#' @seealso \code{\link{datestamp}} for the forward, non-reversed dating of
#' origination and collapse that this function complements.
#'
#' @note The function returns its own class and not `radf_obj`, so it does not work
#' with `summary()`, `\link{datestamp}` and `tidy`. It has its own `print()` and
#' `autoplot()` methods instead.
#' `print()` shows the origination and recovery dates. See
#' `vignette("naming-and-analysis", package = "exuber")` for which functions fit
#' the shared pipeline and which do not.
#'
#' @examples
#' \donttest{
#' # The expansion, bubble, collapse and recovery process of sim_ps1()
#' y <- sim_ps1(n = 100, seed = 1)
#' res <- radf_recovery(y, minw = 15, nrep = 200, seed = 1)
#' print(res)
#'
#' # Plot the series with the estimated collapse (f_c) and recovery (f_r) points
#' autoplot(res)
#' }
#'
#' @family dating
#' @export
radf_recovery <- function(data, minw = NULL, lag = 0, nrep = 1000L, sig_lvl = 95, seed = NULL) {
  caveat <- "Experimental. f_c and the overall false-detection rate are exploratory pending further validation; see ?radf_recovery, Caveats section."
  message_glue(caveat)

  assert_sig_lvl(sig_lvl)
  x <- parse_data(data)
  n <- nrow(x)
  minw <- minw %||% psy_minw(n)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  snames <- colnames(x)
  idx <- attr(x, "index")
  nc <- ncol(x)
  lvl_lab <- paste0(sig_lvl, "%")
  zadj <- minw + lag

  x_rev <- unclass(x)[rev(seq_len(n)), , drop = FALSE]
  rev_fit <- radf(x_rev, minw = minw, lag = lag)
  cv <- radf_recovery_cv(n = n, minw = minw, nrep = nrep, seed = seed, lag = lag)

  f_c <- f_r <- setNames(rep(NA_integer_, nc), snames)
  detected <- censored <- setNames(rep(FALSE, nc), snames)

  for (j in seq_len(nc)) {
    exceed <- rev_fit$bsadf[, j] > cv$bsadf_cv[, lvl_lab]
    g_e <- which(exceed)[1L]
    if (is.na(g_e)) {
      next
    }
    detected[j] <- TRUE

    after <- which(!exceed[g_e:length(exceed)])
    if (length(after) == 0L) {
      g_c <- length(exceed) + 1L
      censored[j] <- TRUE
    } else {
      g_c <- g_e + after[1L] - 1L
    }

    rev_pos_e <- g_e + zadj
    rev_pos_c <- min(g_c + zadj, n)

    f_r[j] <- n + 1L - rev_pos_e
    if (!censored[j]) f_c[j] <- n + 1L - rev_pos_c
  }

  f_c_date <- vapply(
    f_c,
    function(i) if (is.na(i)) NA_character_ else as.character(idx[i]),
    character(1)
  )
  f_r_date <- vapply(
    f_r,
    function(i) if (is.na(i)) NA_character_ else as.character(idx[i]),
    character(1)
  )

  list(
    f_c = f_c,
    f_r = f_r,
    f_c_date = f_c_date,
    f_r_date = f_r_date,
    detected = detected,
    censored = censored
  ) %>%
    add_attr(
      index = idx,
      series_names = snames,
      minw = minw,
      lag = lag,
      n = n,
      sig_lvl = sig_lvl,
      iter = nrep,
      caveat = caveat,
      mat = x
    ) %>%
    add_class("radf_recovery_obj")
}

#' Plot method for radf_recovery() output
#'
#' Plots each series with vertical markers at the estimated collapse and recovery dates.
#'
#' @param object An object of class \code{radf_recovery_obj}, the output of \code{\link{radf_recovery}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{radf_recovery}}
#' @export
autoplot.radf_recovery_obj <- function(object, ...) {
  idx <- index(object)
  breaks <- breaks_tbl(idx, collapse = object$f_c_date, recovery = object$f_r_date)
  autoplot_series_breaks(mat(object), idx, breaks)
}

#' @export
print.radf_recovery_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(
    left = glue(
      "radf_recovery (n = {attr(x, 'n')}, minw = {get_minw(x)}, ",
      "level = {attr(x, 'sig_lvl')}%)"
    )
  )
  cat_line()
  cat_caveat(x)
  print(
    data.frame(
      series = names(x$f_c),
      f_c = x$f_c_date,
      f_r = x$f_r_date,
      detected = x$detected,
      censored = x$censored,
      row.names = NULL
    ),
    digits = digits,
    print.gap = 2L,
    row.names = FALSE
  )
  cat_line()
}
