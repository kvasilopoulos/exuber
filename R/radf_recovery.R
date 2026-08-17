# Phillips & Shi (2014, Econometric Theory), "Financial Bubble Implosion
# and Reverse Regression" ("PS14"). See docs/enhancements/
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
      .combine = "cbind",
      .options.future = list(seed = TRUE, globals = structure(TRUE, add = c("rls_gsadf", "unroot"))),
      .inorder = FALSE
    ) %dofuture% {
      p()
      y <- rev(cumsum(rnorm(n)))
      yxmat <- unroot(y, lag = lag)
      rls_gsadf(yxmat, min_win = minw, lag = lag)
    }
  })

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
#' Computes critical values for the reverse-regression BSADF statistic
#' used by \code{\link{radf_recovery}}, calibrated to its own (Phillips &
#' Shi 2014's Theorem 1) null limiting distribution rather than the
#' standard forward \code{\link{radf_mc_cv}} boundary -- reversing the
#' simulated null path before running the recursive computation, since
#' reversal induces an endogeneity with no forward-regression analogue
#' (see \code{\link{radf_recovery}}'s Details).
#'
#' @inheritParams radf_mc_cv
#'
#' @return A list of class \code{radf_cv} with a single element,
#' \code{bsadf_cv}: a matrix of critical values (columns \code{90\%},
#' \code{95\%}, \code{99\%}), one row per reverse-time position, aligned
#' the same way \code{\link{radf_mc_cv}}'s own \code{bsadf_cv} aligns to
#' \code{radf()$bsadf}.
#'
#' @note \code{print()}/\code{tidy()} are not yet implemented for this
#' object's own class (\code{recovery_cv} has no \code{tidy_radf_cv}
#' method, unlike \code{radf_sign_cv()}/\code{radf_tt_cv()} which fall
#' back to \code{mc_cv}'s -- that fallback does not apply here since this
#' object only carries \code{bsadf_cv}, not the \code{adf_cv}/
#' \code{sadf_cv}/\code{gsadf_cv} fields that method expects). Inspect
#' \code{cv$bsadf_cv} directly instead of calling \code{print(cv)}/
#' \code{tidy(cv)}, which currently error.
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
#' \code{radf_recovery} implements Phillips & Shi (2014)'s reverse-
#' regression dating: reverses the series, runs \code{radf()}'s existing
#' bsadf recursion on it, and locates the first up-crossing of a
#' reversal-calibrated critical value boundary (the market recovery
#' date) followed by the next down-crossing (the crisis/collapse
#' origination date in the original series), then maps both back to the
#' original time index.
#'
#' @details
#' Two dates are returned per series: \code{f_c}, the crisis origination
#' (collapse-onset) date -- a reverse-regression-derived alternative to
#' the collapse date \code{\link{datestamp}} already dates from the
#' forward test -- and \code{f_r}, the market recovery date, always
#' \code{f_c <= f_r} by construction (the down-crossing is searched only
#' after the up-crossing). If no up-crossing is found, neither date is
#' identified (\code{NA}, \code{detected = FALSE}). If an up-crossing is
#' found but no subsequent down-crossing occurs before the reverse-time
#' sample is exhausted, \code{f_c} is \code{NA} and \code{censored = TRUE}
#' (the crisis origination predates the observed sample).
#'
#' @section Caveats:
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Validation status (2026-08-10), reported honestly rather than
#' silently}: \code{f_r} (recovery date) validates well against synthetic
#' collapse-then-recovery data -- bias in the same range the paper's own
#' Monte Carlo reports (a few observations early). \code{f_c} (crisis
#' origination date) shows a materially larger residual bias in Monte
#' Carlo checks, and the empirical false-detection rate under a pure
#' random-walk null (n=100, minw=20, 95\% level) is around 29\%, higher
#' than comparable forward-test numbers elsewhere in this package. One
#' real synthetic-DGP artifact (a level-jump at a regime boundary
#' producing a spurious spike) was found and fixed during validation, but
#' the residual \code{f_c} bias/false-detection elevation was not fully
#' resolved -- plausibly genuine finite-sample noise in the paper's own
#' literal first-down-crossing rule (eq. 9's \code{inf} has no
#' persistence requirement, so a transient dip below the boundary is
#' enough to trigger a premature \code{f_c}), but this has not been ruled
#' out against a subtler implementation issue. Treat \code{f_c} and the
#' overall detection rate as exploratory pending further validation; see
#' docs/enhancements/dating-and-root-inference.md for the full numbers.
#' The same short pointer is emitted as a message when this function is
#' called (see \code{\link{suppressMessages}} to silence it) and stored as
#' \code{attr(x, "caveat")} on the returned object.
#'
#' @inheritParams radf
#' @param nrep Number of Monte Carlo replications for
#' \code{\link{radf_recovery_cv}}'s critical value.
#' @param sig_lvl Significance level, one of \code{90}, \code{95}, \code{99}.
#' @param seed Optional seed for the Monte Carlo draws.
#'
#' @return An object of class \code{radf_recovery_obj}: a list with
#' \code{f_c}/\code{f_r} (the estimated dates, \code{NA} if not
#' identified), \code{detected} (logical, whether an up-crossing was
#' found at all), and \code{censored} (logical, whether \code{f_c} is
#' left-censored by the start of the reverse-time sample).
#'
#' @references Phillips, P. C. B., & Shi, S. (2014). Financial Bubble
#' Implosion and Reverse Regression. Cowles Foundation Discussion Paper
#' No. 1967, Yale University. Published in Econometric Theory.
#'
#' @seealso \code{\link{datestamp}} for the (forward, non-reversed)
#' origination/collapse dating this complements.
#'
#' @note Returns its own class (not `radf_obj`), so it does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`/`autoplot` -- prints its own
#' origination/recovery date summary -- see
#' `vignette("naming-and-analysis", package = "exuber")` for the full
#' picture of which functions do and don't fit that pipeline.
#'
#' @examples
#' \donttest{
#' res <- radf_recovery(sim_data, nrep = 200)
#' print(res)
#' }
#'
#' @export
radf_recovery <- function(data, minw = NULL, lag = 0, nrep = 1000L,
                           sig_lvl = 95, seed = NULL) {
  caveat <- "Experimental. f_c and the overall false-detection rate are exploratory pending further validation; see ?radf_recovery, Caveats section."
  message_glue(caveat)

  stopifnot(sig_lvl %in% c(90, 95, 99))
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
    if (is.na(g_e)) next
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

  f_c_date <- vapply(f_c, function(i) if (is.na(i)) NA_character_ else as.character(idx[i]), character(1))
  f_r_date <- vapply(f_r, function(i) if (is.na(i)) NA_character_ else as.character(idx[i]), character(1))

  list(
    f_c = f_c, f_r = f_r, f_c_date = f_c_date, f_r_date = f_r_date,
    detected = detected, censored = censored
  ) %>%
    add_attr(
      index = idx, series_names = snames, minw = minw, lag = lag,
      n = n, sig_lvl = sig_lvl, iter = nrep, caveat = caveat
    ) %>%
    add_class("radf_recovery_obj")
}

#' @export
print.radf_recovery_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "radf_recovery (n = {attr(x, 'n')}, minw = {get_minw(x)}, ",
    "level = {attr(x, 'sig_lvl')}%)"
  ))
  cat_line()
  cat_caveat(x)
  print(
    data.frame(
      series = names(x$f_c), f_c = x$f_c_date, f_r = x$f_r_date,
      detected = x$detected, censored = x$censored, row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
