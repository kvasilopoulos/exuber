# Harvey, Leybourne & Whitehouse (2020, Journal of Empirical Finance,
# "Date-stamping multiple bubble regimes"; "HLW"). See docs/
# dating-and-root-inference.md, "SSR/BIC dating vs. PSY recursive
# dating", for the full evaluation this implements.
#
# A two-step wrapper around dating_hls()'s already-shipped single-bubble
# fitting: Step 1 runs PSY's existing GSADF/BSADF detection+dating
# (radf()/datestamp()) to get preliminary start/end estimates for each
# detected explosive regime, and uses them to carve the sample into
# disjoint "date windows" (splitting at the midpoint between one
# regime's end and the next one's start). Step 2 applies dating_hls()-
# style fitting independently within each window -- restricted to
# Models 2 and 4 for every window but the last, since a window boundary
# is by construction a unit-root point, not a genuine sample end.
#
# HLW's own eq. for the window boundaries (their sj/ej, confirmed
# against docs/dating-and-root-inference.md's already-
# image-verified reading of this paper) uses datestamp()'s Start/End
# columns directly as observation positions (not fractions requiring
# multiplication by T -- HLW's own text: "j1_hat*T and j2_hat*T
# correspond to estimates of the first observations of the explosive
# and post-explosive regimes respectively"), and a sequential rule for
# adjusting window j+1's start once window j has actually been fit: the
# next window always begins at the first observation of the just-fitted
# unit-root/collapse regime, preventing a window from starting mid-
# bubble (a known failure mode HLW's own paper discusses explicitly).

# Given a window's own local i-index breakpoint (1-based, same
# convention as hls_model1()/hls_model23()/hls_model4()) and the
# window's starting global position `s`, returns the corresponding
# global i-index and the observation position of the new regime's first
# observation (matching dating_hls()'s own idx[b + 1L] convention).
hlw_local_to_global <- function(local_tau, s) {
  list(i_index = s + local_tau - 1L, position = s + local_tau)
}

# HLW's run-joining rule for step-1 fragmentation: if up to `max_gap`
# non-rejections separate two explosive runs that each last at least
# `min_len` (their ln(T)), treat them as one episode. `start`/`end` follow
# datestamp()'s convention (End = first non-explosive observation), so the
# gap is start[k] - end[k - 1] and a run's length is end - start.
hlw_join_runs <- function(start, end, max_gap, min_len) {
  if (length(start) < 2L || max_gap <= 0L) return(list(start = start, end = end))
  out_s <- out_e <- integer(0)
  s <- start[1L]
  e <- end[1L]
  for (k in 2:length(start)) {
    long_enough <- (e - s) >= min_len && (end[k] - start[k]) >= min_len
    if ((start[k] - e) <= max_gap && long_enough) {
      e <- end[k]
    } else {
      out_s <- c(out_s, s)
      out_e <- c(out_e, e)
      s <- start[k]
      e <- end[k]
    }
  }
  list(start = c(out_s, s), end = c(out_e, e))
}

#' Multi-Bubble SSR/BIC Dating (Harvey, Leybourne & Whitehouse 2020)
#'
#' \code{dating_hlw} extends \code{\link{dating_hls}} to series with more than one
#' explosive episode. It first runs the existing detection and dating of PSY
#' (\code{\link{radf}} and \code{\link{datestamp}}) to locate a preliminary start
#' and end for each episode. It then splits the sample into disjoint date windows
#' around them and dates each window again with the SSR/BIC fitting of
#' \code{\link{dating_hls}}, restricted to Models 2 and 4 for every window except
#' the last.
#'
#' When exactly one episode is detected, the function reduces to
#' \code{\link{dating_hls}} applied to the whole series, which is a stated property
#' of the paper. The single window then runs over \code{[1, n]} and fits all four
#' models.
#'
#' @note The step-2 SSR/BIC dating within each window needs no critical values, as
#' in \code{\link{dating_hls}}. The step-1 PSY detection and dating pass does use a
#' wild bootstrap critical value (\code{cv}, \code{nboot} and \code{seed} below),
#' but only to locate the preliminary episode windows and not for the dating
#' step.
#'
#' @inheritParams dating_hls
#' @param cv Critical values for the step-1 PSY detection and dating, as accepted by
#' \code{\link{datestamp}}. The default \code{NULL} computes \code{\link{radf_wb_cv}}
#' internally.
#' @param minw Minimum window size for the step-1 \code{\link{radf}} call. The
#' default is \code{\link{psy_minw}}.
#' @param min_duration Minimum duration (in observations) for a step-1 PSY episode
#' to be counted. The default is \code{\link{psy_ds}} (the \eqn{\ln(T)} rule of
#' HLW).
#' @param nboot,seed Passed to \code{\link{radf_wb_cv}} when \code{cv} is not
#' supplied.
#' @param join The run-joining rule of HLW for fragmented step-1 detections. Two
#' explosive runs that are separated by at most \code{join} non-rejections and are
#' each at least \eqn{\ln(T)} long are treated as one episode. The default is 3, the
#' value in the paper, and \code{0} disables joining.
#'
#' @return An object of class \code{dating_hlw_obj}: a list with one element for each
#' series. Each element is a data frame with one row for each detected episode
#' (\code{model}, \code{origination}, \code{collapse}, \code{recovery}). A series
#' with no step-1 detected episode gets a data frame with zero rows.
#'
#' @references Harvey, D. I., Leybourne, S. J., & Whitehouse, E. J.
#' (2020). Date-stamping multiple bubble regimes. Journal of Empirical
#' Finance, 58, 226-246.
#'
#' @seealso \code{\link{dating_hls}} for the single-bubble fitting that this function
#' wraps, and \code{\link{datestamp}} for the multi-bubble threshold-crossing
#' dating of PSY.
#'
#' @note The function returns its own class and not `radf_obj`, so it does not work
#' with `summary()`, `\link{datestamp}` and `tidy`. It has its own `print()` and
#' `autoplot()` methods instead. `print()` shows the dating table (model, origination, collapse, recovery). See
#' `vignette("naming-and-analysis", package = "exuber")` for which functions fit
#' the shared pipeline and which do not.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- dating_hlw(sim_data$psy1, trim = 0.1, nboot = 199L, seed = 1)
#' print(res)
#'
#' # Plot the breakpoints of every detected episode over the series
#' autoplot(res)
#'
#' # A two-bubble series, for which dating_hls() alone would fit only one bubble
#' res2 <- dating_hlw(sim_psy2(n = 200, seed = 123), trim = 0.1, nboot = 199L, seed = 1)
#' autoplot(res2)
#' }
#'
#' @family dating
#' @export
dating_hlw <- function(data, cv = NULL, minw = NULL, trim = 0.1,
                      min_duration = NULL, nboot = 199L, seed = NULL,
                      join = 3L) {
  x <- parse_data(data)
  n <- nrow(x)
  snames <- colnames(x)
  idx <- attr(x, "index")
  nc <- ncol(x)
  minw <- minw %||% psy_minw(n)
  min_duration <- min_duration %||% psy_ds(n)

  full <- radf(x, minw = minw)
  cv <- cv %||% radf_wb_cv(x, minw = minw, nboot = nboot, seed = seed)
  ds <- tryCatch(
    datestamp(full, cv, min_duration = min_duration),
    error = function(e) list(),
    warning = function(w) list()
  )

  results <- vector("list", nc)
  names(results) <- snames

  for (j in seq_len(nc)) {
    y <- as.numeric(x[, j])
    regimes <- ds[[snames[j]]]

    if (is.null(regimes) || nrow(regimes) == 0L) {
      results[[j]] <- data.frame(
        model = integer(0), origination = character(0),
        collapse = character(0), recovery = character(0)
      )
      next
    }

    tau1_psy <- match(regimes$Start, idx)
    tau2_psy <- match(regimes$End, idx)
    tau2_psy[is.na(tau2_psy)] <- n  # an episode still running at the sample end
    runs <- hlw_join_runs(tau1_psy, tau2_psy, join, log(n))
    tau1_psy <- runs$start
    tau2_psy <- runs$end
    nhat <- length(tau1_psy)

    e <- integer(nhat)
    for (jj in seq_len(nhat)) {
      e[jj] <- if (jj < nhat) {
        tau2_psy[jj] + (tau1_psy[jj + 1L] - tau2_psy[jj]) %/% 2L
      } else {
        n
      }
    }

    model <- rep(NA_integer_, nhat)
    origination <- collapse <- recovery <- rep(NA_character_, nhat)
    s <- 1L

    for (jj in seq_len(nhat)) {
      if (s >= e[jj]) {
        next
      }
      y_win <- y[s:e[jj]]
      models_allowed <- if (jj < nhat) c(2L, 4L) else 1:4
      res <- hls_fit_series(y_win, trim, models = models_allowed)
      model[jj] <- res$model

      glb <- lapply(res$breaks, hlw_local_to_global, s = s)
      origination[jj] <- as.character(idx[glb$tau1$position])
      if (!is.null(glb$tau2)) collapse[jj] <- as.character(idx[glb$tau2$position])
      if (!is.null(glb$tau3)) recovery[jj] <- as.character(idx[glb$tau3$position])

      if (jj < nhat) {
        s <- if (res$model == 2L) {
          s + res$breaks[["tau2"]]
        } else {
          s + res$breaks[["tau3"]]
        }
      }
    }

    results[[j]] <- data.frame(
      model = model, origination = origination,
      collapse = collapse, recovery = recovery
    )
  }

  results %>%
    add_attr(index = idx, series_names = snames, n = n, trim = trim, minw = minw, mat = x) %>%
    add_class("dating_hlw_obj")
}

#' Plot method for dating_hlw() output
#'
#' Plots each series with vertical markers at every estimated origination, collapse and recovery date.
#'
#' @param object An object of class \code{dating_hlw_obj}, the output of \code{\link{dating_hlw}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{dating_hlw}}
#' @importFrom tidyr drop_na
#' @export
autoplot.dating_hlw_obj <- function(object, ...) {
  idx <- index(object)
  is_date <- lubridate::is.Date(idx)
  breaks <- object %>%
    purrr::imap_dfr(~ mutate(.x, id = .y)) %>%
    pivot_longer(c(origination, collapse, recovery), names_to = "label", values_to = "at") %>%
    mutate(at = if (is_date) as.Date(at) else as.numeric(at)) %>%
    drop_na(at) %>%
    select(id, label, at)
  autoplot_series_breaks(mat(object), idx, breaks)
}

#' @export
print.dating_hlw_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("dating_hlw (n = {attr(x, 'n')}, trim = {attr(x, 'trim')})"))
  for (nm in names(x)) {
    cat_line()
    cat_line(nm, ":")
    print(x[[nm]], digits = digits, row.names = FALSE)
  }
  cat_line()
}
