# Harvey, Leybourne & Whitehouse (2020, Journal of Empirical Finance,
# "Date-stamping multiple bubble regimes"; "HLW"). See docs/enhancements/
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
# against docs/enhancements/dating-and-root-inference.md's already-
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

#' Multi-Bubble SSR/BIC Dating (Harvey, Leybourne & Whitehouse 2020)
#'
#' \code{dating_hlw} extends \code{\link{dating_hls}} to series with more
#' than one explosive episode: it first runs PSY's existing detection
#' and dating (\code{\link{radf}}/\code{\link{datestamp}}) to locate a
#' preliminary start/end for each episode, splits the sample into
#' disjoint date windows around them, then re-dates each window with
#' \code{\link{dating_hls}}-style SSR/BIC fitting (restricted to Models 2
#' and 4 for every window but the last).
#'
#' When exactly one episode is detected, this reduces to
#' \code{\link{dating_hls}} applied to the whole series -- the paper's own
#' stated property, since the single window then runs \code{[1, n]} and
#' fits all four models.
#'
#' @note The step-2 SSR/BIC dating within each window needs no critical
#' values at all, same as \code{\link{dating_hls}}. The step-1 PSY
#' detection/dating pass does use a wild bootstrap critical value
#' (\code{cv}/\code{nboot}/\code{seed} below), but only to locate the
#' preliminary episode windows, not for the dating step itself.
#'
#' @inheritParams dating_hls
#' @param cv Critical values for the step-1 PSY detection/dating step,
#' as accepted by \code{\link{datestamp}}. Default \code{NULL} computes
#' \code{\link{radf_wb_cv}} internally.
#' @param minw Minimum window size for the step-1 \code{\link{radf}}
#' call. Default \code{\link{psy_minw}}.
#' @param min_duration Minimum duration (in observations) for a step-1
#' PSY episode to be counted. Default \code{\link{psy_ds}} (HLW's own
#' \eqn{\ln(T)} rule).
#' @param nboot,seed Passed to \code{\link{radf_wb_cv}} when \code{cv}
#' is not supplied.
#'
#' @return An object of class \code{dating_hlw_obj}: a list, one element
#' per series, each a data frame with one row per detected episode
#' (\code{model}, \code{origination}, \code{collapse}, \code{recovery}).
#' A series with no step-1 detected episode gets a zero-row data frame.
#'
#' @references Harvey, D. I., Leybourne, S. J., & Whitehouse, E. J.
#' (2020). Date-stamping multiple bubble regimes. Journal of Empirical
#' Finance, 58, 226-246.
#'
#' @seealso \code{\link{dating_hls}} for the single-bubble fitting this
#' wraps, and \code{\link{datestamp}} for PSY's own multi-bubble
#' threshold-crossing dating.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- dating_hlw(sim_data$sim_psy1, trim = 0.1, nboot = 199L, seed = 1)
#' print(res)
#' }
#'
#' @export
dating_hlw <- function(data, cv = NULL, minw = NULL, trim = 0.1,
                      min_duration = NULL, nboot = 199L, seed = NULL) {
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
    add_attr(index = idx, series_names = snames, n = n, trim = trim, minw = minw) %>%
    add_class("dating_hlw_obj")
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
