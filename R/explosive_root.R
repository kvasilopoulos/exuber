# Inference on the explosive autoregressive root, for use *after* a bubble
# has been detected and dated (e.g. via datestamp()). PSY-style tests only
# answer "is there a bubble"; this answers "how fast is it growing".
#
# Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate explosiveness.
# The Econometrics Journal. Building on Phillips, P. C. B., & Magdalinos, T.
# (2007). Limit theory for moderate deviations from a unit root. Journal of
# Econometrics, 136(1), 115-130.

#' Estimate the Explosive Autoregressive Root over a Sub-Sample
#'
#' Fits the no-intercept AR(1) regression \eqn{y_t = \rho y_{t-1} + \epsilon_t}
#' over the sub-sample \code{from:to} of \code{data} -- the model used by
#' Phillips & Magdalinos (2007) and Guo, Sun & Wang (2019) for inference on a
#' (moderately) explosive root, e.g. an episode already identified by
#' \code{\link{datestamp}}. No intercept is included, following Phillips &
#' Magdalinos's model (their eq. 58 excludes it "to exclude the presence of
#' a deterministically explosive component").
#'
#' @param data A numeric vector (a single series).
#' @param from,to Integer row positions delimiting the sub-sample (e.g. from
#' \code{datestamp()}'s \code{Start}/\code{End}, converted to row positions
#' if they are dates: \code{match(start_date, index(x))}).
#'
#' @return A list with \code{rho} (the OLS estimate), \code{se} (its
#' standard error), \code{t_stat}, and \code{n} (sub-sample size).
#'
#' @seealso \code{\link{root_ci}} for a confidence interval and doubling
#' time based on this estimate.
#'
#' @references Phillips, P. C. B., & Magdalinos, T. (2007). Limit theory for
#' moderate deviations from a unit root. Journal of Econometrics, 136(1),
#' 115-130.
#' @references Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate
#' explosiveness. The Econometrics Journal, 22(3), 279-303.
#'
#' @note Returns its own class (not `radf_obj`), so it does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`/`autoplot` -- prints its own
#' confidence-interval summary (this is inference on the root's
#' magnitude, not a `radf_obj`-shaped test result) -- see
#' `vignette("naming-and-analysis", package = "exuber")` for the full
#' picture of which functions do and don't fit that pipeline.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' set.seed(2026)
#' burn <- cumsum(rnorm(60))
#' bubble <- burn[length(burn)] * 1.04^(1:40) + cumsum(rnorm(40, sd = 0.5))
#' y <- c(burn, bubble)
#'
#' r <- radf(y, minw = 20)
#' cv <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
#' ds <- datestamp(r, cv = cv, min_duration = 3)
#'
#' est <- explosive_root(y, ds[["series1"]]$Start[1], ds[["series1"]]$End[1])
#' est
#'
#' @export
explosive_root <- function(data, from, to) {
  y <- as.numeric(data)[from:to]
  y_lag <- y[-length(y)]
  dy <- diff(y)

  sxx <- sum(y_lag^2)
  sxy <- sum(y_lag * dy)
  beta <- sxy / sxx
  res <- dy - beta * y_lag
  n <- length(dy)
  sigma2 <- sum(res^2) / (n - 1)
  se_beta <- sqrt(sigma2 / sxx)

  list(rho = 1 + beta, se = se_beta, t_stat = beta / se_beta, n = n)
}

#' Confidence Interval and Doubling Time for an Explosive Root
#'
#' Guo, Sun & Wang (2019) show that -- unlike the classical (stationary or
#' unit-root) case -- the ordinary t-statistic for the autoregressive root
#' \eqn{\hat\rho} of a (moderately) explosive AR(1), estimated by OLS with no
#' intercept, is asymptotically \strong{standard normal} under i.i.d. errors
#' (and under weakly dependent errors, with a HAC standard error). This means
#' an ordinary-looking Wald interval, \eqn{\hat\rho \pm z_{\alpha/2}\cdot se(\hat\rho)},
#' is asymptotically valid here even though \eqn{\hat\rho > 1} -- despite
#' looking identical in form to a classical (invalid, for an explosive root)
#' normal-theory interval, the justification is different (Guo, Sun & Wang's
#' explosive-root CLT, not the classical stationary one).
#'
#' \code{type = "cauchy"} instead uses the Phillips & Magdalinos (2007)
#' fixed-root result (their eq. 27, restating White 1958): for a genuinely
#' explosive, non-drifting root, \eqn{\frac{\rho^n}{\rho^2-1}(\hat\rho-\rho)}
#' converges to a standard Cauchy variate. Plugging in \eqn{\hat\rho} for the
#' unknown \eqn{\rho} in the normalization (the usual practice for this kind
#' of self-normalized pivot) gives \eqn{\hat\rho \pm q_{\alpha/2}\cdot
#' (\hat\rho^2-1)/\hat\rho^n}, with \eqn{q_{\alpha/2}} a standard-Cauchy
#' quantile. This interval assumes a \emph{fixed} explosive root (no drift,
#' no unknown localizing rate); the default \code{"normal"} type is the
#' safer choice when that assumption is in doubt, since Guo, Sun & Wang's
#' result allows drift and weak dependence.
#'
#' \code{root_ci} also reports the implied \emph{doubling time}
#' \eqn{\log(2)/\log(\hat\rho)}: the number of periods for the bubble to
#' double in magnitude at the estimated growth rate, with its own interval
#' obtained by transforming the endpoints of the \eqn{\hat\rho} interval
#' (doubling time is strictly decreasing in \eqn{\rho}, so the CI's lower and
#' upper doubling-time bounds come from the upper and lower \eqn{\rho} bounds,
#' respectively).
#'
#' @param x A list as returned by \code{\link{explosive_root}}.
#' @param level Confidence level (default 0.95).
#' @param type \code{"normal"} (default) for Guo, Sun & Wang's normal-t
#' interval, or \code{"cauchy"} for the Phillips-Magdalinos fixed-root
#' Cauchy interval.
#'
#' @return A list with \code{rho}, \code{rho_ci} (length-2 vector), and
#' \code{doubling_time}, \code{doubling_time_ci}.
#'
#' @references Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate
#' explosiveness. The Econometrics Journal, 22(3), 279-303.
#' @references Phillips, P. C. B., & Magdalinos, T. (2007). Limit theory for
#' moderate deviations from a unit root. Journal of Econometrics, 136(1),
#' 115-130.
#'
#' @note Returns its own class (not `radf_obj`), so it does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`/`autoplot` -- prints its own
#' confidence-interval summary (this is inference on the root's
#' magnitude, not a `radf_obj`-shaped test result) -- see
#' `vignette("naming-and-analysis", package = "exuber")` for the full
#' picture of which functions do and don't fit that pipeline.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' set.seed(2026)
#' burn <- cumsum(rnorm(60))
#' bubble <- burn[length(burn)] * 1.04^(1:40) + cumsum(rnorm(40, sd = 0.5))
#' y <- c(burn, bubble)
#'
#' r <- radf(y, minw = 20)
#' cv <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
#' ds <- datestamp(r, cv = cv, min_duration = 3)
#'
#' est <- explosive_root(y, ds[["series1"]]$Start[1], ds[["series1"]]$End[1])
#' root_ci(est) # true rho = 1.04 -- CI should bracket it
#' root_ci(est, type = "cauchy")
#'
#' @export
root_ci <- function(x, level = 0.95, type = c("normal", "cauchy")) {
  type <- match.arg(type)
  alpha <- 1 - level

  rho_ci <- if (type == "normal") {
    z <- qnorm(1 - alpha / 2)
    x$rho + c(-1, 1) * z * x$se
  } else {
    q <- qcauchy(1 - alpha / 2)
    half_width <- q * (x$rho^2 - 1) / x$rho^x$n
    x$rho + c(-1, 1) * half_width
  }

  dt <- function(rho) log(2) / log(rho)

  list(
    rho = x$rho,
    rho_ci = rho_ci,
    doubling_time = dt(x$rho),
    doubling_time_ci = c(dt(rho_ci[2]), dt(rho_ci[1]))
  )
}

#' Root Confidence Intervals for Every Datestamped Episode
#'
#' Convenience wrapper that runs \code{\link{explosive_root}}/\code{\link{root_ci}}
#' on every episode in a \code{\link{datestamp}} result, so root inference
#' doesn't have to be hand-run per episode.
#'
#' \strong{Not} folded into \code{\link{summary.radf_obj}}: that function's
#' existing S3 dispatch (\code{summary_radf.mc_cv}/\code{.wb_cv}/\code{.sb_cv})
#' is built entirely around \code{radf_cv} test-statistic critical values --
#' root CIs are a different kind of output (needing a \code{datestamp()}
#' result, not a \code{radf_cv}) with no natural fit in that dispatch chain.
#' A standalone function keeps this addition to the size the enhancement
#' notes actually scoped ("a small follow-up"), rather than restructuring
#' \code{summary()}'s shared machinery to accommodate a fundamentally
#' different kind of result.
#'
#' Root inference on a very short episode is statistically meaningless (the
#' same way it would be calling \code{\link{explosive_root}} directly on 2-3
#' points) -- this function doesn't filter episodes itself, since
#' \code{\link{datestamp}} already has a \code{min_duration} argument for
#' exactly this; set it there before piping into this function, rather than
#' expecting this function to second-guess what counts as "too short".
#'
#' @param object A \code{radf_obj} (or subclass) that \code{ds} was computed
#' on -- needs the original data, retrieved via its \code{"mat"} attribute.
#' @param ds A \code{\link{datestamp}} result computed on \code{object}. Set
#' \code{min_duration} there to exclude episodes too short for reliable root
#' inference.
#' @param level Confidence level, passed to \code{\link{root_ci}} (default 0.95).
#' @param type CI type, passed to \code{\link{root_ci}} (default \code{"normal"}).
#'
#' @return A named list (one element per series in \code{ds}; the panel
#' sieve-bootstrap case, whose \code{ds} entry is named \code{"panel"} and
#' has no single corresponding series, is dropped with a warning), each a
#' data frame with one row per datestamped episode: \code{Start}, \code{End},
#' \code{rho}, \code{rho_lower}, \code{rho_upper}, \code{doubling_time},
#' \code{doubling_time_lower}, \code{doubling_time_upper}.
#'
#' @seealso \code{\link{explosive_root}}, \code{\link{root_ci}}, \code{\link{datestamp}}
#'
#' @note Returns its own class (not `radf_obj`), so it does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`/`autoplot` -- prints its own
#' per-episode confidence-interval table -- see
#' `vignette("naming-and-analysis", package = "exuber")` for the full
#' picture of which functions do and don't fit that pipeline.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' set.seed(2026)
#' burn <- cumsum(rnorm(60))
#' bubble <- burn[length(burn)] * 1.04^(1:40) + cumsum(rnorm(40, sd = 0.5))
#' y <- c(burn, bubble)
#'
#' r <- radf(y, minw = 20)
#' cv <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
#' ds <- datestamp(r, cv = cv, min_duration = 3)
#'
#' root_ci_datestamp(r, ds) # one row per datestamped episode
#'
#' @importFrom purrr imap pmap
#' @export
root_ci_datestamp <- function(object, ds, level = 0.95, type = c("normal", "cauchy")) {
  type <- match.arg(type)
  x <- mat(object)
  idx <- index(object)

  if ("panel" %in% names(ds) && !("panel" %in% colnames(x))) {
    warning_glue("Dropping 'panel' entry of `ds` -- root inference needs a single series, not a sieve-bootstrap panel result.")
    ds <- ds[names(ds) != "panel"]
  }

  purrr::imap(ds, function(episodes, snm) {
    y <- x[, snm]
    rows <- purrr::pmap(list(episodes$Start, episodes$End), function(s, e) {
      from <- match(s, idx)
      to <- match(e, idx)
      est <- explosive_root(y, from, to)
      ci <- root_ci(est, level = level, type = type)
      data.frame(
        rho = ci$rho, rho_lower = ci$rho_ci[1], rho_upper = ci$rho_ci[2],
        doubling_time = ci$doubling_time,
        doubling_time_lower = ci$doubling_time_ci[1],
        doubling_time_upper = ci$doubling_time_ci[2]
      )
    })
    cbind(
      data.frame(Start = episodes$Start, End = episodes$End),
      do.call(rbind, rows)
    )
  })
}
