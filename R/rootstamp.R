# Inference on the explosive autoregressive root, for use *after* a bubble
# has been detected and dated (e.g. via datestamp()). PSY-style tests only
# answer "is there a bubble"; this answers "how fast is it growing".
#
# Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate explosiveness.
# The Econometrics Journal. Building on Phillips, P. C. B., & Magdalinos, T.
# (2007). Limit theory for moderate deviations from a unit root. Journal of
# Econometrics, 136(1), 115-130.

#' Confidence Interval and Doubling Time for an Explosive Root
#'
#' Fits a no-intercept AR(1) regression \eqn{y_t = \rho y_{t-1} + \epsilon_t}
#' (Phillips & Magdalinos 2007; no intercept, following their eq. 58, "to
#' exclude the presence of a deterministically explosive component") and
#' reports \eqn{\hat\rho} together with a confidence interval and implied
#' \strong{doubling time} \eqn{\log(2)/\log(\hat\rho)} -- the number of
#' periods for the series to double in magnitude at the estimated growth
#' rate. Guo, Sun & Wang (2019) show that -- unlike the classical (stationary
#' or unit-root) case -- the ordinary t-statistic for \eqn{\hat\rho},
#' estimated by OLS with no intercept, is asymptotically \strong{standard
#' normal} under i.i.d. errors (and under weakly dependent errors, with a HAC
#' standard error). This means an ordinary-looking Wald interval,
#' \eqn{\hat\rho \pm z_{\alpha/2}\cdot se(\hat\rho)}, is asymptotically valid
#' here even though \eqn{\hat\rho > 1} -- despite looking identical in form to
#' a classical (invalid, for an explosive root) normal-theory interval, the
#' justification is different (Guo, Sun & Wang's explosive-root CLT, not the
#' classical stationary one).
#'
#' \code{type = "cauchy"} instead uses the Phillips & Magdalinos (2007)
#' fixed-root result (their eq. 27, restating White 1958): for a genuinely
#' explosive, non-drifting root, \eqn{\frac{\rho^n}{\rho^2-1}(\hat\rho-\rho)}
#' converges to a standard Cauchy variate. Plugging in \eqn{\hat\rho} for the
#' unknown \eqn{\rho} in the normalization (the usual practice for this kind
#' of self-normalized pivot) gives \eqn{\hat\rho \pm q_{\alpha/2}\cdot
#' (\hat\rho^2-1)/\hat\rho^n}, with \eqn{q_{\alpha/2}} a standard-Cauchy
#' quantile. This interval assumes a \emph{fixed} explosive root (no drift,
#' no unknown localizing rate); the default \code{"normal"} type is the safer
#' choice when that assumption is in doubt, since Guo, Sun & Wang's result
#' allows drift and weak dependence.
#'
#' Two methods, for two different starting points:
#' \itemize{
#'  \item \strong{Default}: \code{object} is a numeric vector -- the
#'  sub-sample to fit (e.g. an episode already sliced out by hand, or by
#'  position from a \code{\link{datestamp}} result:
#'  \code{y[from:to]}). Fits once and returns one CI.
#'  \item \strong{\code{radf_obj}}: \code{object} is the \code{radf_obj} a
#'  \code{\link{datestamp}} result \code{ds} was computed on. Runs the
#'  default method once per datestamped episode, per series, slicing
#'  \code{object}'s own data itself -- no manual loop needed.
#' }
#'
#' @param object For the default method, a numeric vector (the sub-sample to
#' fit -- already sliced to the episode of interest). For the \code{radf_obj}
#' method, the \code{radf_obj} that \code{ds} was computed on.
#' @param ds (\code{radf_obj} method only) A \code{\link{datestamp}} result
#' computed on \code{object}. Root inference on a very short episode is
#' statistically meaningless -- set \code{min_duration} in that
#' \code{\link{datestamp}} call to exclude episodes too short for reliable
#' root inference, rather than expecting this method to second-guess what
#' counts as "too short".
#' @param level Confidence level (default 0.95).
#' @param type \code{"normal"} (default) for Guo, Sun & Wang's normal-t
#' interval, or \code{"cauchy"} for the Phillips-Magdalinos fixed-root
#' Cauchy interval.
#' @param ... further arguments passed to methods.
#'
#' @return The default method returns a \code{rootstamp_est} object (a list
#' with \code{rho}, \code{se}, \code{t_stat}, \code{n}, \code{rho_ci},
#' \code{doubling_time}, \code{doubling_time_ci}, with its own \code{print()}
#' method).
#'
#' The \code{radf_obj} method returns a \code{rootstamp_episodes} object (a
#' named list, one element per series in \code{ds}; the panel
#' sieve-bootstrap case, whose \code{ds} entry is named \code{"panel"} and
#' has no single corresponding series, is dropped with a warning), each a
#' data frame with one row per datestamped episode: \code{Start}, \code{End},
#' \code{rho}, \code{rho_lower}, \code{rho_upper}, \code{doubling_time},
#' \code{doubling_time_lower}, \code{doubling_time_upper} -- also with its
#' own \code{print()} method.
#'
#' @references Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate
#' explosiveness. The Econometrics Journal, 22(3), 279-303.
#' @references Phillips, P. C. B., & Magdalinos, T. (2007). Limit theory for
#' moderate deviations from a unit root. Journal of Econometrics, 136(1),
#' 115-130.
#'
#' @note Neither method's return value carries the `radf_obj` class -- even
#' the \code{radf_obj} method, which dispatches on that class for its
#' \emph{input}, returns its own \code{rootstamp_episodes} class -- so
#' \code{rootstamp()} does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`/`autoplot`. See
#' `vignette("naming-and-analysis", package = "exuber")` for the full picture
#' of which functions do and don't fit that pipeline.
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
#' # default method: one episode, sliced by hand
#' rootstamp(y[ds[["series1"]]$Start[1]:ds[["series1"]]$End[1]]) # true rho = 1.04
#' rootstamp(y[ds[["series1"]]$Start[1]:ds[["series1"]]$End[1]], type = "cauchy")
#'
#' @export
rootstamp <- function(object, ...) {
  UseMethod("rootstamp")
}

#' @rdname rootstamp
#' @export
rootstamp.default <- function(object, level = 0.95, type = c("normal", "cauchy"), ...) {
  type <- match.arg(type)
  alpha <- 1 - level

  y <- as.numeric(object)
  y_lag <- y[-length(y)]
  dy <- diff(y)

  sxx <- sum(y_lag^2)
  sxy <- sum(y_lag * dy)
  beta <- sxy / sxx
  res <- dy - beta * y_lag
  n <- length(dy)
  sigma2 <- sum(res^2) / (n - 1)
  se <- sqrt(sigma2 / sxx)
  rho <- 1 + beta
  t_stat <- beta / se

  rho_ci <- if (type == "normal") {
    z <- qnorm(1 - alpha / 2)
    rho + c(-1, 1) * z * se
  } else {
    q <- qcauchy(1 - alpha / 2)
    half_width <- q * (rho^2 - 1) / rho^n
    rho + c(-1, 1) * half_width
  }

  dt <- function(rho) log(2) / log(rho)

  list(
    rho = rho, se = se, t_stat = t_stat, n = n,
    rho_ci = rho_ci,
    doubling_time = dt(rho),
    doubling_time_ci = c(dt(rho_ci[2]), dt(rho_ci[1]))
  ) %>%
    add_attr(level = level, type = type) %>%
    add_class("rootstamp_est")
}

#' @rdname rootstamp
#' @importFrom purrr imap pmap
#' @export
#'
#' @examples
#'
#' # radf_obj method: every datestamped episode at once
#' rootstamp(r, ds)
rootstamp.radf_obj <- function(object, ds, level = 0.95, type = c("normal", "cauchy"), ...) {
  type <- match.arg(type)
  x <- mat(object)
  idx <- index(object)

  if ("panel" %in% names(ds) && !("panel" %in% colnames(x))) {
    warning_glue("Dropping 'panel' entry of `ds` -- root inference needs a single series, not a sieve-bootstrap panel result.")
    ds <- ds[names(ds) != "panel"]
  }

  res <- purrr::imap(ds, function(episodes, snm) {
    y <- x[, snm]
    rows <- purrr::pmap(list(episodes$Start, episodes$End), function(s, e) {
      from <- match(s, idx)
      to <- match(e, idx)
      ci <- rootstamp.default(y[from:to], level = level, type = type)
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

  res %>%
    add_attr(level = level, type = type) %>%
    add_class("rootstamp_episodes")
}

#' @export
print.rootstamp_est <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cli::cat_line()
  cli::cat_rule(left = glue(
    "rootstamp (n = {x$n}, level = {attr(x, 'level') * 100}%, type = {attr(x, 'type')})"
  ))
  cli::cat_line()
  print(
    data.frame(
      rho = x$rho, se = x$se, t_stat = x$t_stat,
      rho_lower = x$rho_ci[1], rho_upper = x$rho_ci[2],
      doubling_time = x$doubling_time,
      dt_lower = x$doubling_time_ci[1], dt_upper = x$doubling_time_ci[2],
      row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cli::cat_line()
  invisible(x)
}

#' @export
print.rootstamp_episodes <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  if (length(x) == 0) {
    return(invisible(NULL))
  }
  cli::cat_line()
  cli::cat_rule(left = glue(
    "rootstamp (level = {attr(x, 'level') * 100}%, type = {attr(x, 'type')})"
  ))
  cli::cat_line()
  print.listof(x, digits = digits)
  cli::cat_line()
  invisible(x)
}
