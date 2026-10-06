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
#' (Phillips & Magdalinos 2007, who omit the intercept in their eq. 58 "to exclude
#' the presence of a deterministically explosive component") and reports
#' \eqn{\hat\rho} together with a confidence interval and the implied
#' \strong{doubling time} \eqn{\log(2)/\log(\hat\rho)}, which is the number of
#' periods the series needs to double in magnitude at the estimated growth rate.
#'
#' Guo, Sun & Wang (2019) show that the ordinary t-statistic for \eqn{\hat\rho},
#' estimated by OLS with no intercept, is asymptotically \strong{standard normal}
#' under i.i.d. errors, and also under weakly dependent errors when a HAC standard
#' error is used. This differs from the classical stationary and unit-root cases.
#' An ordinary-looking Wald interval, \eqn{\hat\rho \pm z_{\alpha/2}\cdot
#' se(\hat\rho)}, is therefore asymptotically valid here even though
#' \eqn{\hat\rho > 1}. The interval has the same form as a classical normal-theory
#' interval, which would be invalid for an explosive root, but the justification
#' is different: it rests on the explosive-root central limit theorem of Guo, Sun
#' & Wang and not on the classical stationary one.
#'
#' \code{type = "cauchy"} instead uses the fixed-root result of Phillips &
#' Magdalinos (2007, their eq. 27, which restates White 1958). For an explosive
#' root that does not drift, \eqn{\frac{\rho^n}{\rho^2-1}(\hat\rho-\rho)}
#' converges to a standard Cauchy variate. We replace the unknown \eqn{\rho} in the
#' normalization with \eqn{\hat\rho}, as is usual for this kind of self-normalized
#' pivot, and obtain \eqn{\hat\rho \pm q_{\alpha/2}\cdot
#' (\hat\rho^2-1)/\hat\rho^n}, where \eqn{q_{\alpha/2}} is a standard-Cauchy
#' quantile. This interval assumes a \emph{fixed} explosive root, with no drift and
#' no unknown localizing rate. When that assumption is in doubt, the default
#' \code{"normal"} type is safer, because the result of Guo, Sun & Wang allows for
#' drift and weak dependence.
#'
#' There are two methods, for two different starting points:
#' \itemize{
#'  \item \strong{Default}: \code{object} is a numeric vector, the sub-sample to
#'  fit. It can be an episode that you sliced out by hand or by position from a
#'  \code{\link{datestamp}} result (\code{y[from:to]}). The method fits the
#'  sub-sample once and returns one confidence interval.
#'  \item \strong{\code{radf_obj}}: \code{object} is the \code{radf_obj} on which
#'  a \code{\link{datestamp}} result \code{ds} was computed. The method runs the
#'  default method once for each datestamped episode of each series and slices the
#'  data of \code{object} itself, so no manual loop is needed.
#' }
#'
#' @param object For the default method, a numeric vector with the sub-sample to
#' fit, already sliced to the episode of interest. For the \code{radf_obj} method,
#' the \code{radf_obj} on which \code{ds} was computed.
#' @param ds (\code{radf_obj} method only) A \code{\link{datestamp}} result
#' computed on \code{object}. Root inference on a very short episode is
#' statistically meaningless. Set \code{min_duration} in that
#' \code{\link{datestamp}} call to exclude episodes that are too short for reliable
#' root inference, because this method does not decide what counts as too short.
#' @param sig_lvl Confidence level of the interval on the 0 to 100 scale used
#' throughout the package (default \code{95}). Any value in \code{[50, 100)} is
#' allowed.
#' @param type \code{"normal"} (default) for the normal-t interval of Guo, Sun &
#' Wang, or \code{"cauchy"} for the fixed-root Cauchy interval of Phillips &
#' Magdalinos.
#' @param ... Further arguments passed to methods.
#'
#' @return The default method returns a \code{rootstamp_est} object, which is a
#' list with \code{rho}, \code{se}, \code{t_stat}, \code{n}, \code{rho_ci},
#' \code{doubling_time} and \code{doubling_time_ci} and has its own \code{print()}
#' method.
#'
#' The \code{radf_obj} method returns a \code{rootstamp_episodes} object, which is a
#' named list with one element for each series in \code{ds}. Each element is a
#' data frame with one row for each datestamped episode and the columns
#' \code{Start}, \code{End}, \code{rho}, \code{rho_lower}, \code{rho_upper},
#' \code{doubling_time}, \code{doubling_time_lower} and
#' \code{doubling_time_upper}. The object has its own \code{print()} method. The
#' panel sieve-bootstrap case has a \code{ds} entry named \code{"panel"} that
#' corresponds to no single series, and it is dropped with a warning.
#'
#' @references Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate
#' explosiveness. The Econometrics Journal, 22(3), 279-303.
#' @references Phillips, P. C. B., & Magdalinos, T. (2007). Limit theory for
#' moderate deviations from a unit root. Journal of Econometrics, 136(1),
#' 115-130.
#'
#' @note Neither method returns the `radf_obj` class. Even the \code{radf_obj}
#' method, which dispatches on that class for its \emph{input}, returns its own
#' \code{rootstamp_episodes} class. \code{rootstamp()} therefore does not work with
#' `summary()`, `\link{datestamp}`, `tidy` and `autoplot`. See
#' `vignette("naming-and-analysis", package = "exuber")` for which functions fit
#' that pipeline and which do not.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' # The martingale-to-explosive process of sim_psy1(), explosive through the sample end
#' y <- sim_psy1(n = 100, te = 60, tf = 100, seed = 2026)
#'
#' r <- radf(y, minw = 20)
#' cv <- radf_mc_cv(length(y), minw = 20, nrep = 300, seed = 4)
#' ds <- datestamp(r, cv = cv, min_duration = 3)
#'
#' # default method: one episode, sliced by hand
#' ep <- y[ds[["series1"]]$Start[1]:ds[["series1"]]$End[1]]
#' fit <- rootstamp(ep) # recovers the DGP's explosive AR coefficient
#' fit
#' rootstamp(ep, type = "cauchy")
#'
#' # Plot the episode with the fitted explosive-root path overlaid
#' autoplot(fit)
#'
#' @family dating
#' @export
rootstamp <- function(object, ...) {
  UseMethod("rootstamp")
}

#' @rdname rootstamp
#' @export
rootstamp.default <- function(object, sig_lvl = 95, type = c("normal", "cauchy"), ...) {
  type <- match.arg(type)
  assert_sig_lvl(sig_lvl, choices = NULL)
  alpha <- 1 - sig_lvl / 100

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
    rho = rho,
    se = se,
    t_stat = t_stat,
    n = n,
    rho_ci = rho_ci,
    doubling_time = dt(rho),
    doubling_time_ci = c(dt(rho_ci[2]), dt(rho_ci[1]))
  ) %>%
    add_attr(sig_lvl = sig_lvl, type = type, y = y) %>%
    add_class("rootstamp_est")
}

#' @rdname rootstamp
#' @importFrom purrr imap pmap
#' @export
#'
#' @examples
#'
#' # radf_obj method: every datestamped episode at once
#' res_all <- rootstamp(r, ds)
#' res_all
#'
#' # Plot the estimated rho (with its CI) for every episode
#' autoplot(res_all)
rootstamp.radf_obj <- function(object, ds, sig_lvl = 95, type = c("normal", "cauchy"), ...) {
  type <- match.arg(type)
  x <- mat(object)
  idx <- index(object)

  if ("panel" %in% names(ds) && !("panel" %in% colnames(x))) {
    warning_glue(
      "Dropping 'panel' entry of `ds` -- root inference needs a single series, not a sieve-bootstrap panel result."
    )
    ds <- ds[names(ds) != "panel"]
  }

  res <- purrr::imap(ds, function(episodes, snm) {
    y <- x[, snm]
    rows <- purrr::pmap(list(episodes$Start, episodes$End), function(s, e) {
      from <- match(s, idx)
      to <- match(e, idx)
      ci <- rootstamp.default(y[from:to], sig_lvl = sig_lvl, type = type)
      data.frame(
        rho = ci$rho,
        rho_lower = ci$rho_ci[1],
        rho_upper = ci$rho_ci[2],
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
    add_attr(sig_lvl = sig_lvl, type = type) %>%
    add_class("rootstamp_episodes")
}

#' @export
print.rootstamp_est <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cli::cat_line()
  cli::cat_rule(
    left = glue(
      "rootstamp (n = {x$n}, sig_lvl = {attr(x, 'sig_lvl')}%, type = {attr(x, 'type')})"
    )
  )
  cli::cat_line()
  print(
    data.frame(
      rho = x$rho,
      se = x$se,
      t_stat = x$t_stat,
      rho_lower = x$rho_ci[1],
      rho_upper = x$rho_ci[2],
      doubling_time = x$doubling_time,
      dt_lower = x$doubling_time_ci[1],
      dt_upper = x$doubling_time_ci[2],
      row.names = NULL
    ),
    digits = digits,
    print.gap = 2L,
    row.names = FALSE
  )
  cli::cat_line()
  invisible(x)
}

#' Plot method for rootstamp() output on a single sub-sample
#'
#' Plots the sub-sample against the fitted explosive path implied by the
#' estimated root, \eqn{y_1 \rho^{t-1}}.
#'
#' @param object An object of class \code{rootstamp_est}, the output of the
#' default \code{\link{rootstamp}} method.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{rootstamp}}
#' @export
autoplot.rootstamp_est <- function(object, ...) {
  y <- attr(object, "y")
  n <- length(y)
  fitted <- y[1] * object$rho^(0:(n - 1L))
  tibble(t = seq_len(n), y = y, fitted = fitted) %>%
    pivot_longer(c(y, fitted), names_to = "series", values_to = "value") %>%
    ggplot(aes(t, value, color = series, linetype = series)) +
    geom_line() +
    scale_color_manual(values = c(y = "black", fitted = "red")) +
    scale_linetype_manual(values = c(y = 1, fitted = 2)) +
    labs(
      title = paste0(
        "rootstamp: rho = ",
        round(object$rho, 3),
        ", doubling time = ",
        round(object$doubling_time, 1)
      )
    ) +
    theme_exuber() +
    theme(legend.position = "bottom", legend.title = element_blank())
}

#' @export
print.rootstamp_episodes <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  if (length(x) == 0) {
    return(invisible(NULL))
  }
  cli::cat_line()
  cli::cat_rule(
    left = glue(
      "rootstamp (sig_lvl = {attr(x, 'sig_lvl')}%, type = {attr(x, 'type')})"
    )
  )
  cli::cat_line()
  print.listof(x, digits = digits)
  cli::cat_line()
  invisible(x)
}

#' Plot method for rootstamp() output on datestamped episodes
#'
#' Plots the estimated root and its confidence interval for every episode,
#' one panel per series.
#'
#' @param object An object of class \code{rootstamp_episodes}, the output of
#' the \code{radf_obj} \code{\link{rootstamp}} method.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{rootstamp}}
#' @export
autoplot.rootstamp_episodes <- function(object, ...) {
  df <- object %>%
    purrr::imap_dfr(~ mutate(.x, id = .y, mid = (Start + End) / 2))
  gg <- ggplot(df, aes(mid, rho)) +
    geom_pointrange(aes(ymin = rho_lower, ymax = rho_upper)) +
    labs(x = NULL, y = "rho (with CI)", title = "rootstamp: explosive-root estimate per episode") +
    theme_exuber()
  if (length(unique(df$id)) > 1) gg + facet_wrap(~id, scales = "free_x") else gg
}
