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
#'
#' @return A list with \code{rho}, \code{rho_ci} (length-2 vector), and
#' \code{doubling_time}, \code{doubling_time_ci}.
#'
#' @references Guo, G., Sun, Y., & Wang, S. (2019). Testing for moderate
#' explosiveness. The Econometrics Journal, 22(3), 279-303.
#'
#' @export
root_ci <- function(x, level = 0.95) {
  z <- qnorm(1 - (1 - level) / 2)
  rho_ci <- x$rho + c(-1, 1) * z * x$se

  dt <- function(rho) log(2) / log(rho)

  list(
    rho = x$rho,
    rho_ci = rho_ci,
    doubling_time = dt(x$rho),
    doubling_time_ci = c(dt(rho_ci[2]), dt(rho_ci[1]))
  )
}
