#' Simulation of a single-bubble process
#'
#' The following function generates a time series which switches from a martingale to a mildly explosive
#' process and then back to a martingale.
#'
#' @param n A strictly positive integer specifying the length of the simulated output series.
#' @param te A scalar in (0, tf) specifying the observation in which the bubble originates.
#' @param tf A scalar in (te, n) specifying the observation in which the bubble collapses.
#' @param c A positive scalar determining the autoregressive coefficient in the explosive regime.
#' @param alpha A positive scalar in (0, 1) determining the value of the expansion rate in the autoregressive coefficient.
#' @param sigma A positive scalar indicating the standard deviation of the innovations.
#'
#' @details
#' The data generating process is described by the following equation:
#' \deqn{X_t = X_{t-1}1\{t < \tau_e\}+ \delta_T X_{t-1}1\{\tau_e \leq t\leq \tau_f\} +
#' \left(\sum_{k=\tau_f+1}^t \epsilon_k + X^*_{\tau_f}\right) 1\{t > \tau_f\} + \epsilon_t 1\{t \leq \tau_f\}}{X[t] =
#' X[t-1] 1{t < te}+ \delta[T] * X[t-1] 1{te \le t \le tf} +
#' (\sum[k=tf+1]^t \epsilon[k] + X'[tf]) 1{t > tf} + \epsilon[t] 1{t \le tf},}
#'
#' where the autoregressive coefficient \eqn{\delta_T}{\delta[T]} is given by:
#'
#' \deqn{\delta_T = 1 + cT^{-a}}{\delta[T] = 1 + c*T^{-a}}
#'
#' with \eqn{c>0}, \eqn{\alpha \in (0,1)}{\alpha in (0,1)},
#' \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)} and
#' \eqn{X_{\tau_f} = X_{\tau_e} + X^*}{X[tf] = X[te] + X'} \eqn{\tau}{t} is the last observation of the sample.
#' During the pre- and post- bubble periods, \eqn{N_0 = [1, \tau_e)}{N0 = [1, te)}, X is a pure random walk process.
#' During the bubble expansion period \eqn{B = [\tau_e, \tau_f]}{B = [te,tf]} is a mildly explosive process with expansion rate given by the autoregressive
#' coefficient \eqn{\delta_T}{\delta[T]}, and continues its martingale path for the subsequent period
#' \eqn{N_1 = (\tau_f, \tau]}{N1 = (tf, t]}.
#'
#'
#' For further details the user can refer to Phillips et al., (2015) p. 1054.
#'
#' @return A numeric vector of length n.
#' @export
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple Bubbles:
#' Historical Episodes of Exuberance and Collapse in the S&P 500. International Economic Review, 5
#' 6(4), 1043-1078.
#'
#' @seealso \code{\link{sim_dgp2}}, \code{\link{sim_blan}}, \code{\link{sim_evans}}
#'
#' @examples
#' # 100 periods with bubble origination date 40 and termination date 55
#' sim_dgp1(n = 100)
#'
#' # 200 periods with bubble origination date 80 and termination date 110
#' sim_dgp1(n = 200)
#'
#' # 200 periods with bubble origination date 100 and termination date 150
#' sim_dgp1(n = 200, te = 100, tf = 150)
sim_dgp1 <- function(n, te = 0.4 * n, tf = 0.15 * n + te, c = 1,
                     alpha = 0.6, sigma = 6.79) {
  assert_positive_int(n)
  assert_between(te, 0, n)
  assert_between(tf, te, n)
  assert_positive_int(c)
  assert_between(alpha, 0, 1)
  stopifnot(sigma >= 0)

  delta <- 1 + c * n^(-alpha)
  y <- 100

  for (i in 1:(n - 1)) {
    if (i < te) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if (i >= te & i < tf) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    } else if (i == tf) {
      y[i + 1] <- y[te]
    } else {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    }
  }
  return(y)
}

#' Simulation of a two-bubble process
#'
#' The following data generating process is similar to  \code{\link{sim_dgp1}}, with the difference that
#' there are two episodes of midly explosive dynamics.
#'
#' @inheritParams sim_dgp1
#' @param te1 A scalar in (0, n) specifying the observation in which the first bubble originates.
#' @param tf1 A scalar in  (te1, n) specifying the observation in which the first bubble collapses.
#' @param te2 A scalar in (tf1, n) specifying the observation in which the second bubble originates.
#' @param tf2 A scalar in (te2, n) specifying the observation in which the second bubble collapses.
#'
#' @details
#' The data generating process is described by:
#'
#' \deqn{X_t = X_{t-1}1\{t \in N_0\}+ \delta_T X_{t-1}1\{t \in B_1 \cup B_2\} +
#' \left(\sum_{k=\tau_{1f}+1}^t \epsilon_k + X^*_{\tau_{1f}}\right) 1\{t \in N_1\} }{X[t]=X[t-1]
#' 1{t in N[0]}+ \delta[T] * X[t-1] 1{t in B[1] union B[2]} +
#' (\sum[k=t1f+1]^t \epsilon[k] + X'[t1f]) 1{t in N[1]} +
#' }
#'
#' \deqn{ + \left(\sum_{l=\tau_{2f}+1}^t \epsilon_l + X^*_{\tau_{2f}}\right) 1\{t \in N_2\} +
#' \epsilon_t 1\{t \in N_0 \cup B_1 \cup B_2\}}{(\sum[l=t2f+1]^t \epsilon[l] + X'[t2f]) 1{t in N[2]} +
#' \epsilon[t] 1{t in N[0] union B[1] union B[2]},}
#'
#' where the autoregressive coefficient \eqn{\delta_T}{\delta[T]} is given:
#'
#' \deqn{\delta_T = 1 + cT^{-a}}{\delta[T] = 1 + c*T^{-a},}
#'
#' with \eqn{c>0}, \eqn{\alpha \in (0,1)}{\alpha in (0,1)},
#' \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)},
#' \eqn{X_{\tau_{1f}} = X_{\tau_{1e}} + X^*}{X[t1f] = X[t1e] + X'} and
#' \eqn{X_{\tau_{2f}} = X_{\tau_{2e}} + X^*}{X[t2f] = X[t2e] + X'}.
#' We use the notation
#' \eqn{N_0 = [1, \tau_{1e})}{N0 = [1, t1e)},
#' \eqn{B_1 = [\tau_{1e}, \tau_{1f}]}{B1 = [te1, t1f]},
#' \eqn{N_1 = (\tau_{1f}, \tau_{2e})}{N0 = (t1f, t2e)},
#' \eqn{B_2 = [\tau_{2e}, \tau_{2f}]}{N0 = [t2e, t2f]},
#' \eqn{N_2 = (\tau_{2f}, \tau]}{N0 = [t2f, t]}, where \eqn{\tau}{t} is the last observation of the sample.
#' After the collapse of the first bubble, \eqn{X_t}{X[t]} resumes a martingale path until time
#' \eqn{\tau_{2e}-1}{t2e - 1}, and a second episode of exuberance begins at \eqn{\tau_{2e}}{t2e}.
#' The expansion process lasts until \eqn{\tau_{2f}}{t2f} and collapses to a value of
#' \eqn{X^*_{\tau_{2f}}}{X'[t2f]}. The process then continues on a martingale path until the end of the
#' sample period \eqn{\tau}{t}. The expansion duration of the first bubble is assumed to be longer than
#' that of the second bubble, i.e. \eqn{\tau_{1f}-\tau_{1e}>\tau_{2f}-\tau_{2e}}{t1f - t1e > t2f - t2e}.
#'
#' For further details the user can refer to Phillips et al., (2015) p. 1055.
#'
#' @return A numeric vector of length \code{n}.
#' @export
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for Multiple Bubbles:
#' Historical Episodes of Exuberance and Collapse in the S&P 500. International Economic Review, 5
#' 6(4), 1043-1078.
#'
#' @seealso \code{\link{sim_dgp1}}, \code{\link{sim_blan}}, \code{\link{sim_evans}}
#'
#' @examples
#' # 100 periods with bubble origination dates 20/60 and termination dates 40/70 respectively
#' sim_dgp2(n = 100)
#'
#' # 200 periods with bubble origination dates 40/120 and termination dates 80/140 respectively
#' sim_dgp2(n = 200)
sim_dgp2 <- function(n, te1 = 0.2 * n, tf1 = 0.2 * n + te1,
                     te2 = 0.6 * n, tf2 = 0.1 * n + te2,
                     c = 1, alpha = 0.6, sigma = 6.79) {
  assert_positive_int(n)
  assert_between(te1, 0, n)
  assert_between(tf1, te1, n)
  assert_between(te2, tf1, n)
  assert_between(tf2, te2, n)
  assert_between(alpha, 0, 1)
  stopifnot(sigma >= 0)

  delta <- 1 + c * n^(-alpha)
  y <- 100

  for (i in 1:(n - 1)) {
    if (i < te1) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if (i >= te1 & i < tf1) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    } else if (i == tf1) {
      y[i + 1] <- y[te1]
    } else if (i > tf1 & i < te2) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if (i >= te2 & i < tf2) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    } else if (i == tf2) {
      y[i + 1] <- y[te2]
    } else {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    }
  }

  return(y)
}

#' Simulation of a Blanchard (1979) bubble process
#'
#' Simulation of a Blanchard (1979) rational bubble process.
#'
#' @inheritParams sim_dgp1
#' @param pi A positive value in (0, 1) which governs the probability of the bubble continuing to grow.
#' @param r A positive scalar that determines the growth rate of the bubble process.
#'
#' @export
#' @return A numeric vector of length \code{n}.
#'
#' @importFrom stats rbinom
#' @details
#' Blanchard's bubble process has two regimes, which occur with probability \eqn{\pi} and \eqn{1-\pi}.
#' In the first regime, the bubble grows exponentially, whereas in the second regime, the bubble
#' collapses to a white noise.
#'
#' With probability \eqn{\pi}:
#' \deqn{B_{t+1} = \frac{1+r}{\pi}B_t+\epsilon_{t+1}}{B[t+1]=(1+r)/\pi*B[t]+\epsilon[t+1],}
#' With probability \eqn{1 - \pi}:
#' \deqn{B_{t+1} = \epsilon_{t+1}}{B[t+1] = \epsilon[t+1],}
#'
#' where \code{r} is a positive constant and \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)}.
#'
#'
#' @references Blanchard, O. J. (1979). Speculative bubbles, crashes and rational expectations.
#' Economics letters, 3(4), 387-389.
#'
#' @seealso \code{\link{sim_dgp1}}, \code{\link{sim_dgp2}}, \code{\link{sim_evans}}
#'
#' @examples
#' sim_blan(n = 100)
sim_blan <- function(n, pi = 0.7, sigma = 0.03, r = 0.05) {
  assert_positive_int(n)
  assert_between(pi, 0, 1)
  stopifnot(sigma >= 0)
  stopifnot(r >= 0)

  b <- 1
  theta <- rbinom(n, 1, pi)
  i <- 1
  while (i < n) {
    if (b[i] > 0) {
      if (theta[i] == 1) {
        b[i + 1] <- (1 + r) / pi * b[i] + rnorm(1, 0, sigma)
      } else {
        b[i + 1] <- rnorm(1, 0, sigma)
      }
      i <- i + 1
    } else {
      i <- i - 1
    }
  }
  return(b)
}

#' Simulation of an Evans (1991) bubble process
#'
#' Simulation of an Evans (1991) rational periodically collapsing bubble process.
#'
#' @inheritParams sim_blan
#' @param delta A positive scalar, with restrictions (see details).
#' @param tau The standard deviation of the innovations.
#' @param alpha A positive scalar, with restrictions (see details).
#' @param b1 A positive scalar, the inital value of the series. Defaults to \code{delta}.
#'
#' @return A numeric vector of length \code{n}.
#'
#' @importFrom stats rbinom
#'
#' @details
#'
#' \code{delta} and \code{alpha} are positive parameters which satisfy \eqn{0 < \delta < (1+r)\alpha}.
#' \code{delta} represents the size of the bubble after collapse.
#' The default value of \code{r} is 0.05.
#' The function checks whether \code{alpha} and \code{delta} satisfy this condition and will return an error if not.
#'
#' The Evans bubble has two regimes. If \eqn{B_t \leq \alpha}{B[t] \le \alpha} the bubble grows at an average rate of \eqn{1 + r}:
#'
#' \deqn{B_{t+1} = (1+r) B_t u_{t+1},}{B[t+1]= (1+r)*B[t]*u[t+1].}
#'
#' When \eqn{B_t > \alpha}{B[t] > \alpha} the bubble expands at an increased rate of \eqn{(1+r)\pi^{-1}}:
#'
#' \deqn{B_{t+1} =  [\delta + (1+r)\pi^{-1} \theta_{t+1}(B_t -  (1+r)^{-1}\delta B_t )]u_{t+1},}{B[t+1] = \delta*(1+r)/\pi* (B[t]-\delta/(1+r))) *u[t+1],}
#'
#' where \eqn{\theta} is an indicator function taking a value of 0 with probability \eqn{1-\pi} and 1 with probability \eqn{\pi}.
#' In this secondary phase there is a probability (\eqn{1-\pi}) that the bubble collapses to \code{delta} and the process starts again.
#' By modifying the values of \code{delta}, \code{alpha} and \code{pi} the user can change the frequency at which bubbles appear, the mean duration of a bubble before collapse and the scale of the bubble.
#'
#' @export
#'
#' @seealso \code{\link{sim_dgp1}}, \code{\link{sim_dgp2}}, \code{\link{sim_blan}}
#'
#' @references Evans, G. W. (1991). Pitfalls in testing for explosive
#' bubbles in asset prices. The American Economic Review, 81(4), 922-930.
#'
sim_evans <- function(n, alpha = 1, delta = 0.5,
                      tau = 0.05, pi = 0.7, r = 0.05, b1 = delta) {

  # checks here
  assert_positive_int(n)
  stopifnot(alpha > 0)
  if (delta < 0 | delta > (1 + r) * alpha) {
    stop("alpha and delta should satisfy: 0 < delta < (1+r)*alpha", call. = FALSE)
  }
  assert_between(pi, 0, 1)
  stopifnot(r >= 0)

  y <- rnorm(n, 0, tau)
  u <- exp(y - tau^2 / 2)
  theta <- rbinom(n, 1, pi)
  b <- b1

  for (i in 1:(n - 1)) {
    if (b[i] <= alpha) {
      b[i + 1] <- (1 + r) * b[i] * u[i + 1]
    } else {
      b[i + 1] <- (delta + pi^(-1) * (1 + r) * theta[i + 1] * (b[i] -
        (1 + r)^(-1) * delta)) * u[i + 1]
    }
  }
  return(b)
}

#' Simulation of dividends
#'
#' Simulate (log) dividends from a random walk with drift.
#'
#' @inheritParams sim_dgp1
#' @param mu A scalar indicating the drift.
#' @param r A positive value indicating the discount factor.
#' @param log A logical. If true dividends follow a lognormal distribution.
#' @param output A character string giving the fundamental price("pf") or
#' dividend series("d"). Default is `pf'.
#'
#' @return A numeric vector of length n.
#' @export
#'
#' @details
#'
#' If log is set to FALSE (default value) the dividends follow:
#'
#' \deqn{d_t = \mu + d_{t-1} + \epsilon_t}{d[t] = \mu + d[t-1] + \epsilon[t],}
#'
#' where \eqn{\epsilon \sim \mathcal{N}(0, \sigma^2)}{\epsilon - N(0, \sigma^2)}. The default parameters
#' are \eqn{\mu = 0.0373}, \eqn{\sigma^2 = 0.1574} and \eqn{d[0] = 1.3} (the initial value of the dividend sequence).
#' The above equation can be solved to yield the fundamental price:
#'
#' \deqn{F_t = \mu(1+r)r^{-2} + r^{-1}d_t}{F[t] = \mu * (1 + r)/r^2 + d[t]/r.}
#'
#' If log is set to TRUE then dividends follow a lognormal distribution or log(dividends) follow:
#'
#' \deqn{\ln(d_t) = \mu + \ln(d_{t-1}) + \epsilon_t}{ln(d[t]) = \mu + ln(d[t-1]) + \epsilon[t],}
#'
#' where \eqn{\epsilon \sim \mathcal{N}(0, \sigma^2)}{\epsilon - N(0, \sigma^2)}. Default parameters are
#' \eqn{\mu = 0.013}, \eqn{\sigma^2 = 0.16}. The fundamental price for this case is:
#'
#' \deqn{F_t = \frac{1+g}{r-g}d_t}{F[t] = (1 + g)/(r -g) * d[t],}
#'
#' where \eqn{1+g=\exp(\mu+\sigma^2/2)}{1 + g = exp(\mu + \sigma^2/2)}.
#' All default parameter values are those suggested by West (1988).
#'
#' @references West, K. D. (1988). Dividend innovations and stock price volatility.
#' Econometrica: Journal of the Econometric Society, p. 37-61.
#'
#' @examples
#' # Price is the sum of the bubble and fundamental components
#' # 20 is the scaling factor
#' pf <- sim_div(100, r = 0.05, output = "pf")
#' pb <- sim_evans(100, r = 0.05)
#' p <- pf + 20*pb
sim_div <- function(n, mu, sigma, r = 0.05, log = FALSE, output = c("pf", "d")) {
  initval <- 1.3
  # Values obtained from West(1988, p53)
  if (missing(mu)) if (log) mu <- 0.013 else mu <- 0.0373
  if (missing(sigma)) if (log) sigma <- sqrt(0.16) else sigma <- sqrt(0.1574)

  assert_positive_int(n)
  stopifnot(sigma >= 0)
  stopifnot(r >= 0)
  stopifnot(is.logical(log))
  return <- match.arg(output)

  d <- stats::filter(mu + c(initval, rnorm(n - 1, 0, sigma)),
    c(1),
    init = 1.3, method = "recursive"
  )

  if (log) {
    g <- exp(mu + sigma^2 / 2) - 1
    pf <- (1 + g) * d / (r - g)
  } else {
    pf <- mu * (1 + r) * r^(-2) + d / r
  }

  if (return == "pf") {
    return(pf)
  } else {
    return(d)
  }
}
