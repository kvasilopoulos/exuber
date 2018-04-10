
sim_rw <-  function(n, d = 1, eta = 1){
  y <- matrix(cumsum(rnorm(n, mean = d*n^(-eta))))
}

sim_het <- function(n, d = 1, eta = 1, omega = 30.69,
                    alpha = 0, beta = 0.61){
  h <- y <-  0
  for (i in 1:(n - 1)) {
    h[i + 1] <- omega + alpha*rnorm(1) + beta*h[i]
    y[i + 1]  <-  d*n^(-eta) + y[i] + exp(h[i + 1]/2)*rnorm(1)
  }
  return(y)
}

sim_ar <- function(n, ar = 1.02, drift = 0.05) {
  p = 0
  for (i in 1:(n - 1)) {
    p[i + 1] <- drift + ar * p[i] + rnorm(1, 0, 1 - 0.1^2)
  }
  return(matrix(p))
}

#' Simulation of a single-bubble episode
#'
#' The following generating process is an effective reduced-form mechanism that switches betweem a martingale
#' mechanism, a single mildy explosive episode, collapse, and subsequent renewal of martingale behavior
#'
#' @param n a positive integer indicating the number of simulations
#' @param te a value in (0,n) dating the origination of bubble expansion
#' @param tf a value in (te,n) dating the termination of bubble collapse
#' @param c a positive  value determining the value of the constant in the autoregressive coeffcient
#' @param alpha a positve value in (0,1) determining the value of the expansion rate in the autoregressive coefficient
#' @param sigma a posiitve number indicating the standard deviation of the innovations
#
#'
#' @details
#' The data generating process can be described with the following equation:
#' \deqn{X_t = X_{t-1}1\{t < \tau_e\}+ \delta_T X_{t-1}1\{\tau_e \leq t\leq \tau_f\} +
#' \left(\sum_{k=\tau_f+1}^t \epsilon_k + X^*_{\tau_f}\right) 1\{t > \tau_f\} + \epsilon_t 1\{t \leq \tau_f\}}{X[t] =
#' X[t-1] 1{t < te}+ \delta[T] * X[t-1] 1{te \le t \le tf} +
#' (\sum[k=tf+1]^t \epsilon[k] + X'[tf]) 1{t > tf} + \epsilon[t] 1{t \le tf}}
#'
#' where the he autoregressive coefficient \eqn{\delta_T}{\delta[T]} is given by the formula
#'
#' \deqn{\delta_T = 1 + cT^{-a}}{\delta[T] = 1 + c*T^{-a}}
#'
#' with \eqn{c>0} and \eqn{\alpha \in (0,1)}{{\alpha}} in (0,1),
#' \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)} and \eqn{X_{\tau_f} = X_{\tau_e} + X^*}{X[tf] = X[te] + X'}.
#'
#' @return a numeric vector of length n
#' @export
#'
#' @seealso \code{\link{sim_dgp2}}, \code{\link{sim_blan}}, \code{\link{sim_evans}}
#'
#' @examples
#' # 100 periods with origination 40 and termination 55 (default values for te and tf)
#' sim_dgp1(100)
#'
#' # 200 periods with origination 80 and termination 110
#' sim_dgp1(200)
#'
#' # 200 periods with origination 100 and termination 150
#' sim_dgp1(n = 200, te = 100, tf = 150)
sim_dgp1 <- function(n, te = 0.4*n, tf = 0.15*n + te, c = 1, alpha = 0.6, sigma = 6.79) {

  is.positive.int(n)
  is.between(te, 0, n)
  is.between(tf, te, n)
  is.positive.int(c)
  is.between(alpha, 0, 1)
  stopifnot(sigma > 0)

  delta <-  1 + c*n^(-alpha)
  y <- 100

  for (i in 1:(n - 1)) {
    if (i < te) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if ( i >= te & i < tf) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    } else if ( i == tf) {
      y[i + 1] <- y[te]
    } else {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    }
  }
  return(y)
}


#' Simulation of a two-bubble proccess
#'
#' The following generating process is simular to the simulation of the single-bubble process \code{\link{sim_dgp1}}.
#' The generating process switches betweem a martingale mechanism, the first mildy explosive episode, collapse,
#' renewal of martingale behavior, the second mildly explosive episode, another collapse and the subsequent renewal
#' of martingale behavior.
#'
#' @param n a positive integer indicating the number of simulations
#' @param te1 a value dating the originatiopn of the first bubble epsiode
#' @param tf1 a value dating the termination of the first bubble episode
#' @param te2 a value dating the origination of the second bubble episode
#' @param tf2 a value dating the termination of the second bubble episode
#' @param c a positive numeric value determining the value of the constant in the autoregressive coeffcient
#' @param alpha a positve number determining the value of the expansion rate in the autoregressive coefficient
#' @param sigma a posiitve number indicating the standard deviation of the innovations
#'
#' @details
#' The data generating process can be described with the following equation:
#' \deqn{X_t = X_{t-1}1\{t \in N_0\}+ \delta_T X_{t-1}1\{t \in B_1 \cup B_2\} +
#' \left(\sum_{k=\tau_{1f}+1}^t \epsilon_k + X^*_{\tau_{1f}}\right) 1\{t \in N_1\} }{X[t]=X[t-1]
#' 1{t in N[0]}+ \delta[T] * X[t-1] 1{t in B[1] union B[2]} +
#' (\sum[k=t1f+1]^t \epsilon[k] + X'[t1f]) 1{t in N[1]} +
#' }
#'
#' \deqn{ + \left(\sum_{l=\tau_{2f}+1}^t \epsilon_l + X^*_{\tau_{2f}}\right) 1\{t \in N_2\} +
#' \epsilon_t 1\{t \in N_0 \cup B_1 \cup B_2\}}{(\sum[l=t2f+1]^t \epsilon[l] + X'[t2f]) 1{t in N[2]} +
#' \epsilon[t] 1{t in N[0] union B[1] union B[2]}}
#'
#' where the he autoregressive coefficient \eqn{\delta_T}{\delta[T]} is given by the formula
#'
#' \deqn{\delta_T = 1 + cT^{-a}}{\delta[T] = 1 + c*T^{-a}}
#'
#' with \eqn{c>0} and \eqn{\alpha \in (0,1)}{\alpha} in (0,1),
#' \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)} and \eqn{X_{\tau_f} = X_{\tau_e} + X^*}{X[tf] = X[te] + X'}.
#'
#' @return a numeric vector of length n
#' @export
#'
#' @examples
#' sim_dgp2(100)
#'
#'
sim_dgp2 <- function(n, te1 = 0.2*n, tf1 = 0.2*n + te1, te2 = 0.6*n, tf2 = 0.1*n + te2,
                     c = 1, alpha = 0.6, sigma = 6.79){

  is.positive.int(n)
  is.between(te1, 0, n)
  is.between(tf1, te1, n)
  is.between(te2, tf1, n)
  is.between(tf2, te2, n)
  is.between(alpha, 0, 1)
  stopifnot(sigma>0)

  delta <-  1 + c*n^(-alpha)
  y <- 100

  for (i in 1:(n - 1)) {
    if (i < te1) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if ( i >= te1 & i < tf1) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    } else if (i == tf1) {
      y[i + 1] <-  y[te1]
    } else if (i > tf1 & i < te2) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if (i >= te2 & i < tf2) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    } else if (i == tf2) {
      y[i + 1] <-  y[te2]
    } else {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    }
  }

  return(y)
}

#' Simulation of Blanchard Bubble
#'
#' @param n a positive integer indicating the number of simulations
#' @param pi a positive value in (0, 1) indicating the probability of collapse
#' @param sigma the standard deviation of the innovations
#' @param r a positive value describing the exapnsion rate
#'
#' @export
#' @return a numeric vector of length n
#'
#' @details
#' Blanchard's Bubble process has two regimes, which occur with probability \eqn{\pi} and \eqn{1-\pi}.
#' In the first regime, the bubble grows expontentially at the rate \eqn{(1+r)\pi}, whereas in the
#' second regime, the bubble collapses to a white noise.
#'
#' With probability \eqn{\pi}
#' \deqn{B_{t+1} = \frac{1+r}{\pi}B_t+\epsilon_{t+1}}{B[t+1]=(1+r)/\pi*B[t]+\epsilon[t+1]}
#' With probability \eqn{\pi}
#' \deqn{B_{t+1} = \epsilon_{t+1}}{B[t+1] = \epsilon[t+1]}
#'
#' where r is a positive constant and \eqn{\epsilon \sim iid(0, \sigma^2)}{\epsilon - iid(0, \sigma^2)}.
#' Taking expectations on both sides yields
#'
#' \deqn{E_t[B_{t+1}] = (1+r)B_t}{E[B[t+1]]= (1+r)*B[t]}
#'
#' where \eqn{E_t}{E} is the expectation operator. The expected growth rate of the bubble is (1+r).
#'
#' @references Blanchard, O. J. (1979). Speculative bubbles, crashes and rational expectations.
#' Economics letters, 3(4), 387-389.
sim_blan <- function(n, pi = 0.7, sigma = 0.03, r = 0.05){
  b <- 1
  theta <- rbinom(n, 1, pi)
  i <- 1
  while (i < n) {
    if (b[i] > 0) {
      if (theta[i] == 1) {
        b[i + 1] <- (1 + r)/pi*b[i] + rnorm(1, 0, sigma)
      }else{
        b[i + 1] <- rnorm(1, 0, sigma)
      }
      i <- i + 1
    }else{
      i <- i - 1
    }
  }
  return(b)
}


#' Simulation of Evans Bubble
#'
#' The following generating process simulates a rational bubble that is always positive and collapses periodically.
#'
#' @param n a positive integer indicating the number of simulations
#' @param delta positive value
#' @param tau the standard deviation of the innovations
#' @param pi a positive value in (0, 1) indicating the probability of collapse
#' @param r a positive value describing the exapnsion rate
#' @param alpha positive
#'
#' @return a numeric vector of length n
#'
#' @details
#' If \eqn{B_t \leq \alpha}{B[t] \le \alpha}
#'
#' \deqn{B_{t+1} =  (1+r) B_t u_{t+1}}{B[t+1]= (1+r)*B[t]*u[t+1]}
#'
#' If \eqn{B_t > \alpha}{B[t] > \alpha}
#'
#' \deqn{B_{t+1} =  \delta + (1+r)\pi^{-1} \theta_{t+1}(B_t -  (1+r)^{-1}\delta B_t )u_{t+1}}{B[t+1] =
#' \delta*(1+r)/\pi* (B[t]-\delta/(1+r))*u[t+1]}
#'
#' @export
sim_evans <- function(n, alpha = 1, delta = 0.5, tau = 0.05, pi = 0.7, r = 0.05){

  stopifnot(alpha > 0)
  if (delta < 0 & delta > (1 + r)*alpha) stop("Arguemnt delta should be 0 < delta < (1+r)*alpha")


  y <- rnorm(n, 0, tau)
  u <- exp(y - tau^2/2)
  theta <- rbinom(n, 1, pi)
  b <- delta

  for (i in 1:(n - 1)) {
    if (b[i] <= alpha) {
      b[i + 1] <- (1 + r)*b[i]*u[i + 1]
    }else{
      b[i + 1] <- (delta + pi^(-1)*(1 + r)*theta[i + 1]*(b[i] - (1 + r)^(-1)*delta) )*u[i + 1]
    }
  }
  return(b)
}

#' Simulation of dividends
#'
#' The follow data generating process assumes a random walk with drift. Dividens can follow either a
#' normal or lognormal distribution.
#'
#' @param n a positive integer indicating the number of simulations
#' @param mu a value inidcating the drift
#' @param sigma a positive value inidcating the standard deviation of the the white noise
#' @param r a positive value indicationg the expansion of the bubble
#' @param log a logical. If true dividends follow a lognormal distribution
#' @param output a character vector. Is set to either return fundamental price('pf') or dividend series('d')
#'
#' @return a numeric vector of length n
#' @export
#'
#' @details
#'
#' If log is set to FALSE (default value) the dividends follow:
#'
#' \deqn{d_t = \mu + d_{t-1} + \epsilon_t}{d[t] = \mu + d[t-1] + \epsilon[t]}
#'
#' where \eqn{\epsilon \sim \mathcal{N}(0, \sigma^2)}{\epsilon - N(0, \sigma^2)}. The default parameters
#' are set \eqn{\mu = 0.0373}, \eqn{\sigma^2 = 0.1574} and \eqn{d[0] = 1.3} (initval). The above
#' equation can be solved to yield the fundamental price:
#'
#' \deqn{F_t = \mu(1+r)r^{-2} + r^{-1}d_t}{F[t] = \mu * (1 + r)/r^2 + d[t]/r}
#'
#' If log is set to TRUE then dividends follow a lognormal distribution or log(dividends) follow:
#'
#'\deqn{\ln(d_t) = \mu + \ln(d_{t-1}) + \epsilon_t}{ln(d[t]) = \mu + ln(d[t-1]) + \epsilon[t]}
#'
#'where \eqn{\epsilon \sim \mathcal{N}(0, \sigma^2)}{\epsilon - N(0, \sigma^2)}. The parameters
#'\eqn{\mu = 0.013}, \eqn{\sigma^2 = 0.16}. The fundamental price for this case:
#'
#'\deqn{F_t = \frac{1+g}{r-g}d_t}{F[t] = (1 + g)/(r -g) * d[t]}
#'
#'where \eqn{1+g=\exp(\mu+\sigma^2/2)}{1 + g = exp(\mu + \sigma^2/2)}. All of the parameter values are
#'obtained from West(1988).
#'
#' @references West, K. D. (1988). Dividend innovations and stock price volatility.
#' Econometrica: Journal of the Econometric Society, 37-61.
#'
#' @examples
#' # Price is the sum of bubble component and fundamental component
#' # 20 is the scaling factor
#' pf <- sim_div(100, r = 0.05, output = "pf")
#' pb <- sim_evans(100, r = 0.05)
#' p <- pf + 20*pb
sim_div <- function(n, mu, sigma, r = 0.05, log = FALSE, output = c("pf","d")){


  initval <- 1.3
  # Values obtained from West(1988, p53)
  if (missing(mu)) if (log) mu = 0.013 else mu = 0.0373
  if (missing(sigma)) if (log) sigma = sqrt(0.16) else sigma = sqrt(0.1574)

  is.positive.int(n)
  stopifnot(sigma > 0)
  stopifnot(r > 0)
  stopifnot(is.logical(log))
  return <- match.arg(output)

  d <- stats::filter(mu + c(initval, rnorm(n - 1 , 0, sigma)),
                               c(1), init = 1.3, method = "recursive")

  if (log) {
    g <- exp(mu + sigma^2/2) - 1
    pf <- (1 + g)*d/(r - g)
  }else{
    pf <- mu*(1 + r)*r^(-2) + d/r
  }

  if (return == "pf") {
    return(pf)
  }else{
    return(d)
  }
}

