#' Title
#'
#' @param n
#' @param d
#' @param eta
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_rw <-  function(n, d = 1, eta = 1){
  y <- matrix(cumsum(rnorm(n, mean = d*n^(-eta))))
}

#' Title
#'
#' @param n
#' @param d
#' @param eta
#' @param omega
#' @param alpha
#' @param beta
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_het <- function(n, d = 1, eta = 1, omega = 30.69,
                    alpha = 0, beta = 0.61){
  h <- y <-  0
  for (i in 1:(n - 1)) {
    h[i + 1] <- omega + alpha*rnorm(1) + beta*h[i]
    y[i + 1]  <-  d*n^(-eta) + y[i] + exp(h[i + 1]/2)*rnorm(1)
  }
  return(y)
}


#' Title
#'
#' @param n
#' @param ar
#' @param drift
#'
#' @return
#' @export
#'
#' @examples
sim_ar <- function(n, ar = 1.02, drift = 0.05) {
  p = 0
  for (i in 1:(n - 1)) {
    p[i + 1] <- drift + ar * p[i] + rnorm(1, 0, 1 - 0.1^2)
  }
  return(matrix(p))
}

#' One Bubble
#'
#' @param n
#' @param ci
#' @param alpha
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
sim_dgp1 <- function(n, te = 0.4*n, tf = 0.15*n + te, ci = 1, alpha =0.6, sigma = 6.79) {

  # te <-  0.4*n        # origin
  # tf <-  0.15*n + te # duration
  y <- 100
  delta <-  1 + ci*n^(-alpha)
  ind <- rep(0, n); ind[tf + 1] <- 1

  for (i in 1:(n - 1)) {
    if (i < te) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if ( i >= te & i <= tf) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    }
  }
  ystar <- y[te] + cumsum(rnorm(n - tf - 1, sd = sigma))
  y <- c(y, ystar)
  return(y)
}


#' Two Bubbles
#'
#' @param n a positive ingefer indicating the nubmer of simulations
#' @param te1 a positive integer inidcating the origin of the first bubble
#' @param tf1 a positive integer inidcating the termination of the first bubble
#' @param te2 a positive integer inidcating the origin of the second bubble
#' @param tf2 a positive integer inidcating the termination of the second bubble
#' @param ci a positve integer determining the value of the constant
#' @param alpha a positve integer determining the value of the constant
#' @param sigma a posiitve origin indicatin the standard deviation of the innovations
#'
#' @return
#' @export
#'
#' @examples
sim_dgp2 <- function(n, te1 = 0.2*n, tf1 = 0.2*n + te1, te2 = 0.6*n, tf2 = 0.1*n + te2,
                     ci =1, alpha = 0.6, sigma =6.79){

  # te1 <- 0.2*n
  # tf1 <- 0.2*n + te1
  # te2 <- 0.6*n
  # tf2 <- 0.1*n + te2
  delta <-  1 + ci*n^(-alpha)
  y <- 100

  ### make it everything inside a loop
  for (i in 1:(tf1 - 1)) {
    if (i < te1) {
      y[i + 1] <- y[i] + rnorm(1, sd = sigma)
    } else if ( i >= te1 & i <= tf1) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
    }
  }
  ystar1 <- y[te1] + cumsum(rnorm(te2 - tf1 - 1, sd = sigma))
  y <- c(y, ystar1)

  for (i in (te2 - 1):tf2) {
      y[i + 1] <- delta * y[i] + rnorm(1, sd = sigma)
  }

  ystar2 <- y[te2] + cumsum(rnorm(n - tf2 - 1, sd = sigma))
  y <- c(y, ystar2)
  return(y)


}
#' Title
#'
#' @param n
#' @param pi
#' @param sigma
#' @param r
#' @param initval
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_blan <- function(n, pi = 0.7, sigma = 0.03, r = 0.05, initval = 1){
  b <- initval
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


#' Title
#'
#' @param n
#' @param delta
#' @param tau
#' @param pi
#' @param r
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
sim_evans <- function(n, delta = 0.5, tau = 0.05, pi = 0.7,
                      r = 0.05, alpha = 1){
  if (alpha < 0) {
    stop("Argument 'alpha' should be positive!")
  }
  if (delta < 0 & delta > (1 + r)*alpha) {
    stop("Arguemnt delta should be 0 < delta < (1+r)*alpha")
  }
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

#' Title
#'
#' @param n
#' @param drift
#' @param sigma
#' @param r
#' @param initval
#' @param log
#' @param return
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_div <- function(n, drift, sigma, r = 0.05, initval = 1.3,
                    log = FALSE, return = c("pf","d")){

  return <- match.arg(return)
  # Values obtained from West(1988, p53)
  if (missing(drift)) if (log) drift = 0.013 else drift = 0.0373
  if (missing(sigma)) if (log) sigma = sqrt(0.16) else sigma = sqrt(0.1574)

  d <- stats::filter(drift + c(initval, rnorm(n - 1 , 0, sigma)),
                               c(1), init = initval, method = "recursive")

  if (log) {
    g <- exp(drift + sigma^2/2) - 1
    pf <- (1 + g)*d/(r - g)
  }else{
    pf <- drift*(1 + r)*r^(-2) + d/r
  }
  if (return == "pf") {
    return(pf)
  }else{
    return(d)
  }
}

