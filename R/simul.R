#' @export
sim_rw <- function(t, n = 1){
  y <- matrix(cumsum(rnorm(t*n)), t, n)
}

#' @export
sim_ar <- function(n, ar = 1.02, drift = 0.05) {
  p = 0
  for (i in 1:(n - 1)) {
    p[i + 1] <- drift + ar * p[i] + rnorm(1, 0, 1 - 0.1^2)
  }
  return(matrix(p))
}

# Values obtained from West(1988, p53)
sim_div <- function(n, drift, sigmasq, do, logdiv = FALSE){
  arg <- match.call(expand.dots = FALSE)

  if (logdiv) {
    if (missing(names(arg))) drift = 0.013; sigmasq = 0.16; do = 1.3
    cumsum(c(log(do), rnorm(n - 1, drift, sqrt(sigmasq))))
  }else{
    drift = 0.0373; sigmasq = 0.1574; do = 1.3
    cumsum(c(do, rnorm(n - 1, drift, sqrt(sigmasq))))
  }
}

# Blanchard model
sim_blan <- function(n, pi=0.5, sigmae=0.03, r=0.05){
  B <- 1
  theta <- rbinom(n, 1, pi)
  # theta <- sample(c(1,0),n,pi,1-pi)
  for (i in 1:(n - 1)) {
    if (theta[i] == 1) {
      B[i + 1] <- (1 + r)/pi*B[i] + rnorm(1, 0, sigmae)
    }else{
      B[i + 1] <- rnorm(1, 0, sigmae)
    }
  }
  return(B)
}

sim_evans <- function(n, delta = 0.5, tau = 0.5, pi = 0.7,
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
  B <- delta

  for (i in 1:(N - 1)) {
    if (B[i] <= alpha) {
      B[i + 1] <- (1 + r)*B[i]*u[i + 1]
    }else{
      B[i + 1] <- (delta + pi^(-1)*(1 + r)*theta[i + 1]*(B[i] - (1 + r)^(-1)*delta) )*u[i + 1]
    }
  }
  return(B)
}
