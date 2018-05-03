shade_b <- function(x,y) {
  b <- e <- matrix(ncol = )
  for (i in seq_along(col_names(x))) {
    v1 <- which(x$bsadf[, i] > y$badf_cv[, 2])
    b[,i] <- v1[c(TRUE,diff(v1) != 1)]
    e[,i] <- v1[c(diff(v1) != 1, TRUE)]
  }
  return(cbind(b,e))
}

# Simple recursive linear regression  -------------------------------------


srls_gsadf <- function(yy , xx, winm){

  end   <- length(yy)
  start <- winm
  total <- end - winm + 1
  tstat <- matrix(data = -999, nrow = end, ncol = total)

  for (j in 1:total) {
    for (i in j:total) {
      x = xx[j:(start + i - 1)]
      y = yy[j:(start + i - 1)]
      N <- winm + i - j
      if (i == j) {
        Sx  <- sum(x)
        Sy  <- sum(y)
        Sxx <- sum(x * x)
        Sxy <- sum(x * y)
      }else{
        Sx  <- Sx + x[N]
        Sy  <- Sy + y[N]
        Sxx <- Sxx + x[N] * x[N]
        Sxy <- Sxy + x[N] * y[N]
      }
      meanx <- Sx/N
      meany <- Sy/N
      den <- Sxx/N - meanx * meanx
      b <- (Sxy/N - meanx * meany)/den
      alpha <- meany - b * meanx;
      u <- y - alpha - b * x
      sqres <- sum(u * u)
      sb  <- sqrt(sqres/(N - 2)/den/N)
      tstat[winm + i - 1, j]  <-  (b - 1)/sb
    }
  }
  adf   <- tstat[end, 1]
  badf  <- tstat[, 1]
  sadf  <- max(badf)
  bsadf <- apply(tstat, 1, max)
  gsadf <- max(bsadf)
  return(c(bsadf, sadf, gsadf, adf, badf))
}


# Simulation --------------------------------------------------------------

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

