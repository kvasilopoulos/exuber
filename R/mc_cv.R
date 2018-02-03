#' #' Monte Carlo Critical values
#'
#' \code{mc_cv} is used to compute
#'
#' @param n the number of simulated periods
#' @param nrep a positive integer, The number of simulations.
#' @param minw a non-negative integer. The minimum window
#' @param parallel logical. If TRUE parallel programming
#'
#' @return \code{mc_cv} a list. Contains the critical values for ADF, BADF, BSADF, GSADF t-statistics
#'
#' @import foreach
#' @import parallel
#' @import doSNOW
#' @export
#'
mc_cv <- function(n, nrep = 2000, minw, parallel = FALSE){

  if (!n == round(n) | n <= 0) {
    stop("'n' should be a positive integer")
  } else if (n < 7) {
    stop("'n' is too small")
  }

  if (missing(minw)) {
    r0 = 0.01 + 1.8 / sqrt(n)
    minw = floor(r0 * n)
  } else if (!minw == round(minw) & minw >= 0) {
    stop("Argument 'minw' should be an integer")
  }

  stopifnot(is.logical(parallel))

  pb <- txtProgressBar(max = nrep, style = 3)
  if (parallel) {
    cl <- makeCluster(detectCores(), type = "SOCK")
    registerDoSNOW(cl)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    results  <-  foreach(i = 1:nrep, .export = 'srls_gsadf', .combine = 'cbind',
                         .options.snow = opts) %dopar% {
                           y  <-  cumsum(rnorm(n))
                           srls_gsadf(y[-1], y[-n], winm = minw)
                         }
    stopCluster(cl)
  }else{
    results <- matrix(0, 2 * n + 1, nrep)
    for (i in 1:nrep) {
      y <- cumsum(rnorm(n))
      setTxtProgressBar(pb, i)
      results[, i] <- srls_gsadf(y[-1], y[-n], winm = minw)
    }
  }
  close(pb)

  bsadf_critical <-  t(apply(results[(minw + 1):(n - 1), ], 1, quantile,
                           probs =  c(0.9, 0.95, 0.99)))
  sadf_critical  <-  quantile(results[n, ], probs = c(0.9, 0.95, 0.99), drop = FALSE)
  gsadf_critical <-  quantile(results[n + 1, ], probs = c(0.9, 0.95, 0.99), drop = FALSE)
  adf_critical   <-  quantile(results[n + 2, ], probs =  c(0.9, 0.95, 0.99),drop = FALSE)
  badf_critical  <-  t(apply(results[-c(1:(n + 2 + minw)), ], 1, quantile,
                           probs =  c(0.9, 0.95, 0.99)))

  for (i in 2:length(bsadf_critical)) {
    if (bsadf_critical[i] <= bsadf_critical[i - 1]) {
      bsadf_critical[i] <- bsadf_critical[i - 1]
    }
    if (badf_critical[i] <= badf_critical[i - 1]) {
      badf_critical[i] <- badf_critical[i - 1]
    }
  }

  output <- list(adf_cv   = adf_critical,
                 sadf_cv  = sadf_critical,
                 gsadf_cv = gsadf_critical,
                 badf_cv  = badf_critical,
                 bsadf_cv = bsadf_critical,
                 info     = list( method = "Monte Carlo",
                                  iter = nrep, minw = minw))
  return(output)
}
