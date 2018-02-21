#' Wild Bootstrapped Critical values
#'
#'\code{wb_cv} performs the Harvey et al. (2016) wild bootstrap re-sampling scheme which is asymptotically robust to
#'non-stationary volatility.
#'
#' @param y a data.frame or matrix containing the data.
#' @param nboot a positive integer idicating the number of bootstraps.
#' @param minw a non-negative integer indicating the minimum window.
#' @param distribution_rad a logical alue indicating whether Radstander distributions should bapplied for bootstraping.
#' @param parallel a logical value indicating whether parallel computing should be performd
#'
#' @return  a list that contains the critical values for ADF, BADF, BSADF, GSADF t-statistics.
#'
#' @references Harvey, D. I., Leybourne, S. J., Sollis, R., & Taylor, A. M. R. (2016). Tests for explosive financial bubbles
#' in the presence of non-stationary volatility. Journal of Empirical Finance, 38(Part B), 548–574.
#'
#' @seealso \code{\link{mc_cv}} for Monte Carlo simulation
#' @import foreach
#' @import parallel
#' @import doSNOW
#' @export
#'
wb_cv <- function(y, nboot = 1000, minw , distribution_rad = FALSE, parallel = FALSE){

  y  <- as.matrix(y)
  nc <- NCOL(y)
  nr <- NROW(y)

  stopifnot(is.logical(parallel))
  stopifnot(is.logical(distribution_rad))

  if (!nboot == round(nboot) | nboot <= 0) {
    stop("'n' should be a positive integer")
  }

  if (missing(minw)) {
    r0 = 0.01 + 1.8 / sqrt(nr)
    minw = floor(r0 * nr)
  } else if (!minw == round(minw) & minw >= 0) {
    stop("Argument 'minw' should be an integer")
  }

  adf_critical   <- matrix(NA, nrow = nc, ncol = 3,
                           dimnames = list(colnames(y), c("90%","95%","95%")))
  sadf_critical  <- matrix(NA, nrow = nc, ncol = 3,
                           dimnames = list(colnames(y), c("90%","95%","95%")))
  gsadf_critical <- matrix(NA, nrow = nc, ncol = 3,
                           dimnames = list(colnames(y), c("90%","95%","95%")))
  bsadf_critical <- array(NA, dim = c(nr - minw - 1, 3, nc),
                          dimnames = list(NULL, c("90%","95%","95%"), colnames(y)))
  badf_critical  <- array(NA, dim = c(nr - minw - 1, 3, nc),
                          dimnames = list(NULL, c("90%","95%","95%"), colnames(y)))
  results <- matrix(0, nrow = 2 * nr + 1, ncol = nboot)

  pb  <- txtProgressBar(max = nboot, style = 3)

  if (parallel) {
    cl <- makeCluster(detectCores(), type = "SOCK")
    registerDoSNOW(cl)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    for (j in 1:nc) {
      dy <- y[-1, j] - y[-nr, j]
      results <- foreach(i = 1:nboot, .export = 'srls_gsadf', .combine = 'cbind',
                         .options.snow = opts) %dopar% {
                           ystar <- 0
                           if (distribution_rad) {
                             w <- sample(c(-1, 1), nr - 1, replace = TRUE )
                           }else{
                             w <- rnorm(nr - 1, 0, 1)
                           }
                           ystar <- c(0, cumsum(w * dy))
                           srls_gsadf(ystar[-1], ystar[-nr], winm = minw)
                         }

      bsadf_critical[, , j] <- t(apply(results[(1 + minw):(nr - 1), ], 1, quantile,
                                   prob = c(0.9, 0.95, 0.99)))
      sadf_critical[j, ]   <- quantile(results[nr, ], prob = c(0.9, 0.95, 0.99))
      gsadf_critical[j, ]  <- quantile(results[nr + 1, ], prob = c(0.9, 0.95, 0.99))
      adf_critical[j, ]    <- quantile(results[nr + 2, ], prob = c(0.9, 0.95, 0.99))
      badf_critical[, , j] <- t(apply(results[-c(1:(nr + 2 + minw)), ], 1, quantile,
                                   prob = c(0.9, 0.95, 0.99)))
      cat("\n", paste("Series", j, "out of", nc, "completed!", sep = " "),"\n")
    }
    stopCluster(cl)
  }else {
    for (j in 1:nc) {
      dy <- y[-1, j] - y[-nr, j]
      for (i in 1:nboot) {
        ystar <- 0
        if (distribution_rad) {
          w <- sample(c(-1,1), nr - 1, replace = TRUE )
        }else{
          w <- rnorm(nr - 1, 0, 1)
        }
        ystar <- c(0, cumsum(w*dy))
        results[,i] <- srls_gsadf(ystar[-1] , ystar[-nr], winm = minw)
        setTxtProgressBar(pb, i)
      }
      bsadf_critical[,,j] <-  t(apply(results[(1 + minw):(nr - 1), ], 1, quantile,
                                    prob = c(0.9, 0.95, 0.99)))
      sadf_critical[j,  ]  <- quantile(results[nr, ], probs = c(0.9, 0.95, 0.99))
      gsadf_critical[j, ] <- quantile(results[nr + 1, ], prob = c(0.9, 0.95, 0.99))
      adf_critical[j, ]   <- quantile(results[nr + 2, ], prob = c(0.9, 0.95, 0.99))
      badf_critical[,,j]  <- t(apply(results[-c(1:(nr + 2 + minw)), ], 1, quantile,
                                   prob = c(0.9, 0.95, 0.99)))
      cat("\n")
      print(paste("Series", j,"out of", nc, "completed!", sep = " "), quote = FALSE)
    }
  }
  close(pb)

  output <- list(adf_cv   = adf_critical,
                 sadf_cv  = sadf_critical,
                 gsadf_cv = gsadf_critical,
                 badf_cv  = badf_critical,
                 bsadf_cv = bsadf_critical,
                 info     = list(method = "Wild Bootstrap",
                                 iter = nboot, minw = minw))
  return(output)
}
