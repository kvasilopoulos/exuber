#' Wild Bootstrap Critical values
#'
#' \code{wb_cv} performs the Harvey et al. (2016) wild bootstrap re-sampling
#' scheme, which is asymptotically robust to non-stationary volatility, to
#' generate critical values for the recursive unit root tests.
#'
#' @param y A data.frame or matrix containing the data.
#' @param nboot A positive integer indicating the number of bootstraps. Default is 1000 repetitions.
#' @inheritParams mc_cv
#' @param dist_rad Logical. If \code{TRUE} then  the Rademacher distribution
#' will be used.
#'
#' @return  A list that contains the critical values for ADF, BADF, BSADF and GSADF
#' t-statistics.
#'
#' @details This approach involves applying a wild bootstrap re-sampling scheme
#' to construct the bootstrap analogue of the PWY test which is asymptotically
#' robust to non-stationary volatility.
#'
#' @references Harvey, D. I., Leybourne, S. J., Sollis, R., & Taylor, A. M. R.
#' (2016). Tests for explosive financial bubbles in the presence of
#' non-stationary volatility. Journal of Empirical Finance, 38(Part B), 548-574.
#'
#' @seealso \code{\link{mc_cv}} for Monte Carlo critical values
#'
#' @import doParallel
#' @import parallel
#' @import foreach
#' @importFrom utils setTxtProgressBar txtProgressBar flush.console
#' @importFrom stats rnorm quantile
#' @export
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100))
#'
#' # Default minimum window
#' wb <- wb_cv(y = dta)
#'
#' # Change the minimum window and the number of bootstraps
#' wb <- wb_cv(y = dta, nboot = 1500,  minw = 20)
#'
#' # Use parallel computing (utilizing all available cores)
#' wb <- wb_cv(y = dta, parallel = TRUE)
#' }
wb_cv <- function(y, nboot = 1000, minw, parallel = FALSE, ncores,
                  dist_rad = FALSE) {
  y <- as.matrix(y)
  nc <- NCOL(y)
  nr <- NROW(y)

  is.positive.int(nboot)
  if (missing(minw)) {
    r0 <- 0.01 + 1.8 / sqrt(nr)
    minw <- floor(r0 * nr)
  } else if (!minw == round(minw) | minw <= 0) {
    stop("Argument 'minw' should be a positive integer", call. = FALSE)
  } else if (minw < 3) {
    stop("Argument 'minw' is too small", call. = FALSE)
  }

  stopifnot(is.logical(parallel))
  stopifnot(is.logical(dist_rad))
  if (any(is.na(y))) {
    stop("Recursive least square estimation cannot handle NA", call. = FALSE)
  }

  if (missing(ncores)) {
    ncores <- detectCores()
  }else{
    if (!parallel) {
      stop("Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'",
           call. = FALSE)
    }
  }

  is.between(ncores, 2, detectCores())
  adf_critical <- matrix(NA,
    nrow = nc, ncol = 3,
    dimnames = list(colnames(y), c("90%", "95%", "95%"))
  )
  sadf_critical <- matrix(NA,
    nrow = nc, ncol = 3,
    dimnames = list(colnames(y), c("90%", "95%", "95%"))
  )
  gsadf_critical <- matrix(NA,
    nrow = nc, ncol = 3,
    dimnames = list(colnames(y), c("90%", "95%", "95%"))
  )
  bsadf_critical <- array(NA,
    dim = c(nr - minw - 1, 3, nc),
    dimnames = list(
      NULL, c("90%", "95%", "95%"),
      colnames(y)
    )
  )
  badf_critical <- array(NA,
    dim = c(nr - minw - 1, 3, nc),
    dimnames = list(
      NULL, c("90%", "95%", "95%"),
      colnames(y)
    )
  )
  results <- matrix(0, nrow = 2 * nr + 1, ncol = nboot)


  pb <- txtProgressBar(max = nboot, style = 3)

  if (parallel) {

    f <- function(){
      count <- 0
      function(...) {
        count <<- count + length(list(...)) - 1
        setTxtProgressBar(pb, count)
        flush.console()
        cbind(...)
      }
    }

    cl <- makeCluster(ncores, type = 'PSOCK')
    registerDoParallel(cl)

    for (j in 1:nc) {
      dy <- y[-1, j] - y[-nr, j]
      results <- foreach(
        i = 1:nboot, .export = "srls_gsadf_cpp",
        .combine = f()
      ) %dopar% {
        ystar <- 0
        if (dist_rad) {
          w <- sample(c(-1, 1), nr - 1, replace = TRUE)
        } else {
          w <- rnorm(nr - 1, 0, 1)
        }
        ystar <- c(0, cumsum(w * dy))
        srls_gsadf_cpp(ystar[-1], ystar[-nr], minw)
      }

      bsadf_critical[, , j] <- t(apply(results[(1 + minw):(nr - 1), ], 1,
        quantile,
        prob = c(0.9, 0.95, 0.99)
      ))
      sadf_critical[j, ] <- quantile(results[nr, ],
        prob = c(0.9, 0.95, 0.99)
      )
      gsadf_critical[j, ] <- quantile(results[nr + 1, ],
        prob = c(0.9, 0.95, 0.99)
      )
      adf_critical[j, ] <- quantile(results[nr + 2, ],
        prob = c(0.9, 0.95, 0.99)
      )
      badf_critical[, , j] <- t(apply(results[-c(1:(nr + 2 + minw)), ], 1,
        quantile,
        prob = c(0.9, 0.95, 0.99)
      ))
      cat("\n", paste("Series", j, "out of", nc, "completed!", sep = " "), "\n")
    }
    stopCluster(cl)
  } else {
    for (j in 1:nc) {
      dy <- y[-1, j] - y[-nr, j]
      for (i in 1:nboot) {
        ystar <- 0
        if (dist_rad) {
          w <- sample(c(-1, 1), nr - 1, replace = TRUE)
        } else {
          w <- rnorm(nr - 1, 0, 1)
        }
        ystar <- c(0, cumsum(w * dy))
        results[, i] <- srls_gsadf_cpp(ystar[-1], ystar[-nr], minw)
        setTxtProgressBar(pb, i)
      }
      bsadf_critical[, , j] <- t(apply(results[(1 + minw):(nr - 1), ], 1,
        quantile,
        prob = c(0.9, 0.95, 0.99)
      ))
      sadf_critical[j, ] <- quantile(results[nr, ],
        probs = c(0.9, 0.95, 0.99)
      )
      gsadf_critical[j, ] <- quantile(results[nr + 1, ],
        prob = c(0.9, 0.95, 0.99)
      )
      adf_critical[j, ] <- quantile(results[nr + 2, ],
        prob = c(0.9, 0.95, 0.99)
      )
      badf_critical[, , j] <- t(apply(results[-c(1:(nr + 2 + minw)), ], 1,
        quantile,
        prob = c(0.9, 0.95, 0.99)
      ))
      cat("\n")
      print(paste("Series", j, "out of", nc, "completed!", sep = " "),
        quote = FALSE
      )
    }
  }
  close(pb)

  output <- list(
    adf_cv = adf_critical,
    sadf_cv = sadf_critical,
    gsadf_cv = gsadf_critical,
    badf_cv = badf_critical,
    bsadf_cv = bsadf_critical
  )

  attr(output, "class") <- append(class(output), "cv")
  attr(output, "iter") <- nboot
  attr(output, "method") <- "Wild Bootstrap"
  attr(output, "minw") <- minw

  return(output)
}
