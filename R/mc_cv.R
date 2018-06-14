#'  Monte Carlo Critical Values
#'
#' \code{mc_cv} computes Monte Carlo critical values for the recursive unit
#' root tests.
#'
#' @param n A positive integer. The sample size.
#' @param nrep A positive integer. The number of Monte Carlo simulations.
#' @inheritParams radf
#' @param parallel Logical. If \code{TRUE} parallel programming is used.
#' @param ncores A positive integer, optional. If `parallel' is set to
#' \code{TRUE}, then the user can specify the number of cores (defaults to
#' using all cores).
#'
#' @return A list that contains the critical values for ADF, BADF, BSADF and GSADF
#' t-statistics.
#'
#' @seealso \code{\link{wb_cv}} for Wild Bootstrapped critical values.
#'
#' @import doParallel
#' @import parallel
#' @import foreach
#' @importFrom utils setTxtProgressBar txtProgressBar flush.console
#' @importFrom stats quantile rnorm
#' @export
#'
#' @examples
#' \donttest{
#' # Default minimum window
#' mc <- mc_cv(n = 100)
#'
#' # Change the minimum window and the number of simulations
#' mc <- mc_cv(n = 100, nrep = 2500,  minw = 20)
#'
#' # Use parallel computing (utilizing all available cores)
#' mc <- mc_cv(n = 100, parallel = TRUE)
#' }
mc_cv <- function(n, nrep = 2000, minw, parallel = FALSE, ncores) {

  is.positive.int(n)
  is.positive.int(nrep)
  if (missing(minw)) {
    r0 <- 0.01 + 1.8 / sqrt(n)
    minw <- floor(r0 * n)
  } else if (!minw == round(minw) | minw <= 0) {
    stop("Argument 'minw' should be a positive integer", call. = FALSE)
  } else if (minw < 3) {
    stop("Argument 'minw' is too small", call. = FALSE)
  }
  stopifnot(is.logical(parallel))

  if (missing(ncores)) {
    ncores <- detectCores()
  }else{
    if (!parallel) {
      stop("Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'",
           call. = FALSE)
    }
  }
  is.between(ncores, 2, detectCores())

  pb <- txtProgressBar(min = 1, max = nrep - 1, style = 3)

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

    results <- foreach(
      i = 1:nrep, .export = "srls_gsadf_cpp",
      .combine = f()
    ) %dopar% {
      y <- cumsum(rnorm(n))
      srls_gsadf_cpp(y[-1], y[-n], minw)
    }
    stopCluster(cl)
  } else {
    results <- matrix(0, 2 * n + 1, nrep)
    for (i in 1:nrep) {
      y <- cumsum(rnorm(n))
      setTxtProgressBar(pb, i)
      results[, i] <- srls_gsadf_cpp(y[-1], y[-n], minw)
    }
  }
  close(pb)

  bsadf_critical <- t(apply(results[(minw + 1):(n - 1), ], 1, quantile,
                            probs = c(0.9, 0.95, 0.99)
  ))
  sadf_critical <- quantile(results[n, ],
                            probs = c(0.9, 0.95, 0.99),
                            drop = FALSE
  )
  gsadf_critical <- quantile(results[n + 1, ],
                             probs = c(0.9, 0.95, 0.99),
                             drop = FALSE
  )
  adf_critical <- quantile(results[n + 2, ],
                           probs = c(0.9, 0.95, 0.99),
                           drop = FALSE
  )
  badf_critical <- t(apply(results[-c(1:(n + 2 + minw)), ], 1, quantile,
                           probs = c(0.9, 0.95, 0.99)
  ))

  # cv sequences should be increasing - i could use cummax
  # for (j in 1:3) {
  #   for (i in 2:NROW(bsadf_critical)) {
  #     if (bsadf_critical[i, j] <= bsadf_critical[i - 1, j]) {
  #       bsadf_critical[i, j] <- bsadf_critical[i - 1, j]
  #     }
  #     if (badf_critical[i, j] <= badf_critical[i - 1, j]) {
  #       badf_critical[i, j] <- badf_critical[i - 1, j]
  #     }
  #   }
  # }
  # cv sequences should be increasing
  bsadf_critical_adj <- apply(bsadf_critical, 2, cummax)
  badf_critical_adj <- apply(badf_critical, 2, cummax)

  output <- list(
    adf_cv = adf_critical,
    sadf_cv = sadf_critical,
    gsadf_cv = gsadf_critical,
    badf_cv = badf_critical_adj,
    bsadf_cv = bsadf_critical_adj
  )

  attr(output, "class") <- append(class(output), "cv")
  attr(output, "iter") <- nrep
  attr(output, "method") <- "Monte Carlo"
  attr(output, "minw") <- minw

  return(output)
}
