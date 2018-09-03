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
<<<<<<< HEAD
#' @import doParallel
#' @import doSNOW
#' @import parallel
#' @import foreach
#' @importFrom utils setTxtProgressBar txtProgressBar flush.console
=======
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom utils setTxtProgressBar txtProgressBar
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
#' @importFrom stats quantile rnorm
#' @importFrom lubridate is.Date
#' @importFrom purrr detect_index
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

<<<<<<< HEAD
  assert_positive_int(n)
  assert_positive_int(nrep)
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
    ncores <- detectCores() - 1
  }else{
    if (!parallel) {
      stop("Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'",
           call. = FALSE)
    }
  }

  pb <- txtProgressBar(min = 1, max = nrep - 1, style = 3)

  if (parallel) {
    cl <- parallel::makeCluster(ncores, type = 'PSOCK')
    on.exit(parallel::stopCluster(cl))
    registerDoSNOW(cl)

    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    results <- foreach(
      i = 1:nrep, .export = "srls_gsadf_cpp",
      .combine = "cbind", .options.snow = opts
    ) %dopar% {
      y <- cumsum(rnorm(n))
      srls_gsadf_cpp(y[-1], y[-n], minw)
    }
=======
  # args
  if (missing(minw)) minw <-  floor((r0 <- 0.01 + 1.8 / sqrt(n)) * n)
  warning_redudant(ncores, cond = !missing(ncores) && !parallel)
  if (missing(ncores)) ncores <- detectCores() - 1
  # checks
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  assert_positive_int(minw, greater_than = 2)
  stopifnot(is.logical(parallel))
  # helpers
  point <- n - minw
  pb <- txtProgressBar(min = 1, max = nrep - 1, style = 3)


  if (parallel) {
    cl <- parallel::makeCluster(ncores, type = 'PSOCK')
    on.exit(parallel::stopCluster(cl))
    # use DoSNOW for pb in parallel
    registerDoSNOW(cl)

    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    results <- foreach(i = 1:nrep, .export = c("rls_gsadf","unroot"),
                       .combine = "cbind", .options.snow = opts) %dopar% {
                         y <- cumsum(rnorm(n))
                         yxmat <- unroot(y)
                         rls_gsadf(yxmat, min_win = minw)}
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
  } else {
    # preallocation required
    results <- matrix(0, 2 * point + 3, nrep)
    for (i in 1:nrep) {
      y <- cumsum(rnorm(n))
      yxmat <- unroot(y)
      setTxtProgressBar(pb, i)
      results[, i] <- rls_gsadf(yxmat, min_win = minw)
    }
  }
  close(pb)

<<<<<<< HEAD
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



  bsadf_critical_adj <- apply(bsadf_critical, 2, cummax)
  badf_critical_adj <- apply(badf_critical, 2, cummax)

  output <- structure(list(adf_cv = adf_critical,
                           sadf_cv = sadf_critical,
                           gsadf_cv = gsadf_critical,
                           badf_cv = badf_critical_adj,
                           bsadf_cv = bsadf_critical_adj),
                      method = "Monte Carlo",
                      iter   = nrep,
                      minw   = minw,
                      class  = "cv")

  # attr(output, "class") <- append(class(output), "cv")
  # attr(output, "iter") <- nrep
  # attr(output, "method") <- "Monte Carlo"
  # attr(output, "minw") <- minw
=======
  pr <- c(0.9, 0.95, 0.99)
  badf_crit <- apply(results[1:point, ], 1, quantile, probs = pr)
  badf_crit_adj <- apply(t(badf_crit), 2, cummax)
  adf_crit <- quantile(results[point + 1, ], probs = pr, drop = FALSE)
  sadf_crit <- quantile(results[point + 2, ], probs = pr, drop = FALSE)
  gsadf_crit <- quantile(results[point + 3, ], probs = pr, drop = FALSE)
  bsadf_crit <- apply(results[-c(1:(point + 3)), ], 1, quantile, probs = pr)
  bsadf_crit_adj <- apply(t(bsadf_crit), 2, cummax)


  output <- structure(list(adf_cv = adf_crit,
                           sadf_cv = sadf_crit,
                           gsadf_cv = gsadf_crit,
                           badf_cv = badf_crit_adj,
                           bsadf_cv = bsadf_crit_adj),
                      method = "Monte Carlo",
                      iter   = nrep,
                      minw   = minw,
                      class  = "cv")
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods

  return(output)
}
