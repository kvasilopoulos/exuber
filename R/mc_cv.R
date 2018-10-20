#'  Monte Carlo Critical Values
#'
#' \code{mc_cv} computes Monte Carlo critical values for the recursive unit
#' root tests.
#'
#' @param n A positive integer. The sample size.
#' @param nrep A positive integer. The number of Monte Carlo simulations.
#' @inheritParams radf
#' @param opt_badf the option for badf
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
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom utils setTxtProgressBar txtProgressBar
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
#'}
mc_cv <- function(n, nrep = 2000, minw,
                  opt_badf = c("fixed", "asymptotic", "simulated")) {

  if (missing(minw)) minw <-  floor((r0 <- 0.01 + 1.8 / sqrt(n)) * n)
  opt_badf <- match.arg(opt_badf)
  if (is.data.frame(n) || is.matrix(n))
    stop("Argument 'minw' should be a postive integer")
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  assert_positive_int(minw, greater_than = 2)

  # helpers
  point <- n - minw
  pr <- c(0.9, 0.95, 0.99)

  # get options
  show_pb <- getOption("exuber.show_progress")
  parallel <- getOption("exuber.parallel")
  ncores <- getOption("exuber.ncores")

  pb <- txtProgressBar(min = 1, max = nrep - 1, style = 3)

  if (parallel) {
    cl <- parallel::makeCluster(ncores, type = 'PSOCK')
    on.exit(parallel::stopCluster(cl))
    registerDoSNOW(cl) # use DoSNOW for pb in parallel
    progress <- if (show_pb)  function(n) setTxtProgressBar(pb, n) else NULL
    opts <- list(progress = progress)

    results <- foreach(i = 1:nrep, .export = c("rls_gsadf","unroot"),
                       .combine = "cbind", .options.snow = opts) %dopar% {
                         y <- cumsum(rnorm(n))
                         yxmat <- unroot(y)
                         rls_gsadf(yxmat, min_win = minw)}
  } else {
    # preallocation required for non-parallel estimation
    results <- matrix(0, 2 * point + 3, nrep)
    for (i in 1:nrep) {
      y <- cumsum(rnorm(n))
      yxmat <- unroot(y)
      if (show_pb) setTxtProgressBar(pb, i)
      results[, i] <- rls_gsadf(yxmat, min_win = minw)
    }
  }
  if (show_pb) close(pb)



  adf_crit <- quantile(results[point + 1, ], probs = pr, drop = FALSE)
  sadf_crit <- quantile(results[point + 2, ], probs = pr, drop = FALSE)
  gsadf_crit <- quantile(results[point + 3, ], probs = pr, drop = FALSE)
  bsadf_crit <- apply(results[-c(1:(point + 3)), ], 1, quantile, probs = pr) %>%
    t() %>% apply(2, cummax)

  if (opt_badf == "fixed") {
    temp <- log(log(n*seq(minw + 1, n)))/100
    badf_crit <- matrix(rep(temp, 3), ncol = 3,
                        dimnames = list(NULL, c(paste(pr))))
  } else if (opt_badf == "asymptotic") {
    temp <- c(-0.44, -0.08, 0.6) %>% rep(each = 100) # values taken from PWY
    badf_crit <- matrix(temp, ncol = 3,
                        dimnames = list(NULL, c(paste(pr))))
  }  else if (opt_badf == "simulated") {
    badf_crit <- apply(results[1:point, ], 1, quantile, probs = pr) %>%
      t() %>% apply(2, cummax)
  }


  output <- structure(list(adf_cv = adf_crit,
                           sadf_cv = sadf_crit,
                           gsadf_cv = gsadf_crit,
                           badf_cv = badf_crit,
                           bsadf_cv = bsadf_crit),
                      method = "Monte Carlo",
                      opt_badf = opt_badf,
                      iter   = nrep,
                      minw   = minw,
                      class  = "cv")

  return(output)
}
