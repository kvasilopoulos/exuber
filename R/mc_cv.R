#'  Monte Carlo Critical Values
#'
#' \code{mc_cv} computes Monte Carlo critical values for the recursive unit
#' root tests.
#'
#' @param n A positive integer. The sample size.
#' @param nrep A positive integer. The number of Monte Carlo simulations.
#' @inheritParams radf
#' @param opt_badf the option for badf
#'
#' @return A list that contains the critical values for ADF, BADF, BSADF and GSADF
#' t-statistics.
#'
#' @seealso \code{\link{wb_cv}} for Wild Bootstrapped critical values and
#' \code{\link{sb_cv}} for Sieve Bootstrapped critical values
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar% %do%
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
#' }
mc_cv <- function(n, nrep = 2000, minw,
                  opt_badf = c("fixed", "asymptotic", "simulated"),
                  opt_bsadf = c("conventional", "conservative")) {

  if (missing(minw)) minw <- floor((0.01 + 1.8 / sqrt(n)) * n)
  opt_badf <- match.arg(opt_badf)
  opt_bsadf <- match.arg(opt_bsadf)

  # asserts
  if (is.data.frame(n) || is.matrix(n)) { # case of providiing data in 'n'
    stop("Argument 'n' should be a positive integer")
  }
  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  assert_positive_int(minw, greater_than = 2)

  # helpers
  point <- n - minw
  pr <- c(0.9, 0.95, 0.99)

  # get Options
  show_pb <- getOption("exuber.show_progress")
  do_par <- getOption("exuber.parallel")

  if (show_pb) {
    pb <- txtProgressBar(min = 1, max = nrep - 1, style = 3)
    opts <- list(progress = function(n) setTxtProgressBar(pb, n))
    on.exit(close(pb))
  } else {
    opts <- list(progress = NULL)
  }

  if (do_par) {
    cl <- parallel::makeCluster(getOption("exuber.ncores"), type = "PSOCK")
    on.exit(parallel::stopCluster(cl))
    registerDoSNOW(cl)
  }

  `%fun%` <- if (do_par) `%dopar%` else `%do%`

  results <- foreach(
    i = 1:nrep,
    .export = c("rls_gsadf", "unroot"),
    .combine = "cbind",
    .options.snow = opts
  ) %fun% {
    if (show_pb && !do_par) setTxtProgressBar(pb, i)
    y <- cumsum(rnorm(n))
    yxmat <- unroot(y)
    rls_gsadf(yxmat, min_win = minw)
  }


  adf_crit <- quantile(results[point + 1, ], probs = pr, drop = FALSE)
  sadf_crit <- quantile(results[point + 2, ], probs = pr, drop = FALSE)
  gsadf_crit <- quantile(results[point + 3, ], probs = pr, drop = FALSE)

  bsadf_crit <-
    if (opt_bsadf == "conventional") {
      apply(results[-c(1:(point + 3)), ], 1, quantile, probs = pr) %>%
        t() %>%
        apply(2, cummax)
    }else{
      apply(results[1:point, ], 2, cummax) %>%
        apply(1, quantile, probs = pr) %>%
        t()
    }

  if (opt_badf == "fixed") {
    temp <- log(log(n * seq(minw + 1, n))) / 100
    badf_crit <- matrix(rep(temp, 3),
      ncol = 3,
      dimnames = list(NULL, c(paste(pr)))
    )
  } else if (opt_badf == "asymptotic") {
    temp <- c(-0.44, -0.08, 0.6) %>% rep(each = 100) # values taken from PWY
    badf_crit <- matrix(temp,
      ncol = 3,
      dimnames = list(NULL, c(paste(pr)))
    )
  } else if (opt_badf == "simulated") {
    badf_crit <- apply(results[1:point, ], 1, quantile, probs = pr) %>%
      t() %>%
      apply(2, cummax)
  }

  output <- structure(list(
    adf_cv = adf_crit,
    sadf_cv = sadf_crit,
    gsadf_cv = gsadf_crit,
    badf_cv = badf_crit,
    bsadf_cv = bsadf_crit
  ),
  method = "Monte Carlo",
  opt_badf = opt_badf,
  opt_bsadf = opt_bsadf,
  iter = nrep,
  minw = minw,
  class = "cv"
  )

  return(output)
}
