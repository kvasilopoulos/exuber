



mc_sim <- function(n, nrep = 2000, minw = psy_rule(n)) {

  if (!is_scalar_atomic(n))  # case of providiing data in 'n'
    stop("Argument 'n' should be a positive integer")

  assert_positive_int(n, greater_than = 5)
  assert_positive_int(nrep)
  assert_positive_int(minw, greater_than = 2)

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
  results
}


#' @rdname mc_cv
#' @inheritParams mc_cv
#' @export
mc_dist <- function(n, nrep = 2000, minw = psy_rule(n)) {

  results <- mc_sim(n = n, nrep = nrep, minw = minw)

  adf_crit <- results[n - minw + 1, ]
  sadf_crit <- results[n - minw + 2, ]
  gsadf_crit <- results[n - minw + 3, ]

  output <- structure(list(
    adf_cv = adf_crit,
    sadf_cv = sadf_crit,
    gsadf_cv = gsadf_crit
  ),
  method = "Monte Carlo",
  iter = nrep,
  minw = minw,
  class = "mc_dist"
  )

  return(output)
}


#'  Monte Carlo Critical Values
#'
#' \code{mc_cv} computes Monte Carlo critical values for the recursive unit
#' root tests. \code{mc_dist} computes the distribution.
#'
#' @inheritParams radf
#' @param n A positive integer. The sample size.
#' @param nrep A positive integer. The number of Monte Carlo simulations.
#' @param opt_bsadf Options for bsadf critical value calculation. "conventional"
#' corresponds to the max of the quantile of the simulated distribution, while
#' "conservative" corresponds to the quantile of the max which is more conservative
#' in nature, thus the name.
#' @param opt_badf Options for badf critical value calculation. "fixed" corresponds
#' to log(log(n*s))/100 rule, "asymptotic" to asymptotic critical values and
#' simulated to the monte carlo simulations.
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
#' \dontrun{
#' # Default minimum window
#' mc <- mc_cv(n = 100)
#'
#' # Change the minimum window and the number of simulations
#' mc <- mc_cv(n = 100, nrep = 2500, minw = 20)
#' }
mc_cv <- function(n, minw = psy_rule(n), nrep = 2000,
                  opt_badf = c("fixed", "asymptotic", "simulated"),
                  opt_bsadf = c("conventional", "conservative")) {

  opt_badf <- match.arg(opt_badf)
  opt_bsadf <- match.arg(opt_bsadf)

  pr <- c(0.9, 0.95, 0.99)

  results <- mc_sim(n = n, nrep = nrep, minw = minw)

  adf_crit <- quantile(results[n - minw + 1, ], probs = pr, drop = FALSE)
  sadf_crit <- quantile(results[n - minw + 2, ], probs = pr, drop = FALSE)
  gsadf_crit <- quantile(results[n - minw + 3, ], probs = pr, drop = FALSE)

  bsadf_crit <-
    if (opt_bsadf == "conventional") {
      apply(results[-c(1:(n - minw + 3)), ], 1, quantile, probs = pr) %>%
        t() %>%
        apply(2, cummax)
    } else if (opt_bsadf == "conservative") {
      apply(results[1:(n - minw), ], 2, cummax) %>%
        apply(1, quantile, probs = pr) %>%
        t()
    }

  if (opt_badf == "fixed") {
    temp <- log(log(n * seq(minw + 1, n))) / 100
    badf_crit <- matrix(rep(temp, 3),
                        ncol = 3,
                        dimnames = list(NULL, paste(pr))
    )
  } else if (opt_badf == "asymptotic") {
    temp <- c(-0.44, -0.08, 0.6) %>% rep(each = 100) # values taken from PWY
    badf_crit <- matrix(temp, ncol = 3, dimnames = list(NULL, paste(pr))
    )
  } else if (opt_badf == "simulated") {
    badf_crit <- apply(results[1:point, ], 1, quantile, probs = pr) %>% t() %>%
      apply(2, cummax)
  }

  output <- structure(
    list(
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

