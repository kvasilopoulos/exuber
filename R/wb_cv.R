#' Wild Bootstrap Critical values
#'
#' \code{wb_cv} performs the Harvey et al. (2016) wild bootstrap re-sampling
#' scheme, which is asymptotically robust to non-stationary volatility, to
#' generate critical values for the recursive unit root tests.
#'
#' @inheritParams radf
#' @inheritParams mc_cv
#' @param nboot A positive integer indicating the number of bootstraps. Default is 1000 repetitions.
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
#' @seealso \code{\link{mc_cv}} for Monte Carlo critical values and
#' \code{\link{sb_cv}} for Sieve Bootstrapped critical values
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats quantile rnorm
#' @export
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes
#' dta <- data.frame("dg1" = sim_dgp1(n = 100), "dgp2" = sim_dgp2(n = 100))
#'
#' # Default minimum window
#' wb <- wb_cv(dta)
#'
#' # Change the minimum window and the number of bootstraps
#' wb <- wb_cv(dta, nboot = 1500,  minw = 20)
#'
#' # Use parallel computing (utilizing all available cores)
#' wb <- wb_cv(dta, parallel = TRUE)
#' }
wb_cv <- function(data, minw, nboot = 1000, dist_rad = FALSE) {

  y <- data %>% rm_index() %>% as.matrix() # index-date check
  nc <- NCOL(y)
  nr <- NROW(y)

  if (missing(minw)) minw <- floor((0.01 + 1.8 / sqrt(nr)) * nr)

  # asserts
  assert_na(y)
  assert_positive_int(nboot, greater_than = 2)
  assert_positive_int(minw, greater_than = 2)
  stopifnot(is.logical(dist_rad))

  # helpers
  point <- nr - minw
  pr <- c(0.9, 0.95, 0.99)

  # preallocation
  results <- matrix(0, nrow = 2 * point + 3, ncol = nboot)
  adf_crit <- matrix(NA, nc, 3, dimnames = list(colnames(y), c(paste(pr))))
  sadf_crit <- gsadf_crit <- adf_crit
  badf_crit <- bsadf_crit <- array(NA,
    dim = c(point, 3, nc),
    dimnames = list(NULL, c(paste(pr)), colnames(y))
  )
  # get Options
  show_pb <- getOption("exuber.show_progress")
  do_par <- getOption("exuber.parallel")

  if (show_pb) {
    pb <- txtProgressBar(min = 1, max = nboot - 1, style = 3)
    opts <- list(progress = function(n) setTxtProgressBar(pb, n))
    on.exit(close(pb))
  } else {
    opts <- list(progress = NULL)
  }

  if (do_par) {
    cl <- parallel::makeCluster(getOption("exuber.ncores"), type = "PSOCK")
    registerDoSNOW(cl)
    on.exit(parallel::stopCluster(cl))
  }

  `%fun%` <- if (do_par) `%dopar%` else `%do%`

  for (j in 1:nc) {
    dy <- diff(y[, j])
    results <- foreach(
      i = 1:nboot, .export = c("rls_gsadf", "unroot"),
      .combine = "cbind", .options.snow = opts
    ) %fun% {
      if (show_pb && !do_par) setTxtProgressBar(pb, i)
      if (dist_rad) {
        w <- sample(c(-1, 1), nr - 1, replace = TRUE)
      } else {
        w <- rnorm(nr - 1, 0, 1)
      }
      ystar <- c(0, cumsum(w * dy))
      yxmat <- unroot(ystar)
      rls_gsadf(yxmat, min_win = minw)
    }


    badf_crit[, , j] <- t(apply(results[1:point, ], 1, quantile, prob = pr))
    adf_crit[j, ] <- quantile(results[point + 1, ], probs = pr)
    sadf_crit[j, ] <- quantile(results[point + 2, ], probs = pr)
    gsadf_crit[j, ] <- quantile(results[point + 3, ], probs = pr)
    bsadf_crit[, , j] <- t(apply(results[-c(1:(point + 3)), ], 1,
      quantile,
      prob = pr
    ))

    if (show_pb) {
      cat("\n")
      print(paste("Series", j, "out of", nc, "completed!", sep = " "), quote = F)
    }
  }

  output <- structure(list(
    adf_cv = adf_crit,
    sadf_cv = sadf_crit,
    gsadf_cv = gsadf_crit,
    badf_cv = badf_crit,
    bsadf_cv = bsadf_crit
  ),
  method = "Wild Bootstrap",
  iter = nboot,
  minw = minw,
  class = "cv"
  )

  return(output)
}
