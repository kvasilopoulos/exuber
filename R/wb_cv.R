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
#' @seealso \code{\link{mc_cv}} for Monte Carlo critical values
#'
<<<<<<< HEAD
#' @import doParallel
#' @import doSNOW
#' @import parallel
#' @import foreach
#' @importFrom utils setTxtProgressBar txtProgressBar flush.console
#' @importFrom stats rnorm quantile
=======
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats quantile rnorm
#' @importFrom lubridate is.Date
#' @importFrom purrr detect_index
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
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
wb_cv <- function(data, nboot = 1000, minw, parallel = FALSE,
                  ncores, dist_rad = FALSE) {

<<<<<<< HEAD
  assert_positive_int(nboot)
  if (missing(minw)) {
    r0 <- 0.01 + 1.8 / sqrt(nr)
    minw <- floor(r0 * nr)
  } else if (!minw == round(minw) | minw <= 0) {
    stop("Argument 'minw' should be a positive integer", call. = FALSE)
  } else if (minw < 3) {
    stop("Argument 'minw' is too small", call. = FALSE)
  }

  stopifnot(is.logical(parallel),
            is.logical(dist_rad))
  if (any(is.na(y))) {
    stop("Recursive least square estimation cannot handle NA", call. = FALSE)
  }

  if (missing(ncores)) {
    ncores <- detectCores() - 1
  } else {
    if (!parallel) {
      stop("Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'",
        call. = FALSE
      )
    }
  }

  # is.between(ncores, 2, detectCores()) more than 2 maybe
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

    for (j in 1:nc) {

      dy <- y[-1, j] - y[-nr, j]

      if (parallel) {
        cl <- parallel::makeCluster(ncores, type = "PSOCK")
        on.exit(parallel::stopCluster(cl))
        registerDoSNOW(cl)

        progress <- function(n) setTxtProgressBar(pb, n)
        opts <- list(progress = progress)


        results <- foreach(i = 1:nboot, .export = "srls_gsadf_cpp",
          .combine = "cbind", .options.snow = opts) %dopar% {
          ystar <- 0
          if (dist_rad) {
            w <- sample(c(-1, 1), nr - 1, replace = TRUE)
          } else {
            w <- rnorm(nr - 1, 0, 1)
          }
          ystar <- c(0, cumsum(w * dy))
          srls_gsadf_cpp(ystar[-1], ystar[-nr], minw)
        }
      }else{
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
  close(pb)

  bsadf_critical_adj <- apply(bsadf_critical, c(2,3), cummax)
  badf_critical_adj <- apply(badf_critical, c(2,3), cummax)

  output <- structure(list(adf_cv = adf_critical,
                           sadf_cv = sadf_critical,
                           gsadf_cv = gsadf_critical,
                           badf_cv = badf_critical_adj,
                           bsadf_cv = bsadf_critical_adj),
=======

  # date check
  if (is.data.frame(data)) {
    date_index <- purrr::detect_index(data, lubridate::is.Date)
    if (as.logical(date_index)) data <- data[, -date_index, drop = FALSE]
  }
  # helpers
  y <- as.matrix(data)
  nc <- NCOL(y)
  nr <- NROW(y)
  # args
  if (missing(minw)) minw <-  floor((r0 <- 0.01 + 1.8 / sqrt(nr)) * nr)
  warning_redudant(ncores, cond = !missing(ncores) && !parallel)
  if (missing(ncores)) ncores <- detectCores() - 1
  # checks
  assert_na(y)
  assert_positive_int(nboot)
  assert_positive_int(minw, greater_than = 2)
  stopifnot(is.logical(parallel), is.logical(dist_rad))
  # helpers 2
  point <- nr - minw
  pr <- c(0.9, 0.95, 0.99)
  pb <- txtProgressBar(max = nboot , style = 3)
  # preallocation
  results <- matrix(0, nrow = 2 * point + 3, ncol = nboot)
  adf_crit <-  matrix(NA, nc, 3, dimnames = list(colnames(y), c(paste(pr))))
  sadf_crit <- gsadf_crit <- adf_crit
  badf_crit <- bsadf_crit <- array(NA, dim = c(point, 3, nc),
                        dimnames = list( NULL, c(paste(pr)), colnames(y)))

  if (parallel) {
    cl <- parallel::makeCluster(ncores, type = "PSOCK")
    registerDoSNOW(cl)
    on.exit(parallel::stopCluster(cl))
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  }

  for (j in 1:nc) {
    dy <- diff(y[, j])
    if (parallel) {
      results <- foreach(i = 1:nboot, .export = c("rls_gsadf", "unroot"),
                         .combine = "cbind", .options.snow = opts) %dopar% {
                           if (dist_rad) {
                             w <- sample(c(-1, 1), nr - 1, replace = TRUE)
                           } else {
                             w <- rnorm(nr - 1, 0, 1)
                           }
                           ystar <- c(0, cumsum(w * dy))
                           yxmat <- unroot(ystar)
                           rls_gsadf(yxmat, min_win = minw)
                         }
    }else{
      for (i in 1:nboot) {
        setTxtProgressBar(pb, i)
        if (dist_rad) {
          w <- sample(c(-1, 1), nr - 1, replace = TRUE)
        } else {
          w <- rnorm(nr - 1, 0, 1)
        }
        ystar <- c(0, cumsum(w * dy))
        yxmat <- unroot(ystar)
        results[, i] <- rls_gsadf(yxmat, min_win = minw)
      }
    }

    badf_crit[, , j] <- t(apply(results[1:point, ], 1, quantile, prob = pr))
    adf_crit[j, ] <- quantile(results[point + 1, ], probs = pr)
    sadf_crit[j, ] <- quantile(results[point + 2, ], probs = pr)
    gsadf_crit[j, ] <- quantile(results[point + 3, ], probs = pr)
    bsadf_crit[, , j] <- t(apply(results[-c(1:(point + 3)), ], 1,
                                 quantile, prob = pr))
    cat("\n")
    print(paste("Series", j, "out of", nc, "completed!", sep = " "),
        quote = FALSE)
  }
  close(pb)

  bsadf_crit_adj <- apply(bsadf_crit, c(2,3), cummax)
  badf_crit_adj <- apply(badf_crit, c(2,3), cummax)

  output <- structure(list(adf_cv = adf_crit,
                           sadf_cv = sadf_crit,
                           gsadf_cv = gsadf_crit,
                           badf_cv = badf_crit_adj,
                           bsadf_cv = bsadf_crit_adj),
>>>>>>> pipe friendly version: confrom better with ggplot and S3 methods
                      method = "Wild Bootstrap",
                      iter   = nboot,
                      minw   = minw,
                      class  = "cv")

  return(output)
}
