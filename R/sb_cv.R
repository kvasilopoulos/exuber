#' Panel Sieve Bootstrap Critical Values
#'
#' \code{sb_cv} computes p-values for the panel recursive unit root test using
#' the sieve bootstrap procedure outlined in Pavlidis et al. (2016)
#'
#' @inheritParams radf
#' @inheritParams wb_cv
#'
#' @return A list that contains the panel critical values for BSADF and GSADF
#' t-statistics.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats quantile lm
#' @export
#'
#' @references Pavlidis, E., Yusupova, A., Paya, I., Peel, D., Martínez-García,
#' E., Mack, A., & Grossman, V. (2016). Episodes of exuberance in housing markets:
#' in search of the smoking gun. The Journal of Real Estate Finance and Economics, 53(4), 419-449.
#'
#' @seealso \code{\link{mc_cv}} for Monte Carlo critical values and
#' \code{\link{wb_cv}} for Wild Bootstrapped critical values
#'
#' @examples
#' \donttest{
#'
#' # Simulate bubble processes
#' set.seed(124)
#' pdta <- cbind(sim_dgp1(100), sim_dgp1(100), sim_div(100), sim_div(100), sim_div(100))
#'
#' # Panel critical vales should have the same lag length with the estimation
#' sb <- sb_cv(pdta, lag = 1)
#'
#' pdta %>%
#'   radf(lag = 1) %>%
#'   summary(cv = sb)
#'
#' pdta %>%
#'   radf(lag = 1) %>%
#'   autoplot(cv = sb)
#' }
sb_cv <- function(data, minw, lag = 0, nboot = 1000) {

  y <- data %>% rm_index() %>% as.matrix() # index-date check
  nc <- NCOL(y)
  nr <- NROW(y)

  # args
  if (missing(minw)) minw <- floor((0.01 + 1.8 / sqrt(nr)) * nr)

  # asserts
  assert_na(y)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)
  assert_positive_int(nboot, greater_than = 2)

  # helpers
  point <- nr - minw - lag
  pr <- c(0.9, 0.95, 0.99)

  # preallocation
  initmat <- matrix(0, nc, 1 + lag)
  resmat <- matrix(0, nr - 2 - lag, nc)
  coefmat <- matrix(0, nc, 2 + lag)

  for (j in 1:nc) {
    ys <- y[, j]
    dy <- ys[-1] - ys[-nr]
    ym <- embed(dy, lag + 2)
    lr_dy <- lm(ym[, 1] ~ ym[, -1])
    res <- as.vector(lr_dy$residuals)
    coef <- as.vector(lr_dy$coef)

    initmat[j, ] <- ym[1, -1]
    coefmat[j, ] <- coef
    resmat[, j] <- res
  }

  nres <- NROW(resmat)

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

  edf_bsadf_panel <- foreach(
    i = 1:nboot,
    .export = c("rls_gsadf", "unroot"),
    .combine = "cbind",
    .options.snow = opts
  ) %fun% {
    boot_index <- sample(1:nres, replace = TRUE)
    if (show_pb && !do_par) setTxtProgressBar(pb, i)
    for (j in 1:nc) {
      boot_res <- resmat[boot_index, j]
      dboot_res <- boot_res - mean(boot_res)
      dy_boot <- c(
        initmat[j, lag:1],
        stats::filter(coefmat[j, 1] + dboot_res,
          coefmat[j, -1], "rec",
          init = initmat[j, ]
        )
      )
      y_boot <- cumsum(c(y[1, j], dy_boot))
      yxmat_boot <- unroot(x = y_boot, lag)
      aux_boot <- rls_gsadf(yxmat_boot, minw, lag)
      bsadf_boot <- aux_boot[-c(1:(point + 3))]
    }
    bsadf_boot / nc
  }

  bsadf_crit <- apply(edf_bsadf_panel, 1, quantile, probs = pr) %>% t()
  gsadf_crit <- apply(edf_bsadf_panel, 2, max) %>% quantile(probs = pr)

  output <- structure(list(
    gsadf_panel_cv = gsadf_crit,
    bsadf_panel_cv = bsadf_crit
  ),
  method = "Sieve Bootstrap",
  lag = lag,
  iter = nboot,
  minw = minw,
  class = "cv"
  )

  return(output)
}
