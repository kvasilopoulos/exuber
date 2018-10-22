#' Panel Sieve Bootstrap Critical values
#'
#' \code{sb_cv} performs the Pavlidis et al. (2016)
#'
#' @inheritParams radf
#' @inheritParams wb_cv
#'
#' @return A list that contains the panel critical values for BSADF and GSADF
#' t-statistics.
#'
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar%
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats quantile lm
#' @export
#'
#'
#' @examples
#' \donttest{
#' # Simulate bubble processes
#' dta <- cbind(sim_dgp1(n = 100), sim_dgp2(n = 100), sim_dgp2(n = 100))
#'
#' rfd <- radf(dta, lag = 1)
#'
#' pcv <- sb_cv(dta, lag = 1)
#'
#' summary(dta, pcv, panel = TRUE)
#' plot(rdta, pcv, panel = TRUE)
#' }

sb_cv <- function(data, minw, lag =0, nboot = 1000){

  # index-date check
  data <- rm_index(data)
  # helpers
  y <- as.matrix(data)
  nc <- NCOL(y)
  nr <- NROW(y)
  # args
  if (missing(minw)) minw <-  floor((r0 <- 0.01 + 1.8 / sqrt(nr)) * nr)
  # checks
  assert_na(y)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)
  assert_positive_int(nboot, greater_than = 2)
  # get options
  show_pb <- getOption("exuber.show_progress")
  parallel <- getOption("exuber.parallel")
  ncores <- getOption("exuber.ncores")

  # helpers 2
  point <- nr - minw
  pr <- c(0.9, 0.95, 0.99)
  pb <- txtProgressBar(min = 1, max = nboot - 1, style = 3)
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

  if (parallel) {
    cl <- parallel::makeCluster(ncores, type = "PSOCK")
    on.exit(parallel::stopCluster(cl))
    registerDoSNOW(cl)

    progress <- if (show_pb)  function(n) setTxtProgressBar(pb, n) else NULL
    opts <- list(progress = progress)

    edf_bsadf_panel <- foreach(i = 1:nboot,
                               .export = c("rls_gsadf", "unroot"),
                               .combine = "cbind",
                               .options.snow = opts) %dopar%
      {
        boot_index <- sample(1:nres, replace = TRUE)
        for (j in 1:nc) {
          boot_res <- resmat[boot_index, j]
          dboot_res <- boot_res - mean(boot_res)
          dy_boot <- c(initmat[j, lag:1],
                       stats::filter(coefmat[j, 1] + dboot_res,
                                     coefmat[j, -1], "rec", init = initmat[j, ]))
          y_boot <- cumsum(c(y[1, j], dy_boot))
          yxmat_boot <- unroot(x = y_boot, lag)
          aux_boot <- rls_gsadf(yxmat_boot, minw, lag)
          bsadf_boot <- aux_boot[-c(1:(point + 3))]
        }
        bsadf_boot / nc
      }
  }else{
    # non-parallel estimation needs preallocation
    # needs to substract 3 because of the lag difference
    if (lag == 0) {
      edf_bsadf_panel <- matrix(0, point, nboot)
    }else{
      edf_bsadf_panel <- matrix(0, point - lag - 3, nboot)
    }
    for (i in 1:nboot) {

      boot_index <- sample(1:nres, replace = TRUE)

      for (j in 1:nc) {
        boot_res <- resmat[boot_index, j]
        dboot_res <- boot_res - mean(boot_res)
        dy_boot <- c(initmat[j, lag:1],
                     stats::filter(coefmat[j, 1] + dboot_res,
                                   coefmat[j, -1], "rec", init = initmat[j, ]))
        y_boot <- cumsum(c(y[1, j], dy_boot))
        yxmat_boot <- unroot(x = y_boot, lag)
        aux_boot <- rls_gsadf(yxmat_boot, minw, lag)
        bsadf_boot <- aux_boot[-c(1:(point + 3))]
      }
      edf_bsadf_panel[, i] <- bsadf_boot / nc
      if (show_pb) setTxtProgressBar(pb, i)
    }
  }
  close(pb)

  bsadf_crit <- apply(edf_bsadf_panel, 1, quantile, probs = pr) %>% t()
  gsadf_crit <- apply(edf_bsadf_panel, 2, max) %>% quantile(probs = pr)

  output <- structure(list(gsadf_panel_cv = gsadf_crit,
                           bsadf_panel_cv = bsadf_crit),
                      method = "Sieve Bootstrap",
                      lag    = lag,
                      iter   = nboot,
                      minw   = minw,
                      class  = "cv")

  return(output)
}
