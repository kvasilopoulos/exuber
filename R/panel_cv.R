#' Panel Sieve Bootstrap Critical values
#'
#' \code{panel_cv} performs the Pavlidis et al. (2016) wild bootstrap re-sampling
#' scheme, which is asymptotically robust to non-stationary volatility, to
#' generate critical values for the recursive unit root tests.
#'
#' @inheritParams radf
#' @inheritParams wb_cv
#'
#' @return A list that contains the panel critical values for BSADF and GSADF
#' t-statistics.
#'
#' @import doParallel
#' @import doSNOW
#' @import parallel
#' @import foreach
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom stats lm
#' @export
#'
panel_cv <- function(x, lag = 0, minw, nboot = 1000, parallel = FALSE, ncores){

  nc <- NCOL(x)
  nr <- NROW(x)

  is.positive.int(nboot)
  if (missing(minw)) {
    r0 <- 0.01 + 1.8 / sqrt(nr)
    minw <- floor(r0 * nr)
  } else if (!minw == round(minw) | minw <= 0) {
    stop("Argument 'minw' should be a positive integer", call. = FALSE)
  } else if (minw < 3) {
    stop("Argument 'minw' is too small", call. = FALSE)
  }
  # stopifnot(is.logical(parallel)
  if (any(is.na(x))) {
    stop("Recursive least square estimation cannot handle NA", call. = FALSE)
  }

  if (missing(ncores)) {
    ncores <- detectCores()
  } else {
    if (!parallel) {
      stop("Argument 'ncores' is redundant when 'parallel' is set to 'FALSE'",
           call. = FALSE
      )
    }
  }


  initmat <- matrix(0, nc, 1)
  resmat <- matrix(0, nr - 2 - lag, nc)
  coefmat <- matrix(0, nc, 2)

  for (j in 1:nc) {

    y <- x[, j]
    dy <- y[-1] - y[-nr]
    ym <- embed(dy, lag + 2)
    lr_dy <- lm(ym[, 1] ~ ym[, -1])
    res <- as.vector(lr_dy$residuals)
    coef <- as.vector(lr_dy$coef)

    initmat[j ] <- ym[1, -1]
    coefmat[j, ] <- coef
    resmat[, j] <- res
  }

  nres <- NROW(resmat)
  edf_bsadf_panel <- matrix(0, nr - 1 - minw, nboot)
  pb <- txtProgressBar(min = 1, max = nboot - 1, style = 3)

  if (parallel) {

    cl <- makeCluster(ncores, type = "PSOCK")
    on.exit(stopCluster(cl))
    registerDoSNOW(cl)

    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    edf_bsadf_panel <- foreach(i = 1:nboot, .export = "rls_gsadf_cpp",
      .combine = "cbind", .options.snow = opts) %dopar% {

      boot_index <- sample(1:nres, replace = TRUE)

      for (j in 1:nc) {

        boot_res <- resmat[boot_index, j]
        dboot_res <- boot_res - mean(boot_res)
        dy_boot <- c(initmat[j, lag:1],
                     stats::filter(coefmat[j, 1] + dboot_res,
                                   coefmat[j, -1], "rec", init = initmat[j, ]))
        y_boot <- cumsum(c(x[1, j], dy_boot))
        yxmat_boot <- unroot(y_boot,lag)
        aux_boot <- rls_gsadf_cpp(yxmat_boot, minw)
        bsadf_boot <- aux_boot$bsadf[-c(1:minw)]

      }
      edf_bsadf_panel <- bsadf_boot / nc
    }
  }else{
    for (i in 1:nboot) {

      boot_index <- sample(1:nres, replace = TRUE)

      for (j in 1:nc) {
        boot_res <- resmat[boot_index, j]
        dboot_res <- boot_res - mean(boot_res)
        dy_boot <- c(initmat[j, lag:1], stats::filter(coefmat[j, 1] + dboot_res,
                                                      coefmat[j, -1], "rec",
                                                      init = initmat[j, ]))
        y_boot <- cumsum(c(x[1, j], dy_boot))
        yxmat_boot <- unroot(y_boot,lag)
        aux_boot <- rls_gsadf_cpp(yxmat_boot, minw)
        bsadf_boot <- aux_boot$bsadf[-c(1:minw)]

      }

      edf_bsadf_panel[, i] <- bsadf_boot / nc
      setTxtProgressBar(pb, i)

    }
  }
  close(pb)


  bsadf_cv <- t(apply(edf_bsadf_panel, 1, function(z)
    cummax(quantile(z, probs = c(0.90, 0.95, 0.99)))))
  gsadf_cv <- quantile(apply(edf_bsadf_panel, 2, max ),
                        probs = c(0.90, 0.95, 0.99))

  output <- structure(list(gsadf_cv = gsadf_cv,
                           bsadf_cv = bsadf_cv),
                      method = "Sieve Bootstrap",
                      iter   = nboot,
                      minw   = minw,
                      class  = "cv")

  return(output)
}
