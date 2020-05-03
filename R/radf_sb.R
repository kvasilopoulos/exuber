radf_sb_ <-  function(data, minw, lag, nboot, seed = NULL) {

  y <- parse_data(data)
  assert_na(y)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)
  assert_positive_int(nboot, greater_than = 2)

  nc <- ncol(y)
  nr <- nrow(y)
  snames <- colnames(y)
  pointer <- nr - minw - lag

  initmat <- matrix(0, nc, 1 + lag)
  resmat  <- matrix(0, nr - 2 - lag, nc)
  coefmat <- matrix(0, nc, 2 + lag)

  set_rng(seed)
  for (j in 1:nc) {
    ys <- y[, j]
    dy <- ys[-1] - ys[-nr]
    ym <- embed(dy, lag + 2)
    lr_dy <- lm(ym[, 1] ~ ym[, -1])
    res <- as.vector(lr_dy$residuals)
    coef <- as.vector(lr_dy$coefficients)

    initmat[j, ] <- ym[1, -1]
    coefmat[j, ] <- coef
    resmat[, j] <- res
  }

  nres <- NROW(resmat)

  show_pb <- getOption("exuber.show_progress")
  pb <- set_pb(nboot)
  pb_opts <- set_pb_opts(pb)

  do_par <- getOption("exuber.parallel")
  if (do_par) {
    cl <- parallel::makeCluster(getOption("exuber.ncores"), type = "PSOCK")
    registerDoSNOW(cl)
    on.exit(parallel::stopCluster(cl))
  }
  set_rng(seed)

  `%fun%` <- if (do_par) `%dorng%` else `%do%`

  edf_bsadf_panel <- foreach(
    i = 1:nboot,
    .export = c("rls_gsadf", "unroot"),
    .combine = "cbind",
    .options.snow = pb_opts,
    .inorder = FALSE
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
      bsadf_boot <- aux_boot[-c(1:(pointer + 3))]
    }
    bsadf_boot / nc
  }

  bsadf_crit <- unname(edf_bsadf_panel)
  gsadf_crit <- apply(edf_bsadf_panel, 2, max) %>% unname()

  list(bsadf_panel = bsadf_crit,
       gsadf_panel = gsadf_crit) %>%
    add_attr(
      index = attr(y, "index"),
      series_names = snames,
      method = "Sieve Bootstrap",
      n = nr,
      minw = minw,
      lag = lag,
      iter = nboot,
      seed = get_rng_state(seed),
      parallel = do_par)
}


#' Panel Sieve Bootstrap Critical Values
#'
#' \code{radf_sb_cv} computes critical values for the panel recursive unit root test using
#' the sieve bootstrap procedure outlined in Pavlidis et al. (2016). \code{radf_sb_distr}
#' computes the distribution.
#'
#' @inheritParams radf
#' @inheritParams radf_wb_cv
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
#' In search of the smoking gun. The Journal of Real Estate Finance and Economics, 53(4), 419-449.
#'
#' @seealso \code{\link{radf_mc_cv}} for Monte Carlo critical values and
#' \code{\link{radf_wb_cv}} for wild Bootstrap critical values
#'
#' @return  For \code{radf_sb_cv} A list A list that contains the critical values
#' for the panel BSADF and panel GSADF test statistics. For \code{radf_wb_dist} a numeric vector
#' that contains the distribution of the panel GSADF statistic.
#'
#' @examples
#' \donttest{
#'
#' rsim_data <- radf(sim_data, lag = 1)
#'
#' # Critical vales should have the same lag length with \code{radf()}
#' sb <- radf_sb_cv(sim_data, lag = 1)
#'
#' tidy(sb)
#'
#' summary(rsim_data, cv = sb)
#'
#' autoplot(rsim_data, cv = sb)
#'
#' # Simulate distribution
#' sdist <- radf_sb_distr(sim_data, lag = 1, nboot = 1000)
#'
#' autoplot(sdist)
#' }
radf_sb_cv <- function(data, minw = NULL, lag = 0L,
                  nboot = 500L, seed = NULL) {

  results <- radf_sb_(data, minw, nboot = nboot, lag = lag, seed = seed)

  pcnt <- c(0.9, 0.95, 0.99)

  bsadf_crit <- apply(results$bsadf_panel, 1, quantile, probs = pcnt) %>% t()
  gsadf_crit <- quantile(results$gsadf_panel, probs = pcnt)

    list(gsadf_panel_cv = gsadf_crit,
         bsadf_panel_cv = bsadf_crit) %>%
      inherit_attrs(results) %>%
      add_class("radf_cv", "sb_cv","cv")

}

#' @rdname radf_sb_cv
#' @inheritParams radf_sb_cv
#' @export
radf_sb_distr <- function(data, minw = NULL, lag = 0L, nboot = 500L, seed = NULL) {

  results <- radf_sb_(data, minw, nboot = nboot, lag = lag, seed = seed)

  c(results$gsadf_panel) %>%
    inherit_attrs(results) %>%
    add_class("radf_distr", "sb_distr","distr")
}
