radf_wb_ <- function(data, minw, nboot, dist_rad, seed = NULL) {

  y <- parse_data(data)
  assert_na(y)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(nboot, greater_than = 2)
  stopifnot(is.logical(dist_rad))

  nc <- ncol(y)
  nr <- nrow(y)

  pointer <- nr - minw
  snames <- colnames(y)
  adf_crit <- sadf_crit <- gsadf_crit <-
    array(NA, dim = c(nboot, nc), dimnames = list(NULL, snames))
  badf_crit <- bsadf_crit <-
    array(NA, dim = c(pointer, nboot, nc), dimnames = list(NULL, NULL, snames))

  show_pb <- getOption("exuber.show_progress")
  pb <- set_pb(nboot, width = getOption("width") - 15)
  pb_opts <- set_pb_opts(pb)

  do_par <- getOption("exuber.parallel")
  if (do_par) {
    cl <- parallel::makeCluster(getOption("exuber.ncores"), type = "PSOCK")
    registerDoSNOW(cl)
    on.exit(parallel::stopCluster(cl))
  }

  `%fun%` <- if (do_par) `%dorng%` else `%do%`

  set_rng(seed)
  for (j in 1:nc) {

    dy <- diff(y[, j])
    results <- foreach(
      i = 1:nboot,
      .export = c("rls_gsadf", "unroot"),
      .combine = "cbind",
      .options.snow = pb_opts,
      .inorder = FALSE
    ) %fun% {
      if (show_pb && !do_par) {
        setTxtProgressBar(pb, i)
      }
      if (dist_rad) {
        w <- sample(c(-1, 1), nr - 1, replace = TRUE)
      } else {
        w <- rnorm(nr - 1, 0, 1)
      }
      estar <- cumsum(w*dy)
      ystar <- c(0, estar)
      yxmat <- unroot(ystar)
      rls_gsadf(yxmat, min_win = minw)
    }

    if (show_pb)
      cat(paste0(" ", j, "/", nc))

    adf_crit[, j] <- results[pointer + 1, ]
    sadf_crit[, j] <- results[pointer + 2, ]
    gsadf_crit[, j] <- results[pointer + 3, ]

    badf_crit[, , j] <- results[1:pointer, ]
    bsadf_crit[, , j] <- results[-c(1:(pointer + 3)), ]
  }

    list(
      adf = adf_crit,
      sadf = sadf_crit,
      gsadf = gsadf_crit,
      badf = badf_crit,
      bsadf = bsadf_crit) %>%
    add_attr(
      index = attr(y, "index"),
      series_names = snames,
      method = "Wild Bootstrap",
      n = nrow(y),
      minw = minw,
      iter = nboot,
      seed = get_rng_state(seed),
      parallel = do_par
      )


}

#' Wild Bootstrap Critical Values
#'
#' \code{radf_wb_cv} performs the Harvey et al. (2016) wild bootstrap re-sampling
#' scheme, which is asymptotically robust to non-stationary volatility, to
#' generate critical values for the recursive unit root tests. \code{radf_wb_distr}
#' computes the distribution.
#'
#' @inheritParams radf
#' @inheritParams radf_mc_cv
#' @param nboot A positive integer. Number of bootstraps (default = 500L).
#' @param dist_rad Logical. If \code{TRUE} then  the Rademacher distribution
#' will be used.
#'
#' @return  For \code{radf_wb_cv} a list that contains the critical values for the ADF,
#' BADF, BSADF and GSADF tests. For \code{radf_wb_distr} a list that
#' contains the ADF, SADF and GSADF distributions.
#'
#' @details This approach involves applying a wild bootstrap re-sampling scheme
#' to construct the bootstrap analogue of the Phillips et al. (2015) test which
#' is asymptotically robust to non-stationary volatility.
#'
#' @references Harvey, D. I., Leybourne, S. J., Sollis, R., & Taylor, A. M. R.
#' (2016). Tests for explosive financial bubbles in the presence of
#' non-stationary volatility. Journal of Empirical Finance, 38(Part B), 548-574.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @seealso \code{\link{radf_mc_cv}} for Monte Carlo critical values and
#' \code{\link{radf_sb_cv}} for sieve bootstrap critical values.
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
#' # Default minimum window
#' wb <- radf_wb_cv(sim_data)
#'
#' tidy(wb)
#'
#' # Change the minimum window and the number of bootstraps
#' wb2 <- radf_wb_cv(sim_data, nboot = 600, minw = 20)
#'
#'tidy(wb2)
#'
#' # Simulate distribution
#' wdist <- radf_wb_distr(sim_data)
#'
#' autoplot(wdist)
#' }
radf_wb_cv <- function(data, minw = NULL, nboot = 500L,
                  dist_rad = FALSE, seed = NULL) {

  results <- radf_wb_(data, minw = minw, nboot = nboot, dist_rad = dist_rad, seed = seed)

  pcnt <- c(0.9, 0.95, 0.99)

  adf_crit   <- apply(results$adf, 2, quantile, probs = pcnt) %>% t()
  sadf_crit  <- apply(results$sadf, 2, quantile, probs = pcnt) %>% t()
  gsadf_crit <- apply(results$gsadf, 2, quantile, probs = pcnt) %>% t()

  badf_crit  <- apply(results$badf, c(1,3), quantile, probs = pcnt) %>%
    apply(c(1,3), t)
  bsadf_crit <- apply(results$bsadf, c(1,3), quantile, probs = pcnt) %>%
    apply(c(1,3), t)

  list(
    adf_cv = adf_crit,
    sadf_cv = sadf_crit,
    gsadf_cv = gsadf_crit,
    badf_cv = badf_crit,
    bsadf_cv = bsadf_crit
  ) %>%
    inherit_attrs(results) %>%
    add_class("radf_cv", "wb_cv", "cv")

}

#' @rdname radf_wb_cv
#' @inheritParams radf_wb_cv
#' @export
radf_wb_distr <- function(data, minw = NULL, nboot = 500L,
                    dist_rad = FALSE, seed = NULL) {

  results <- radf_wb_(data, minw = minw, nboot = nboot, dist_rad = dist_rad, seed = seed)

  list(
    adf_distr = results$adf,
    sadf_distr = results$sadf,
    gsadf_distr = results$gsadf
  ) %>%
    inherit_attrs(results) %>%
    add_class("radf_distr", "wb_distr", "distr")
}

