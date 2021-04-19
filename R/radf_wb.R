

# DGP_PS ------------------------------------------------------------------

#' @importFrom stats coefficients residuals
adf_res <- function(x, adflag = 0, type = c("fixed", "aic", "bic")) {
  x <- as.matrix(x)
  type <- match.arg(type)
  if(type != "fixed") {
    adflag <- lag_select(x, criterion = type, max_lag = adflag)
  }
  yxmat <- as.data.frame(unroot_adf_null(x, adflag))
  reg <- lm(dy ~ ., data = yxmat[,-2, drop = FALSE])
  list(beta = coefficients(reg), res = residuals(reg))
}

radf_wb_dgp_ps <- function(y, adflag = 1, type = "fixed", tb = NULL) {

  result <- adf_res(y, adflag = adflag, type)
  beta   <- result$beta
  eps    <- result$res

  dy <- diff(y)
  nr <- length(dy)
  if(!is.null(tb)) {
    nr <- tb - 1
  }

  rN <- sample(1:(nr-adflag), replace = TRUE)
  wn <- rnorm(nr)

  dyb <- vector("numeric",  nr)
  dyb[1:adflag] <- dy[1:adflag]

  # TODO make it easier to simulate the dgp
  # epstar <- sample(eps, replace = TRUE)
  # w <- rnorm(nr)
  # if(adflag == 0) {
  #   dyb <- epstar * w
  # }else{
  #   x <- unroot_adf_null(as.matrix(y), lag = 1)[,-(1:2)]
  #   dyb <- filter(x %*% beta[-1] + epstar, method = "rec")
  # }

  if (adflag == 0) {
    for (i in (adflag + 1):(nr - 1)) {
      dyb[i] <- wn[i - adflag] * eps[rN[i - adflag]]
    }
  } else if (adflag > 0) {
    x <- matrix(0, nrow = nr - 1, ncol = adflag)
    for (i in (adflag + 1):(nr - 1)) {
      for( k in 1:adflag){
        x[i, k] <- dyb[i-k]
      }
      dyb[i] <- x[i,] %*% beta[-1] + wn[i - adflag] * eps[rN[i - adflag]]
    }
  }
  yb <- cumsum(c(y[1], dyb))
  yb
}


lag_select <- function(data, criterion = c("aic", "bic"), max_lag = 8) {
  criterion <- match.arg(criterion)
  tbl <- lag_select_table(data, max_lag)
  criterion_vec <- unname(tbl[, criterion])
  min_idx <- which.min(criterion_vec)
  as.integer(criterion_vec[min_idx])
}

#' @importFrom stats AIC BIC
lag_select_table <- function(data, max_lag = 8) {
  x <- parse_data(data)
  nc <- ncol(x)
  snames <- series_names(x)
  # min_criterion <- matrix(NA, nrow = nc, ncol = 2)
  aic <- bic <- vector("numeric", max_lag + 1)
  for (i in 1:nc) {
    for (l in 0:max_lag) {
      yxmat <- unroot_adf(x, lag = l)
      reg <- lm(dy ~ ., data = as.data.frame(yxmat[,-2]))
      aic[l + 1] <- AIC(reg)
      bic[l + 1] <- BIC(reg)
    }
    # min_criterion[i, 1] <- aic #which.min(aic) - 1
    # min_criterion[i, 2] <- bic #which.min(bic) - 1
  }
  criterion <- cbind(aic, bic)
  # dimnames(criterion) <- list(snames, c("aic", "bic"))
  criterion
}


# data = sim_data
# minw = 19
# nboot = 120
# adflag = 0
# tb = NULL
# type = "fixed"
# seed = NULL
# radf_wb_cv2(sim_data, minw, nboot, lag, tb = NULL)

radf_wb_ps <- function(data, minw, nboot, adflag, type, tb = NULL, seed = NULL) {

  y <- parse_data(data)
  assert_na(y)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(nboot, greater_than = 2)
  assert_positive_int(adflag, FALSE)

  nc <- ncol(y)
  nr <- nrow(y)
  if(!is.null(tb)) {
    assert_positive_int(tb, greater_than = minw)
    nr <- tb
  }

  pointer <- nr - minw
  snames <- colnames(y)
  adf_crit <- sadf_crit <- gsadf_crit <-
    array(NA, dim = c(nboot, nc), dimnames = list(NULL, snames))
  badf_crit <- bsadf_crit <-
    array(NA, dim = c(pointer, nboot, nc), dimnames = list(NULL, NULL, snames))

  show_pb <- getOption("exuber.show_progress")
  pb <- set_pb(nboot*nc)
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
    results <- foreach(
      i = 1:nboot,
      .export = c("rls_gsadf", "unroot", "radf_wb_dgp_ps"),
      .combine = "cbind",
      .options.snow = pb_opts,
      .inorder = FALSE
    ) %fun% {
      if (show_pb && !do_par) pb$tick()
      ystar <- radf_wb_dgp_ps(y[, j, drop = TRUE], adflag, tb = tb, type = type)
      yxmat <- unroot(ystar)
      rls_gsadf(yxmat, min_win = minw)
    }
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
#' \code{radf_wb_cv} performs the Phillips & Shi (2020) wild bootstrap re-sampling
#' scheme, which is asymptotically robust to non-stationary volatility, to
#' generate critical values for the recursive unit root tests. \code{radf_wb_distr2}
#' computes the distribution.
#'
#' @inheritParams radf
#' @inheritParams radf_mc_cv
#' @param nboot A positive integer. Number of bootstraps (default = 500L).
#' @param adflag A positive integer. Number of lags when type is "fixed" or number
#' of max lags when type is either "aic" or "bic".
#' @param type Character. "fixed" for fixed lag, "aic" or "bic" for automatic lag
#' selection according to the criterion.
#' @param tb A positive integer. The simulated sample size.
#'
#'
#' @return  For \code{radf_wb_cv2} a list that contains the critical values for the ADF,
#' BADF, BSADF and GSADF tests. For \code{radf_wb_distr} a list that
#' contains the ADF, SADF and GSADF distributions.
#'
#' @references Phillips, P. C., & Shi, S. (2020). Real time monitoring of
#' asset markets: Bubbles and crises. In Handbook of Statistics (Vol. 42, pp. 61-80). Elsevier.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @seealso \code{\link{radf_mc_cv}} for Monte Carlo critical values and
#' \code{\link{radf_sb_cv}} for sieve bootstrap critical values.
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom stats quantile rnorm
#' @export
#'
#' @examples
#' \donttest{
#' # Default minimum window
#' wb <- radf_wb_cv2(sim_data)
#'
#' tidy(wb)
#'
#' # Change the minimum window and the number of bootstraps
#' wb2 <- radf_wb_cv2(sim_data, nboot = 600, minw = 20)
#'
#'tidy(wb2)
#'
#' # Simulate distribution
#' wdist <- radf_wb_distr(sim_data)
#'
#' autoplot(wdist)
#' }
radf_wb_cv2 <- function(data, minw = NULL, nboot = 500L, adflag = 0,
                        type = c("fixed", "aic", "bic"), tb = NULL, seed = NULL) {

  type <- match.arg(type)
  results <- radf_wb_ps(data, minw = minw, nboot = nboot, adflag = adflag,
                        type = type, tb = tb, seed = seed)

  pcnt <- c(0.9, 0.95, 0.99)
  adf_crit   <- apply(results$adf, 2, quantile, probs = pcnt) %>% t()
  sadf_crit  <- apply(results$sadf, 2, quantile, probs = pcnt) %>% t()
  gsadf_crit <- apply(results$gsadf, 2, quantile, probs = pcnt) %>% t()

  badf_crit  <- apply(results$badf, c(1,3), quantile, probs = pcnt) %>%
    apply(c(1,3), t)
  bsadf_crit <- apply(results$bsadf, c(1,3), quantile, probs = pcnt) %>%
    apply(c(1,3), t)

  if(!is.null(tb)) {
    y <- parse_data(data)
    minw <- minw %||% psy_minw(data)
    nc <- ncol(y)
    nr <- nrow(y)
    pointer <- nr - minw
    snames <- colnames(y)
    badf_crit <- bsadf_crit <- array(NA, dim = c(pointer, 3, nc),
                        dimnames = list(NULL, c("90%", "95%", "99%"), snames))
    for(i in 1:nc) {
      for(j in 1:3) {
        badf_crit[,j,i] <- rep(sadf_crit[i,j], pointer)
        bsadf_crit[,j,i] <- rep(gsadf_crit[i,j], pointer)
      }
    }
  }

  list(
    adf_cv = adf_crit,
    sadf_cv = sadf_crit,
    gsadf_cv = gsadf_crit,
    badf_cv = badf_crit,
    bsadf_cv = bsadf_crit
  ) %>%
    inherit_attrs(results) %>%
    add_class("radf_cv", "wb_cv")

}


#' @rdname radf_wb_cv2
#' @inheritParams radf_wb_cv
#' @export
radf_wb_distr2 <- function(data, minw = NULL, nboot = 500L, adflag = 0,
                           type = c("fixed", "aic", "bic"), tb = NULL, seed = NULL) {

  results <- radf_wb_ps(data, minw = minw, nboot = nboot,
                        adflag = adflag, type = type, tb = tb, seed = seed)

  list(
    adf_distr = results$adf,
    sadf_distr = results$sadf,
    gsadf_distr = results$gsadf
  ) %>%
    inherit_attrs(results) %>%
    add_class("radf_distr", "wb_distr")
}

# DGP_HLST ----------------------------------------------------------------

radf_wb_dgp_hlst <- function(y, dist_rad) {
  dy <- diff(y)
  nr <- length(dy)
  if (dist_rad) {
    w <- sample(c(-1, 1), nr, replace = TRUE)
  } else {
    w <- rnorm(nr, 0, 1)
  }
  estar <- cumsum(w*dy)
  ystar <- c(0, estar)
  ystar
}


radf_wb_hlst <- function(data, minw, nboot, dist_rad = FALSE, seed = NULL) {

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
  pb <- set_pb(nboot*nc)
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
    results <- foreach(
      i = 1:nboot,
      .export = c("rls_gsadf", "unroot", "radf_wb_dgp_hlst"),
      .combine = "cbind",
      .options.snow = pb_opts,
      .inorder = FALSE
    ) %fun% {
      if (show_pb && !do_par) pb$tick()
      ystar <- radf_wb_dgp_hlst(y[, j], dist_rad)
      yxmat <- unroot(ystar)
      rls_gsadf(yxmat, min_win = minw)
    }

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
#' @param dist_rad Logical. If TRUE then the Rademacher distribution will be used.
#'
#' @return  For \code{radf_wb_cv} a list that contains the critical values for the ADF,
#' BADF, BSADF and GSADF tests. For \code{radf_wb_distr} a list that
#' contains the ADF, SADF and GSADF distributions.
#'
#' @details
#'
#' This approach involves applying a wild bootstrap re-sampling scheme
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
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach %dopar% %do%
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
radf_wb_cv <- function(data, minw = NULL, nboot = 500L, dist_rad = FALSE, seed = NULL) {

  results <- radf_wb_hlst(data, minw = minw, nboot = nboot,
                          dist_rad = dist_rad, seed = seed)

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
    add_class("radf_cv", "wb_cv")

}

#' @rdname radf_wb_cv
#' @inheritParams radf_wb_cv
#' @export
radf_wb_distr <- function(data, minw = NULL, nboot = 500L, dist_rad = FALSE, seed = NULL) {

  results <- radf_wb_hlst(data, minw = minw, nboot = nboot,
                          dist_rad = dist_rad, seed = seed)

  list(
    adf_distr = results$adf,
    sadf_distr = results$sadf,
    gsadf_distr = results$gsadf
  ) %>%
    inherit_attrs(results) %>%
    add_class("radf_distr", "wb_distr")
}

