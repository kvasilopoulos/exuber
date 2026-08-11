# SBZ: weighted-least-squares (WLS) bubble test with a kernel volatility
# estimator, plus a union-of-rejections test combining it with the classic
# PWY/PSY sup-ADF (supDF). Harvey, Leybourne & Zu (2019, Econometric Reviews
# 38(10), 1131-1151; open working paper: Granger Centre DP 18/05).
#
# Size control under time-varying volatility comes from the *same* wild
# bootstrap exuber already implements for supDF (radf_wb_dgp_hlst() /
# radf_wb_cv()) -- HLST (2016) is this paper's own size-control device too
# (Section 4) -- applied *jointly* to supDF and supBZ so the union procedure
# is correctly sized (Theorem 3).

# Nadaraya-Watson kernel smoother of a squared innovation series `e`, with
# leave-one-out cross-validated bandwidth over search range [1/(2T), 1/6]
# (footnote 2), imposing K(0) = 0 in the CV objective. Shared core of
# kernel_spot_vol() below (e = diff(y)) and the WLS bubble-dating
# volatility correction in radf_pdc.R (e = step-1 fitted regime residuals).
nw_spot_vol <- function(e, kernel = c("gaussian", "uniform"), h = NULL) {
  kernel <- match.arg(kernel)
  Tn <- length(e)
  s <- (2:Tn) / Tn # i/T for i = 2..T (e_1 has no predecessor to pair against)
  e2 <- e[-1]^2 # e_i^2 for i = 2..T, aligned with s
  t_grid <- (1:Tn) / Tn

  kern <- switch(kernel,
    gaussian = function(u) dnorm(u),
    uniform = function(u) as.numeric(abs(u) <= 1) / 2
  )

  spot_vol_at <- function(h, drop0 = FALSE) {
    vapply(seq_along(t_grid), function(j) {
      w <- kern((s - t_grid[j]) / h)
      if (drop0) {
        self <- which(abs(s - t_grid[j]) < .Machine$double.eps^0.5)
        w[self] <- 0
      }
      if (sum(w) <= 0) return(mean(e2))
      sum(w * e2) / sum(w)
    }, numeric(1))
  }

  if (is.null(h)) {
    hl <- 1 / (2 * Tn)
    hu <- 1 / 6
    grid <- exp(seq(log(hl), log(hu), length.out = 10))
    cv <- vapply(grid, function(hh) {
      s2_loo <- spot_vol_at(hh, drop0 = TRUE)
      mean((e2 - s2_loo[match(s, t_grid)])^2)
    }, numeric(1))
    h <- grid[which.min(cv)]
  }

  sigma2 <- spot_vol_at(h, drop0 = FALSE)
  list(sigma2 = sigma2, h = h)
}

# Nonparametric spot-volatility estimator, eq. (6): nw_spot_vol() applied to
# the squared first differences of the raw (undemeaned) series.
kernel_spot_vol <- function(y, kernel = c("gaussian", "uniform"), h = NULL) {
  nw_spot_vol(diff(y), kernel = kernel, h = h)
}

# Feasible BZ statistic family, eq. (6)'s "feasible version" (Section 3):
# a WLS (1/sigma2-weighted) no-intercept recursive Dickey-Fuller stat on the
# GLS-demeaned series y_check_t = y_t - y_1. `sigma2` has length T (=
# length(y) - 1), one spot-variance estimate per differenced observation.
wls_dfstat_grid <- function(y, sigma2, minw) {
  yc <- y - y[1]
  n1 <- length(yc) - 1L
  dy <- diff(yc)
  ylag <- yc[1:n1]
  w <- 1 / sigma2

  csxx <- c(0, cumsum(w * ylag^2))
  csxy <- c(0, cumsum(w * ylag * dy))

  b_idx <- minw:n1
  a_idx <- 1:n1

  sxx <- outer(csxx[b_idx + 1], csxx[a_idx], "-")
  sxy <- outer(csxy[b_idx + 1], csxy[a_idx], "-")
  L <- outer(b_idx, a_idx, "-") + 1L

  valid <- L >= minw
  sxx[!valid] <- NA_real_
  tstat <- sxy / sqrt(sxx)
  valid <- valid & is.finite(tstat)
  tstat[!valid] <- NA_real_

  badf <- tstat[, 1]
  adf <- badf[length(badf)]
  sadf <- max(badf, na.rm = TRUE)

  tstat_filled <- tstat
  tstat_filled[!valid] <- -Inf
  best_col <- max.col(tstat_filled, ties.method = "first")
  bsadf <- tstat_filled[cbind(seq_along(best_col), best_col)]
  gsadf <- max(bsadf)

  list(badf = badf, bsadf = bsadf, adf = adf, sadf = sadf, gsadf = gsadf)
}

#' SBZ Weighted Least Squares Bubble Test with Union-of-Rejections
#'
#' \code{radf_sbz_cv} performs the HLST (2016) wild bootstrap -- the same
#' algorithm as \code{\link{radf_wb_cv}} -- \emph{jointly} on the classic
#' sup-ADF statistic (\code{supDF}, i.e. \code{radf()}'s \code{sadf}) and the
#' WLS/kernel-volatility statistic \code{supBZ} of Harvey, Leybourne & Zu
#' (2019), and combines them into the paper's union-of-rejections statistic
#' \code{U}. supBZ can have substantially higher power than supDF under
#' many time-varying-volatility patterns, at the cost of lower power under
#' others (e.g. upward volatility trends); \code{U} is designed to capture
#' whichever of the two is more powerful for a given series.
#'
#' @inheritParams radf_wb_cv
#' @param kernel Kernel for the spot-volatility estimator (eq. 6),
#' \code{"gaussian"} (default, as in the paper) or \code{"uniform"}.
#' @param h Bandwidth for the spot-volatility estimator. Default: leave-one-out
#' cross-validation over the paper's own search range.
#'
#' @return A list with bootstrap p-values (\code{p_supDF}, \code{p_supBZ},
#' \code{p_U}) and critical values (\code{supDF_cv}, \code{supBZ_cv},
#' \code{U_cv}) for each series.
#'
#' @references Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing
#' explosive bubbles with time-varying volatility. Econometric Reviews,
#' 38(10), 1131-1151.
#'
#' @seealso \code{\link{radf_wb_cv}} for the underlying (supDF-only) wild
#' bootstrap, and \code{\link{radf_tt}} for a bootstrap-free
#' heteroskedasticity-robust alternative.
#'
#' @export
radf_sbz_cv <- function(data, minw = NULL, nboot = 499L, kernel = c("gaussian", "uniform"),
                         h = NULL, seed = NULL) {
  kernel <- match.arg(kernel)
  y <- parse_data(data)
  assert_na(y)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(nboot, greater_than = 2)

  nc <- ncol(y)
  snames <- colnames(y)
  pcnt <- c(0.9, 0.95, 0.99)

  supDF_cv <- supBZ_cv <- U_cv <- matrix(NA_real_, nc, 3, dimnames = list(snames, paste0(pcnt * 100, "%")))
  p_supDF <- p_supBZ <- p_U <- setNames(numeric(nc), snames)
  supDF_obs <- supBZ_obs <- U_obs <- setNames(numeric(nc), snames)

  set_rng(seed)
  for (j in 1:nc) {
    yj <- y[, j, drop = TRUE]
    vol <- kernel_spot_vol(yj, kernel = kernel, h = h)

    supDF_obs[j] <- rls_gsadf(unroot(yj), min_win = minw)[length(yj) - minw + 2]
    supBZ_obs[j] <- wls_dfstat_grid(yj, vol$sigma2, minw)$sadf

    boot_df <- boot_bz <- numeric(nboot)
    for (b in 1:nboot) {
      ystar <- radf_wb_dgp_hlst(yj, dist_rad = FALSE)
      pointer <- length(ystar) - minw
      boot_df[b] <- rls_gsadf(unroot(ystar), min_win = minw)[pointer + 2]
      boot_bz[b] <- wls_dfstat_grid(ystar, vol$sigma2, minw)$sadf
    }

    supDF_cv[j, ] <- quantile_narm(boot_df, pcnt)
    supBZ_cv[j, ] <- quantile_narm(boot_bz, pcnt)
    p_supDF[j] <- mean(boot_df > supDF_obs[j])
    p_supBZ[j] <- mean(boot_bz > supBZ_obs[j])

    # Union statistic (Section 2.3): U = max(supDF, (qDF/qBZ) * supBZ),
    # using the bootstrap's own 95% quantiles for the qDF/qBZ scaling ratio.
    ratio <- supDF_cv[j, "95%"] / supBZ_cv[j, "95%"]
    U_boot <- pmax(boot_df, ratio * boot_bz)
    U_obs[j] <- max(supDF_obs[j], ratio * supBZ_obs[j])
    U_cv[j, ] <- quantile_narm(U_boot, pcnt)
    p_U[j] <- mean(U_boot > U_obs[j])
  }

  list(
    supDF = supDF_obs, supBZ = supBZ_obs, U = U_obs,
    supDF_cv = supDF_cv, supBZ_cv = supBZ_cv, U_cv = U_cv,
    p_supDF = p_supDF, p_supBZ = p_supBZ, p_U = p_U
  ) %>%
    add_attr(
      series_names = snames, method = "Wild Bootstrap (SBZ)", n = nrow(y),
      minw = minw, iter = nboot, kernel = kernel
    ) %>%
    add_class("radf_sbz")
}

#' @export
print.radf_sbz <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_sbz (minw = {get_minw(x)}, nboot = {get_iter(x)})"))
  cat_line()
  print(
    data.frame(
      series = names(x$supDF), supDF = x$supDF, supBZ = x$supBZ, U = x$U,
      p_supDF = x$p_supDF, p_supBZ = x$p_supBZ, p_U = x$p_U, row.names = NULL
    ),
    digits = digits, print.gap = 2L, row.names = FALSE
  )
  cat_line()
}
