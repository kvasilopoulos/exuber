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
#
# Three exported functions, split 2026-08-22 (originally one bundled
# radf_sbz_cv() doing statistic + cv in one call, which didn't fit the
# radf_obj/radf_cv convention any other _cv() function follows):
#   radf_sbz()       -- supBZ only, a radf_obj (like radf_tt()/radf_sign()).
#   radf_sbz_cv()     -- supBZ's own wild bootstrap cv, incl. the
#                        time-varying badf_cv/bsadf_cv boundary.
#   radf_sbz_union()  -- the original bundled supDF+supBZ+U union test;
#                        can't be split further, U's own definition needs
#                        the paired bootstrap draws (see its own roxygen).

# Nadaraya-Watson kernel smoother of a squared innovation series `e`, with
# leave-one-out cross-validated bandwidth over search range [1/(2T), 1/6]
# (footnote 2), imposing K(0) = 0 in the CV objective. Shared core of
# kernel_spot_vol() below (e = diff(y)) and the WLS bubble-dating
# volatility correction in dating_pdc.R (e = step-1 fitted regime residuals).
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

#' WLS/Kernel-Volatility Bubble Statistic (SBZ)
#'
#' \code{radf_sbz} computes the WLS (kernel-volatility-weighted) recursive
#' sup-ADF statistic of Harvey, Leybourne & Zu (2019) -- \code{supBZ} in
#' their own notation -- via \code{wls_dfstat_grid()} (internal),
#' returning the same shape \code{\link{radf}} itself does
#' (\code{adf}/\code{sadf}/\code{gsadf} scalars plus the full
#' \code{badf}/\code{bsadf} recursive paths), so it carries the
#' \code{radf_obj} class and the full \code{summary()}/
#' \code{\link{datestamp}}/\code{tidy}/\code{autoplot} pipeline works,
#' paired with \code{\link{radf_sbz_cv}}.
#'
#' Unlike the bundled \code{\link{radf_sbz_union}} (which combines this with
#' the classic \code{supDF} statistic into a bootstrap-calibrated union
#' test), \code{supBZ} alone needs no bootstrap to be \emph{defined} -- only
#' to be tested -- so it splits into a statistic and a critical-value
#' function the way most of exuber does.
#'
#' @inheritParams radf
#' @param kernel Kernel for the spot-volatility estimator (eq. 6 of Harvey,
#' Leybourne & Zu 2019), \code{"gaussian"} (default, as in the paper) or
#' \code{"uniform"}.
#' @param h Bandwidth for the spot-volatility estimator. Default: leave-one-out
#' cross-validation over the paper's own search range.
#'
#' @return An object of class \code{radf_sbz_obj}/\code{radf_obj}: a list
#' with \code{adf}, \code{sadf}, \code{gsadf} (one value per series) and
#' \code{badf}, \code{bsadf} (matrices, one column per series).
#'
#' @references Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing
#' explosive bubbles with time-varying volatility. Econometric Reviews,
#' 38(10), 1131-1151.
#'
#' @seealso \code{\link{radf_sbz_cv}} for critical values, and
#' \code{\link{radf_sbz_union}} for the paper's own headline bootstrap
#' union-of-rejections test against the classic \code{supDF} statistic.
#'
#' @note Needs \code{\link{radf_sbz_cv}} for critical values, not
#' \code{\link{radf_wb_cv}} or \code{\link{radf_mc_cv}} -- \code{supBZ}'s
#' own null distribution depends on the WLS weighting, so it needs its own
#' (data-dependent, wild-bootstrap) critical value function, same reasoning
#' as \code{radf()}/\code{\link{radf_wb_cv}}.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- radf_sbz(sim_data, minw = 20)
#' print(res)
#'
#' cv <- radf_sbz_cv(sim_data, minw = 20, nboot = 200)
#' summary(res, cv = cv)
#' tidy(res, cv = cv)
#'
#' # datestamp()/autoplot() need at least one rejection; supBZ's
#' # kernel-volatility weighting trades away enough power that none of
#' # sim_data's five series clear it, so use a series built to reject:
#' set.seed(7)
#' n <- 120; te <- 70
#' y <- cumsum(rnorm(n))
#' y[(te + 1):n] <- y[te] * 1.15 ^ seq_len(n - te)
#' res2 <- radf_sbz(y, minw = 20)
#' cv2 <- radf_sbz_cv(y, minw = 20, nboot = 100, seed = 1)
#' datestamp(res2, cv = cv2)
#' autoplot(res2, cv = cv2)
#' }
#'
#' @importFrom stats setNames
#' @export
radf_sbz <- function(data, minw = NULL, kernel = c("gaussian", "uniform"), h = NULL) {
  kernel <- match.arg(kernel)
  x <- parse_data(data)
  assert_na(x)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)

  nc <- ncol(x)
  snames <- colnames(x)
  n <- nrow(x)
  n_minw <- n - minw

  badf <- bsadf <- matrix(NA_real_, n_minw, nc, dimnames = list(NULL, snames))
  adf <- sadf <- gsadf <- setNames(numeric(nc), snames)

  for (j in seq_len(nc)) {
    yj <- as.numeric(x[, j])
    vol <- kernel_spot_vol(yj, kernel = kernel, h = h)
    res <- wls_dfstat_grid(yj, vol$sigma2, minw)
    badf[, j] <- res$badf
    bsadf[, j] <- res$bsadf
    adf[j] <- res$adf
    sadf[j] <- res$sadf
    gsadf[j] <- res$gsadf
  }

  list(adf = adf, badf = badf, sadf = sadf, bsadf = bsadf, gsadf = gsadf) %>%
    add_attr(
      mat = x, index = index(x), series_names = snames, minw = minw,
      n = n, lag = 0L, kernel = kernel
    ) %>%
    add_class("radf_sbz_obj", "radf_obj")
}

#' Wild Bootstrap Critical Values for the SBZ Statistic
#'
#' \code{radf_sbz_cv} performs the HLST (2016) wild bootstrap -- the same
#' algorithm as \code{\link{radf_wb_cv}}, applied to \code{\link{radf_sbz}}'s
#' WLS/kernel-volatility statistic instead of the classic \code{supDF} one --
#' to generate critical values, including the time-varying
#' \code{badf_cv}/\code{bsadf_cv} boundary \code{\link{datestamp}}/
#' \code{autoplot} need, not just the three scalar critical values
#' \code{summary()} uses.
#'
#' @inheritParams radf_sbz
#' @inheritParams radf_wb_cv
#'
#' @return An object of class \code{radf_cv}/\code{sbz_cv}/\code{wb_cv}: a
#' list with critical values \code{adf_cv}, \code{sadf_cv}, \code{gsadf_cv}
#' (one row per series) and \code{badf_cv}, \code{bsadf_cv} (one array per
#' series, one row per recursion point).
#'
#' @references Harvey, D. I., Leybourne, S. J., & Zu, Y. (2019). Testing
#' explosive bubbles with time-varying volatility. Econometric Reviews,
#' 38(10), 1131-1151.
#'
#' @seealso \code{\link{radf_sbz}} for the statistic this pairs with, and
#' \code{\link{radf_sbz_union}} for the bundled union-of-rejections test
#' against the classic \code{supDF} statistic (not obtainable from this
#' function and \code{\link{radf_wb_cv}} independently -- see that
#' function's Details for why).
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @export
radf_sbz_cv <- function(data, minw = NULL, nboot = 499L, kernel = c("gaussian", "uniform"),
                         h = NULL, seed = NULL) {
  kernel <- match.arg(kernel)
  x <- parse_data(data)
  assert_na(x)
  minw <- minw %||% psy_minw(data)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(nboot, greater_than = 2)

  nc <- ncol(x)
  snames <- colnames(x)
  n <- nrow(x)
  n_minw <- n - minw
  pcnt <- c(0.9, 0.95, 0.99)
  pcnt_names <- paste0(pcnt * 100, "%")

  adf_cv <- sadf_cv <- gsadf_cv <-
    matrix(NA_real_, nc, 3, dimnames = list(snames, pcnt_names))
  badf_cv <- bsadf_cv <-
    array(NA_real_, dim = c(n_minw, 3, nc), dimnames = list(NULL, pcnt_names, snames))

  set_rng(seed)
  for (j in seq_len(nc)) {
    yj <- as.numeric(x[, j])
    vol <- kernel_spot_vol(yj, kernel = kernel, h = h)

    boot_adf <- boot_sadf <- boot_gsadf <- numeric(nboot)
    boot_badf <- boot_bsadf <- matrix(NA_real_, n_minw, nboot)
    for (b in seq_len(nboot)) {
      ystar <- radf_wb_dgp_hlst(yj, dist_rad = FALSE)
      res <- wls_dfstat_grid(ystar, vol$sigma2, minw)
      boot_adf[b] <- res$adf
      boot_sadf[b] <- res$sadf
      boot_gsadf[b] <- res$gsadf
      boot_badf[, b] <- res$badf
      boot_bsadf[, b] <- res$bsadf
    }

    adf_cv[j, ] <- quantile_narm(boot_adf, pcnt)
    sadf_cv[j, ] <- quantile_narm(boot_sadf, pcnt)
    gsadf_cv[j, ] <- quantile_narm(boot_gsadf, pcnt)
    badf_cv[, , j] <- t(apply(boot_badf, 1, quantile_narm, probs = pcnt))
    bsadf_cv[, , j] <- t(apply(boot_bsadf, 1, quantile_narm, probs = pcnt))
  }

  list(
    adf_cv = adf_cv, sadf_cv = sadf_cv, gsadf_cv = gsadf_cv,
    badf_cv = badf_cv, bsadf_cv = bsadf_cv
  ) %>%
    add_attr(
      index = index(x), series_names = snames, method = "Wild Bootstrap (SBZ)",
      n = n, minw = minw, iter = nboot, kernel = kernel, seed = get_rng_state(seed)
    ) %>%
    add_class("radf_cv", "sbz_cv", "wb_cv")
}

#' SBZ Weighted Least Squares Bubble Test with Union-of-Rejections
#'
#' \code{radf_sbz_union} performs the HLST (2016) wild bootstrap -- the same
#' algorithm as \code{\link{radf_wb_cv}} -- \emph{jointly} on the classic
#' sup-ADF statistic (\code{supDF}, i.e. \code{radf()}'s \code{sadf}) and the
#' WLS/kernel-volatility statistic \code{supBZ} of Harvey, Leybourne & Zu
#' (2019), and combines them into the paper's union-of-rejections statistic
#' \code{U}. supBZ can have substantially higher power than supDF under
#' many time-varying-volatility patterns, at the cost of lower power under
#' others (e.g. upward volatility trends); \code{U} is designed to capture
#' whichever of the two is more powerful for a given series.
#'
#' \code{U}'s value itself -- not just its significance -- is defined using
#' a bootstrap-calibrated scaling ratio between \code{supDF} and
#' \code{supBZ}'s own 95\% critical values (the paper's Section 2.3), and
#' the joint bootstrap needs \code{supDF}/\code{supBZ} computed from the
#' \emph{same} resampled series each replicate for the union's size
#' guarantee (the paper's Theorem 3) to hold. That coupling is why this
#' stays one bundled function rather than splitting into a statistic and a
#' critical-value function the way most of exuber does -- unlike \code{U},
#' \code{supBZ} alone has no such coupling, so it does split that way: see
#' \code{\link{radf_sbz}}/\code{\link{radf_sbz_cv}} for the supBZ-only
#' route, with the usual \code{summary()}/\code{\link{datestamp}}/
#' \code{tidy}/\code{autoplot} pipeline.
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
#' bootstrap, \code{\link{radf_sbz}}/\code{\link{radf_sbz_cv}} for the
#' supBZ-only route with full pipeline support, and \code{\link{radf_tt}}
#' for a bootstrap-free heteroskedasticity-robust alternative.
#'
#' @note This function bundles the statistic and its critical values in a
#' single call -- there is no separate un-cv'd statistic function and no
#' other critical-value function to pair it with, unlike \code{radf()}/
#' \code{radf_wb_cv()} (\code{U}'s own value structurally requires the
#' bootstrap, see Details).
#'
#' @note Returns its own class (not `radf_obj`), so it does not plug into
#' `summary()`/`\link{datestamp}`/`tidy`/`autoplot` -- prints its own
#' statistic/critical-value summary (bundles the test statistic and its
#' critical value in one object), but has its own \code{autoplot} method
#' (a per-series comparison of \code{supDF}/\code{supBZ}/\code{U} against
#' their critical values) -- see \code{vignette("naming-and-analysis",
#' package = "exuber")} for the full picture of which functions do and
#' don't fit the shared pipeline.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' \donttest{
#' res <- radf_sbz_union(sim_data, nboot = 200)
#' print(res)
#' autoplot(res)
#' }
#'
#' @export
radf_sbz_union <- function(data, minw = NULL, nboot = 499L, kernel = c("gaussian", "uniform"),
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
    add_class("radf_sbz_union")
}

#' @export
print.radf_sbz_union <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue("radf_sbz_union (minw = {get_minw(x)}, nboot = {get_iter(x)})"))
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

#' @rdname radf_sbz_union
#' @importFrom ggplot2 autoplot ggplot aes geom_point geom_segment facet_wrap labs scale_color_manual
#' @param object A \code{radf_sbz_union} object.
#' @param sig_lvl Significance level to plot the critical value at, one of
#' \code{90}, \code{95} (default), \code{99}.
#' @param ... Further arguments passed to methods. Not used.
#' @export
autoplot.radf_sbz_union <- function(object, sig_lvl = 95, ...) {
  stopifnot(sig_lvl %in% c(90, 95, 99))
  col <- paste0(sig_lvl, "%")
  snames <- names(object$supDF)

  df <- data.frame(
    series = rep(snames, 3),
    stat = factor(rep(c("supDF", "supBZ", "U"), each = length(snames)),
                  levels = c("U", "supBZ", "supDF")),
    value = c(object$supDF, object$supBZ, object$U),
    crit = c(object$supDF_cv[, col], object$supBZ_cv[, col], object$U_cv[, col])
  )
  df$reject <- df$value > df$crit

  ggplot(df, aes(y = stat)) +
    geom_segment(aes(x = crit, xend = value, yend = stat), color = "grey60") +
    geom_point(aes(x = crit), shape = 4, color = "grey40") +
    geom_point(aes(x = value, color = reject), size = 3) +
    scale_color_manual(values = c(`TRUE` = "#d7263d", `FALSE` = "#1b6ca8"), guide = "none") +
    facet_wrap(~series) +
    labs(
      x = NULL, y = NULL,
      title = glue("SBZ union test (nboot = {get_iter(object)}, level = {sig_lvl}%)"),
      subtitle = "Dot = statistic, x = critical value; red = rejects, blue = does not"
    ) +
    theme_exuber()
}
