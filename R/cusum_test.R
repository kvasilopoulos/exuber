# Kurozumi & Nishi (2025, JTSA 46(5), 945-965; "KN") -- the retrospective
# CUSUM and CUSUM-of-squares statistics of their Section 3.1 (Brown et al.
# 1975's parameter-constancy tests, applied to Delta y_t), companions to
# ssu_test()'s SSU/GSSU. See docs/volatility-robustness.md, "Stochastic
# explosive-coefficient test".
#
# Every statistic is a sup/inf of a partial-sum process, so the
# "generalized" (every window start) versions reduce to a running max/min
# -- O(T) total, no double loop:
#   CS    = max_k S_k,              S_k = sum_{t<=k} Delta y_t / (sigma sqrt(T))
#   GCS   = max_{j<k} (S_k - S_j)
#   CSSQ  = max_k D_k, min_k D_k,   D_k = (sum_{t<=k} (Delta y_t)^2 - k/T sum (Delta y_t)^2)
#                                          / (sigma_eta sqrt(T))
#   GCSSQ = max_{j<k} (D_k - D_j), min_{j<k} (D_k - D_j)
# with sigma^2 = mean((Delta y)^2), sigma_eta^2 = mean((Delta y)^4) -
# sigma^4 (page 7, not demeaned). CS/GCS reject on the right tail; CSSQ/
# GCSSQ are two-sided, each tail at alpha/2. All critical values are
# Table I's published asymptotic ones (kn_table in ssu_test.R).

cusum_test_path <- function(y, type) {
  d <- diff(y)
  nd <- length(d)
  if (type %in% c("cs", "gcs")) {
    p <- c(0, cumsum(d)) / sqrt(mean(d^2) * nd)
  } else {
    d2 <- d^2
    p <- c(0, cumsum(d2) - seq_len(nd) / nd * sum(d2)) / sqrt((mean(d2^2) - mean(d2)^2) * nd)
  }
  k <- 2:(nd + 1L) # p[k] <-> k - 1 increments summed; p[1] = 0 is the start
  switch(type,
    cs = list(sup = p[k]),
    gcs = list(sup = p[k] - cummin(p)[k - 1L]),
    cssq = list(sup = p[k], inf = p[k]),
    gcssq = list(sup = p[k] - cummin(p)[k - 1L], inf = p[k] - cummax(p)[k - 1L])
  )
}

#' CUSUM and CUSUM-of-Squares Bubble Tests (Kurozumi & Nishi 2025)
#'
#' \code{cusum_test} implements the retrospective CUSUM (\code{"cs"}),
#' generalized CUSUM (\code{"gcs"}), CUSUM-of-squares (\code{"cssq"}) and
#' generalized CUSUM-of-squares (\code{"gcssq"}) tests of Kurozumi & Nishi
#' (2025), Brown et al.'s (1975) parameter-constancy statistics applied to
#' the first differences. The generalized versions take the supremum over
#' every window start as well as every end point.
#'
#' CS/GCS reject when the cumulated increments get too large (right tail).
#' CSSQ/GCSSQ are two-sided: they reject when the cumulated squared
#' increments drift too far above \emph{or} below their full-sample
#' average, each tail at half the level. The paper finds the CUSUM-type
#' tests lose almost all power once the explosive coefficient is
#' genuinely stochastic, while the CUSUM-SQ type keeps it -- see
#' \code{\link{ssu_test}} for the paper's more powerful statistics.
#'
#' @note All critical values are published asymptotic values (Kurozumi &
#' Nishi (2025)'s Table I); no minimum window is needed (the paper finds
#' the statistics insensitive to it).
#'
#' @inheritParams radf
#' @param sig_lvl Significance level on the package-wide 0-100 scale, one
#' of \code{90}, \code{95}, \code{99}.
#' @param type One of \code{"cs"}, \code{"gcs"}, \code{"cssq"},
#' \code{"gcssq"}.
#'
#' @return An object of class \code{cusum_test_obj}: a list with the
#' statistic path \code{stat} (one value per end point; for the
#' generalized versions the sup over window starts), \code{stat_inf} (the
#' inf path, CSSQ/GCSSQ only), the statistic \code{sup} (and \code{inf}),
#' the critical value(s) \code{crit}, and \code{detected}.
#'
#' @references Kurozumi, E., & Nishi, M. (2025). Bubble testing with
#' stochastically varying explosive coefficient. Journal of Time Series
#' Analysis, 46(5), 945-965.
#'
#' Brown, R. L., Durbin, J., & Evans, J. M. (1975). Techniques for testing
#' the constancy of regression relationships over time. Journal of the
#' Royal Statistical Society B, 37(2), 149-192.
#'
#' @seealso \code{\link{ssu_test}}; \code{\link{monitor_cusum}} for
#' real-time CUSUM monitoring.
#'
#' @section Status:
#' `r lifecycle::badge("experimental")`
#'
#' @examples
#' y <- sim_psy1(n = 150, te = 75, tf = 150, c = 3, alpha = 1, seed = 2001,
#'   coef_noise = rnorm(149), coef_a = 4)
#' cusum_test(y, type = "cssq")
#' autoplot(cusum_test(y, type = "gcssq"))
#'
#' @family volatility-robust tests
#' @export
cusum_test <- function(data, sig_lvl = 95, type = c("cs", "gcs", "cssq", "gcssq")) {
  type <- match.arg(type)
  x <- parse_data(data)
  n <- nrow(x)
  snames <- colnames(x)
  nc <- ncol(x)
  two_sided <- type %in% c("cssq", "gcssq")
  crit <- if (two_sided) {
    c(sup = ssu_q(sig_lvl, paste0(type, "_sup")), inf = ssu_q(sig_lvl, paste0(type, "_inf")))
  } else {
    ssu_q(sig_lvl, type)
  }

  stat <- stat_inf <- matrix(NA_real_, n - 1L, nc, dimnames = list(NULL, snames))
  for (j in seq_len(nc)) {
    p <- cusum_test_path(as.numeric(x[, j]), type)
    stat[, j] <- p$sup
    if (two_sided) stat_inf[, j] <- p$inf
  }
  sup <- apply(stat, 2, max)
  out <- list(stat = stat, sup = sup, crit = crit)
  if (two_sided) {
    inf <- apply(stat_inf, 2, min)
    out <- c(out, list(stat_inf = stat_inf, inf = inf, detected = sup >= crit["sup"] | inf <= crit["inf"]))
  } else {
    out$detected <- sup > crit
  }
  out$detected <- setNames(out$detected, snames)

  out %>%
    add_attr(index = index(x), series_names = snames, n = n, sig_lvl = sig_lvl, type = type) %>%
    add_class("cusum_test_obj")
}

#' Plot method for cusum_test() output
#'
#' Plots the CUSUM-type statistic path against its critical value(s), one
#' panel per series; the two-sided CUSUM-of-squares tests show the sup path
#' against the upper and the inf path against the lower critical value.
#'
#' @param object An object of class \code{cusum_test_obj}, the output of \code{\link{cusum_test}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A \link[ggplot2]{ggplot}
#' @seealso \code{\link{cusum_test}}
#' @export
autoplot.cusum_test_obj <- function(object, ...) {
  pos <- seq_len(nrow(object$stat)) + 1L
  ylab <- paste(toupper(attr(object, "type")), "statistic")
  gg <- autoplot_stat_boundary(pos, object$stat, object$crit[1L], ylab = ylab)
  if (is.null(object$stat_inf)) {
    return(gg)
  }
  snames <- colnames(object$stat_inf)
  lower <- tibble(
    index = rep(pos, length(snames)), id = factor(rep(snames, each = length(pos)), levels = snames),
    value = c(object$stat_inf)
  )
  gg +
    geom_line(data = lower, aes(index, value), inherit.aes = FALSE, color = "grey40") +
    geom_hline(yintercept = object$crit["inf"], color = "red", linetype = 2)
}

#' @export
print.cusum_test_obj <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat_line()
  cat_rule(left = glue(
    "cusum_test ({toupper(attr(x, 'type'))}, n = {attr(x, 'n')}, ",
    "sig_lvl = {attr(x, 'sig_lvl')}%, crit = {paste(x$crit, collapse = ' / ')})"
  ))
  cat_line()
  df <- data.frame(series = names(x$sup), sup = x$sup, row.names = NULL)
  if (!is.null(x$inf)) df$inf <- x$inf
  df$detected <- x$detected
  print(df, digits = digits, print.gap = 2L, row.names = FALSE)
  cat_line()
}
