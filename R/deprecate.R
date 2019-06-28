#' @title Deprecated functions in package \pkg{exuber}.
#'
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name exuber-deprecated
#' @keywords internal
#' @export
report <- function(x, y, panel = FALSE, ...) {
  .Deprecated("summary()", package = "exuber")
  summary(object = x, cv = y)
}

#' @rdname exuber-deprecated
plot.radf <- function(x, y, option = c("gsadf", "sadf"), min_duration = 0,
                      plot_type = c("multiple", "single")) {
  .Deprecated("autoplot()", package = "exuber")

  option <- match.arg(option)
  plot_type <- match.arg(plot_type)

  if (plot_type == "multiple") {
    x %>%
      autoplot(cv = y, option = option, min_duration = min_duration) %>%
      return()
  } else {
    x %>%
      datestamp(cv = y, option = option, min_duration = min_duration) %>%
      autoplot()
  }
}

#' @rdname exuber-deprecated
sim_dgp1 <- function(n, te = 0.4 * n, tf = 0.15 * n + te, c = 1,
                     alpha = 0.6, sigma = 6.79, seed = NULL) {
  .Deprecated(new = "sim_psy1()", package = "exuber")
  sim_psy1(n = n, te = te, tf = tf, c = c,
           alpha = alpha, sigma = sigma, seed = seed)
}

#' @rdname exuber-deprecated
sim_dgp2 <- function(n, te1 = 0.2 * n, tf1 = 0.2 * n + te1,
                     te2 = 0.6 * n, tf2 = 0.1 * n + te2,
                     c = 1, alpha = 0.6, sigma = 6.79, seed = NULL) {
  .Deprecated("sim_psy2()", package = "exuber")
  sim_psy2(n = n, te1 = te1, tf1 = tf1, te2 = te2, tf2 = tf2, c = c,
           alpha = alpha, sigma = sigma, seed = seed)
}
