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
sim_dgp1 <- function(...) {
  .Deprecated(new = "sim_psy1()", package = "exuber")
  sim_psy1(...)
}

#' @rdname exuber-deprecated
sim_dgp2 <- function(...) {
  .Deprecated("sim_psy2()", package = "exuber")
  sim_psy2(...)
}
