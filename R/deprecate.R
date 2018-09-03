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
  .Deprecated("summary", package = "exuber")
  summary(object = x, cv = y, panel)
}

#' @rdname exuber-deprecated
plot.radf <- function(x, y) {
  .Deprecated("autoplot", package = "exuber")
}
