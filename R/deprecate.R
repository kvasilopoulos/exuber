# TODO depracated mc_cv wb_cv sb_cv

deprecate_arg_warn <- function(old, new) {
  if (!is.null(old) && old != "DEPRECATED")
    warning_glue("`{substitute(old)}` is deprecated. Please use `{substitute(new)}` instead.")
}

deprecate_arg_stop <- function(old, new) {
  if (!is.null(old) && old != "DEPRECATED")
    stop_glue("`{substitute(old)}` is deprecated. Please use `{substitute(new)}` instead.")
}


#' @title Deprecated functions in package \pkg{exuber}.
#'
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name exuber-deprecated
#' @keywords internal
#' @export
col_names <- function(x) {
  .Deprecated(new = "series_names()", package = "exuber")
  series_names(x)
}

#' @title Defunct functions in package \pkg{exuber}.
#'
#' @description The functions listed below are defunct. Help pages for defunct
#' functions are  available at \code{help("-defunct")}.
#' @name exuber-defunct
#' @keywords internal
#' @export
ggarrange <- function(...) {
  .Defunct(new = "do.call(gridExtra::grid.arrange, c(...))", package = "exuber")
}

#' @rdname exuber-defunct
fortify.radf_obj <- function(model, data, ...) {
  .Defunct(new = "tidy()", package = "exuber")
}

#' @rdname exuber-defunct
fortify.ds_radf <- function(model, data, ...) {
  .Defunct(new = "tidy()", package = "exuber")
}
