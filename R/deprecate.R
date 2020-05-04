deprecate_arg_warn <- function(old, new) {
  if (!is.null(old) && old != "DEPRECATED")
    warning_glue("`{substitute(old)}` is deprecated. Please use `{substitute(new)}` instead.")
}

deprecate_arg_stop <- function(old, new) {
  if (!is.null(old) && old != "DEPRECATED")
    stop_glue("`{substitute(old)}` is deprecated. Please use `{substitute(new)}` instead.")
}

# Deprecated --------------------------------------------------------------


#' @title Deprecated functions in package \pkg{exuber}.
#'
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("exuber-deprecated")}.
#' @name exuber-deprecated
#' @keywords internal
#' @export
col_names <- function(x) {
  .Deprecated(new = "series_names()", package = "exuber")
  series_names(x)
}

#' @rdname exuber-deprecated
#' @export
mc_cv <- function(n, minw = NULL, nrep = 1000L, seed = NULL) {
  .Deprecated(new = "radf_mc_cv()", package = "exuber")
  radf_mc_cv(n, minw = minw, nrep = nrep, seed = seed)
}

#' @rdname exuber-deprecated
#' @export
wb_cv <- function(data, minw = NULL, nboot = 1000L, seed = NULL) {
  .Deprecated(new = "radf_wb_cv()", package = "exuber")
  radf_wb_cv(data, minw = minw, nboot = nboot, seed = seed)
}

#' @rdname exuber-deprecated
#' @export
sb_cv <- function(data, minw = NULL, nboot = 1000L, seed = NULL) {
  .Deprecated(new = "radf_sb_cv()", package = "exuber")
  radf_sb_cv(data, minw = minw, nboot = nboot, seed = seed)
}


# Defunct -----------------------------------------------------------------


#' @title Defunct functions in package \pkg{exuber}.
#'
#' @description The functions listed below are defunct. Help pages for defunct
#' functions are  available at \code{help("exuber-defunct")}.
#' @name exuber-defunct
#' @keywords internal
#' @export
ggarrange <- function(...) {
  .Defunct(new = "do.call(gridExtra::grid.arrange, c(...))", package = "exuber")
}


#' @rdname exuber-defunct
#' @export
fortify.radf_obj <- function(model, data, ...) {
  .Defunct(new = "tidy()", package = "exuber")
}

#' @rdname exuber-defunct
#' @export
fortify.ds_radf <- function(model, data, ...) {
  .Defunct(new = "tidy()", package = "exuber")
}

#' @rdname exuber-defunct
#' @export
report <- function(x, y, panel = FALSE, ...) {
  .Defunct(new = "summary()", package = "exuber")
}

#' @rdname exuber-defunct
#' @export
sim_dgp1 <- function(n, te = 0.4 * n, tf = 0.15 * n + te, c = 1,
                     alpha = 0.6, sigma = 6.79, seed = NULL) {
  .Defunct(new = "sim_psy1()", package = "exuber")
}

#' @rdname exuber-defunct
#' @export
sim_dgp2 <- function(n, te1 = 0.2 * n, tf1 = 0.2 * n + te1,
                     te2 = 0.6 * n, tf2 = 0.1 * n + te2,
                     c = 1, alpha = 0.6, sigma = 6.79, seed = NULL) {
  .Defunct(new = "sim_psy2()", package = "exuber")
}


