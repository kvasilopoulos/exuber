#' Summarizing `radf` models
#'
#' \code{summary} method for radf models that consist of `radf_obj` and `radf_cv`.
#'
#' @param object An object of class `radf_obj`. The output of \code{\link[=radf]{radf()}}.
#' @param cv An object of class `radf_cv`. The output of \code{\link[=radf_mc_cv]{radf_mc_cv()}},
#' \code{\link[=radf_wb_cv]{radf_wb_cv()}} or \code{\link[=radf_sb_cv]{radf_sb_cv()}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return Returns a list of summary statistics, which include the estimated ADF,
#' SADF, and GSADF test statistics and the corresponding critical values
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter select
#' @name summary.radf_obj
#' @examples
#' \donttest{
#' # Simulate bubble processes, compute the test statistics and critical values
#' rsim_data <- radf(sim_data)
#'
#' # Summary, diagnostics and datestamp (default)
#' summary(rsim_data)
#'
#' #Summary, diagnostics and datestamp (wild bootstrap critical values)
#'
#' wb <- radf_wb_cv(sim_data)
#'
#' summary(rsim_data, cv = wb)
#'
#' }
#' @export
summary.radf_obj <- function(object, cv = NULL, ...) {

  cv <- cv  %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  assert_match(object, cv)
  ret <- summary_radf(cv, object, ...)

  ret %>%
    add_attr(
      minw = get_minw(object),
      lag = get_lag(object),
      method = get_method(cv),
      iter = get_iter(cv)
    ) %>%
    add_class("sm_radf", "sm")
}

summary_radf <- function(cv, ...){
  UseMethod("summary_radf")
}

summary_radf.sb_cv <- function(cv, object, ...) {
  ret <- list()
  ret[["panel"]] <- tidy_join(object, cv, panel = TRUE) %>%
    pivot_wider(names_from = sig, values_from = crit) %>%
    select(-id)
  ret
}

summary_radf.mc_cv <- summary_radf.wb_cv <- function(cv, object, ...) {
  ret <- list()
  snames <- series_names(object)
  sm <- tidy_join(object, cv) %>%
    pivot_wider(names_from = sig, values_from = crit)
  for (nms in snames) {
    ret[[nms]] <- filter(sm, id == nms) %>%
      select(-id)
  }
  ret
}

#' @importFrom glue glue
#' @export
print.sm_radf <- function(x, ...) {

  iter_char <- if (is_mc(x)) "nrep" else "nboot"
  cat_line()
  cat_rule(
    left = glue("Summary (minw = {get_minw(x)}, lag = {get_lag(x)})"),
    right = glue("{get_method(x)} ({iter_char} = {get_iter(x)})")
  )
  cat_line()
  print.listof(x, ...)
}


