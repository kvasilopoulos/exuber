#' Summarizing radf models
#'
#' \code{summary} method for class "radf"
#'
#' @param object An object of class \code{\link[=radf]{radf()}}.
#' @param cv An object of class "cv". The output of \code{\link[=mc_cv]{mc_cv()}},
#' \code{\link[=wb_cv]{wb_cv()}} or \code{\link[=sb_cv]{sb_cv()}}
#' @param ... further arguments passed to methods, not used.
#'
#' @return Returns a list of summary statistics,
#' the t-statistic and the critical values of the ADF, SADF and GSADF.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter select
#' @examples
#' \donttest{
#' # Simulate bubble processes, compute the t-stat and critical values
#' rsim_data <- radf(sim_data)
#'
#' # Summary, diagnostics and datestamp (default)
#' summary(rsim_data)
#'
#' diagnostics(rsim_data)
#'
#' datestamp(rsim_data)
#'
#' #' # Diagnostics for 'sadf'
#' diagnostics(rsim_data, option = "sadf")
#'
#' # Use log(T)/T rule of thumb to omit periods of explosiveness which are short-lived
#' rot <- psy_ds(sim_data)
#' datestamp(rsim_data, min_duration = rot)
#'
#' # Summary, diagnostics and datestamp (Wild Bootstrapped critical values)
#'
#' wb <- wb_cv(sim_data)
#'
#' summary(rsim_data, cv = wb)
#'
#' diagnostics(rsim_data, cv = wb)
#'
#' datestamp(rsim_data, cv = wb)
#' }
#' @export
summary.radf <- function(object, cv = NULL, ...) {

  cv <- cv  %||% retrieve_crit(object)
  assert_class(cv, "cv")
  assert_match(object, cv)

  ret <- list()
  if (is_sb(cv)) {
    ret[["panel"]] <- glance_join(object, cv) %>%
      pivot_wider(names_from = sig, values_from = crit) %>%
      select(-id)
  } else{
    series <- series_names(object)
    sm <- tidy_join(object, cv) %>%
      pivot_wider(names_from = sig, values_from = crit)
    for (nms in series) {
      ret[[nms]] <- filter(sm, id == nms) %>%
        select(-id)
    }
  }

 ret %>%
   add_attr(
     minw = get_minw(object),
     lag = get_lag(object),
     method = get_method(cv),
     iter = get_iter(cv)
   ) %>%
   add_class("summary.radf")

}

#' @importFrom glue glue
#' @export
print.summary.radf <- function(x, ...) {

  iter_char <- if (is_mc(x)) "nrep" else "nboot"
  cat_line()
  cat_rule(
    left = glue("Summary (minw = {get_minw(x)}, lag = {get_lag(x)})"),
    right = glue("{get_method(x)} ({iter_char} = {get_iter(x)})")
  )
  cat_line()
  print.listof(x, ...)
}


#' Calculate p-values
#'
#' @param x An 'radf' object
#' @param distr Which type of distribution to use to calculate the p-values
#'
#' @export
#' @importFrom tidyr nest spread
#' @importFrom purrr when map2_dbl
#' @importFrom dplyr group_by
#' @examples
#'
#' \dontrun{
#' radf_psy1 <- radf(sim_psy1(100))
#'
#' calc_pvalue(radf_psy1)
#'
#' # Using the Wild-Bootstrapped
#' wb_psy1 <- wb_dist(sim_psy1(100))
#'
#' calc_pvalue(radf_psy1, wb_psy1)
#' }
calc_pvalue <- function(x, distr = NULL) {

  assert_class(x, "radf")
  if (is.null(distr)) {
    message_glue("Using 'mc_distr' for 'distr' argument.")
    distr <- mc_distr(attr(x, "n"), minw = attr(x, "minw"))
  }
  assert_class(distr, "distr"
               )
  if (is_sb(distr)) {
    tbl_x <- glance(x) %>%
      mutate(id = "panel", stat = "gsadf_panel") %>%
      nest(value_x = panel)
  } else{
    tbl_x <- tidy(x) %>%
      gather(stat, value_x, -id) %>%
      nest(value_x = value_x)
  }
  if (is_wb(distr)) {
    tbl_distr <- tidy(distr) %>%
      gather(., stat, value_y, -id) %>%
      group_by(stat) %>%
      nest(value_y = value_y)
    tbl_join_nested <-
      full_join(tbl_x, tbl_distr, by = c("id","stat"))
  }else{
    tbl_distr <- tidy(distr) %>%
      gather(., stat, value_y) %>%
      group_by(stat) %>%
      nest(value_y = value_y)
    tbl_join_nested <-
      full_join(tbl_x, tbl_distr, by = c("stat"))
  }

  xy_pvalue <- purrr::as_mapper(~ mean(unlist(.x) < unlist(.y)))

  tbl_join_nested %>%
    mutate(pval = map2_dbl(value_x, value_y, xy_pvalue)) %>%
    select(id, stat, pval) %>%
    spread(stat, pval) %>%
    when(is_sb(distr) ~ ., ~ select(., id, adf, sadf, gsadf)) %>%
    add_class("pval")
}

# TODO: format.pval()




