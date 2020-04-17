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
#' @examples
#' \dontrun{
#' # Simulate bubble processes, compute the t-stat and critical values
#' rsim_data <- radf(sim_data)
#'
#' # Summary, diagnostics and datestamp (default)
#' summary(rsim_data)
#' diagnostics(rsim_data)
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
#' wb <- wb_cv(rsim_data)
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

  x <- object
  y <- cv

  ret <- list()
  if (is_wb(y)) {
    for (i in seq_along(series_names(x))) {
      df1 <- c(x$adf[i], y$adf_cv[i, ])
      df2 <- c(x$sadf[i], y$sadf_cv[i, ])
      df3 <- c(x$gsadf[i], y$gsadf_cv[i, ])
      df <- data.frame(
        rbind(df1, df2, df3),
        row.names = c("ADF", "SADF", "GSADF")
      )
      colnames(df) <- c("t-stat", "90%", "95%", "99%")
      ret[[i]] <- df
    }
    names(ret) <- series_names(x)
  } else if (is_mc(y)) {
    for (i in seq_along(series_names(x))) {
      df1 <- c(x$adf[i], y$adf_cv)
      df2 <- c(x$sadf[i], y$sadf_cv)
      df3 <- c(x$gsadf[i], y$gsadf_cv)
      df <- data.frame(
        rbind(df1, df2, df3),
        row.names = c("ADF", "SADF", "GSADF")
      )
      colnames(df) <- c("tstat", "90%", "95%", "99%")
      ret[[i]] <- df
    }
    names(ret) <- series_names(x)
  } else if (is_sb(y)) {
    ret <- cbind(x$gsadf_panel, t(y$gsadf_panel_cv))
    colnames(ret) <- c("t-stat", "90%", "95%", "99%")
  }

 ret %>%
   add_attr(
     minw = get_minw(x),
     lag = get_lag(x),
     method = get_method(y),
     iter = get_iter(y)
   ) %>%
   add_class("summary.radf")

}

#' @importFrom glue glue
#' @export
print.summary.radf <- function(x, digits = max(3L, getOption("digits") - 3L),
                             ...) {

  iter_char <- if (is_mc(x)) "nrep" else "nboot"
  cat_line()
  cat_rule(
    left = glue("Summary (minw = {get_minw(x)}, lag = {get_lag(x)})"),
    right = glue("{get_method(x)} ({iter_char} = {get_iter(x)})")
  )
  if (is_sb(x)) {
    cat("\n Panel\n")
    pp <- x[1, , drop = FALSE]
    rownames(pp) <- "GSADF"
    print(format(pp, digits = digits), print.gap = 2L, quote = FALSE)
  } else {
    # cat_line()
    for (i in seq_along(x)) {
      cat("\n", names(x)[i], "\n")
      print(format(x[[i]], digits = digits), print.gap = 2L)
    }
  }
  cat_line()
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




