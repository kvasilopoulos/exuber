# TODO revisit this function if you have time

#' Calculate p-values
#'
#'
#' Calculate p-values from `distr` object
#'
#' @param x A `radf_obj` object.
#' @param distr A `radf_distr` object.
#' @return A `tibble` with one row per series (`id`) and one p-value column
#'   per statistic (`adf`, `sadf`, `gsadf`; the panel statistic only when
#'   `distr` is a sieve-bootstrap distribution).
#'
#' @export
#' @importFrom tidyr nest spread
#' @importFrom purrr when map2_dbl
#' @importFrom dplyr mutate select group_by
#' @keywords internal
#' @examples
#' \donttest{
#' radf_psy1 <- radf(sim_psy1(100))
#'
#' # Default: p-values against a Monte Carlo null distribution
#' pv <- calc_pvalue(radf_psy1)
#' pv
#'
#' # Using the Wild-Bootstrapped null instead
#' wb_psy1 <- radf_wb_distr(sim_psy1(100))
#'
#' calc_pvalue(radf_psy1, wb_psy1)
#'
#' sb_psy1 <- radf_sb_distr(sim_data, nboot = 500)
#'
#' calc_pvalue(radf(sim_data), sb_psy1)
#'
#' # Plot the three p-values for this series
#' barplot(unlist(pv[, c("adf", "sadf", "gsadf")]), ylab = "p-value",
#'   main = "calc_pvalue(): rejects at the usual 5% level if the bar is short")
#' abline(h = 0.05, col = "red", lty = 2)
#' }
calc_pvalue <- function(x, distr = NULL) {
  assert_class(x, "radf_obj")
  if (is.null(distr)) {
    message_glue("Using `radf_mc_distr` for `distr`.")
    distr <- radf_mc_distr(attr(x, "n"), minw = attr(x, "minw"))
  }
  assert_class(distr, "radf_distr")
  if (is_sb(distr)) {
    tbl_x <- tidy(x, panel = TRUE) %>%
      mutate(id = "panel", stat = "gsadf_panel") %>%
      nest(value_x = gsadf_panel)
  } else {
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
      full_join(tbl_x, tbl_distr, by = c("id", "stat"))
  } else {
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
    {
      if (is_sb(distr)) . else select(., id, adf, sadf, gsadf)
    }
}
