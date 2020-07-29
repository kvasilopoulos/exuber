#' @export
print.radf_cv <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  iter_char <- if (is_mc(x)) "nrep" else "nboot"
  lag_str <- if(is_sb(x)) paste0(", lag = ", get_lag(x)) else ""
  cat_line()
  cat_rule(
    left = glue("{get_method(x)} (minw = {get_minw(x)}, {iter_char} = {get_iter(x)}{lag_str})")
  )
  cat_line()

  print(format(as.data.frame(tidy(x)),
               digits = digits), print.gap = 2L, row.names = FALSE)
}
