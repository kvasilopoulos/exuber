#' Plotting and tidying radf objects
#'
#' \code{autoplot.radf} takes an \code{radf} object and returns a facetted ggplot object.
#' \code{shade}
#'
#' @inheritParams datestamp.radf
#'
#' @param include_rejected If not FALSE, plot all variables regardless of rejecting the NULL at the 5 percent significance level.
#' @param select_series If not NULL, only plot with names or column number matching this regular expression will be executed.
#' @param shade_opt options for the shading of the graph, usually used through \code{shade} functions.
#' @param ... further arguments passed to \code{ggplot2::facet_wrap} and \code{ggplot2::geom_rect} for \code{shade}.
#' @param include argument name is depracated and substituted with `include_rejected`.
#' @param select argument name is depracated and substituted with `select_series`.
#'
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @importFrom rlang dots_list
#'
#' @export
#'
#' @examples
#'\donttest{
#' autoplot(radf_dta)
#'
#' # Modify facet_wrap options through ellipsis
#' autoplot(radf_dta, scales = "free", direction  = "v")
#'
#' autoplot(radf_dta, shade_opt = shade(shade_color = "pink", opacity = 0.3))
#'
#' # Change (overwrite) color, size or linetype
#' autoplot(radf_dta) +
#'   scale_color_manual(values = c("black", "black")) +
#'   scale_size_manual(values = c(1.2, 1)) +
#'   scale_linetype_manual(values = c("solid", "solid"))
#'
#' # Change names through ellipsis
#' custom_labels <- c("psy1" = "new_name_for_psy1", "psy2" = "new_name_for_psy2")
#' autoplot(radf_dta, labeller = labeller(id = as_labeller(custom_labels)))
#'
#' # Change Theme options
#' autoplot(radf_dta) +
#'   theme(legend.position = "right")
#'}
autoplot.radf <- function(object, cv = NULL, include_rejected = FALSE,
                          select_series = NULL, option = c("gsadf", "sadf"),
                          shade_opt = shade(),
                          include = NULL, select = NULL, ...) {

  deprecate_arg_warn(include, include_rejected)
  deprecate_arg_warn(select, select_series)

  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "cv")
  option <- match.arg(option)
  option <- if (option == "gsadf") "bsadf" else if (option == "sadf") "badf"
  acc_series <- if (include_rejected) {
    series_names(object)
  } else {
    diagnostics(object, cv)$accepted
  }

  sel_series <- select_series %||% series_names(object)
  series <- intersect(acc_series, sel_series)
  if (rlang::is_bare_character(acc_series, n = 0)) {
    stop_glue("available series are not acceptable for plotting")
  }
  dots <- rlang::dots_list(...)
  gg <- augment_join(object, cv) %>%
    filter(id %in% series, sig == 0.95, name == option) %>%
    droplevels() %>%
    pivot_longer(data = ., cols = c("tstat", "crit"), names_to = "stat") %>%
    ggplot(aes(index, value, col = stat, size = stat, linetype = stat)) +
    geom_line() +
    scale_color_manual(values = c("red", "blue")) +
    scale_size_manual(values = c(0.8, 0.7)) +
    scale_linetype_manual(values = c(2, 1)) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "grey20"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", size = 0.7),
      strip.background = element_blank(),
      strip.text.x = element_text(hjust = 0, size = rel(1.5)),
      axis.title = element_blank(),
      legend.key = element_blank(),
      legend.position = "none"
    )
  if (!is.null(shade_opt)) {
    ds_data <- tidy(datestamp(object, cv)) %>%
      filter(id %in% series) %>%
      droplevels()
    gg <- gg + shade_opt(ds_data)
  }
  gg +
    {if (is.null(dots$scales)) facet_wrap( ~ id, scales = "free", ...)
      else facet_wrap( ~ id, ...)}
}


#' @rdname autoplot.radf
#' @param min_duration the minimum duration.
#' @param shade_color the shade color that indicates the exuberance periods. Defaults to grey70.
#' @param opacity the opacity of the shade color aka alpha.
#' @export
shade <- function(min_duration = NULL, shade_color = "grey70", opacity = 0.5, ...) {
  function(ds_data) {
  filter(ds_data, Duration >= min_duration %||% 0) %>%
    geom_rect(
      data = ., inherit.aes = FALSE, fill = shade_color, alpha = opacity,
      aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf), ...
    )
  }
}
