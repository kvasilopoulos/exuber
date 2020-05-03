#' Plotting `radf` models
#'
#' \code{autoplot.radf_obj} takes \code{radf_obj} and \code{radf_cv} and returns a faceted ggplot object.
#' \code{shade} is used as an input to \code{shape_opt}. \code{shade} modifies the
#' geom_rect layer that demarcates the exuberance periods.
#'
#' @inheritParams datestamp.radf_obj
#'
#' @param include_negative If TRUE, plot all variables regardless of rejecting the NULL at the 5 percent significance level.
#' @param select_series 	A vector of column names or numbers specifying the series to be used in plotting.
#' Note that the order of the series does not alter the order used in plotting.
#' @param shade_opt Shading options, typically set using \code{shade} function.
#' @param ... Further arguments passed to \code{ggplot2::facet_wrap} and \code{ggplot2::geom_rect} for \code{shade}.
#' @param include Argument name is deprecated and substituted with `include_negative`.
#' @param select Argument name is deprecated and substituted with `select_series`.
#'
#' @return A [ggplot2::ggplot()]
#'
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @importFrom rlang dots_list is_missing is_null
#'
#' @export
#'
#' @examples
#' \donttest{
#' rsim_data <- radf(sim_data_wdate)
#'
#' autoplot(rsim_data)
#'
#' # Modify facet_wrap options through ellipsis
#' autoplot(rsim_data, scales = "free_y", dir  = "v")
#'
#' autoplot(rsim_data, shade_opt = shade(fill = "pink", opacity = 0.5))
#'
#' # We will need ggplot2 from here on out
#' library(ggplot2)
#'
#' # Change (overwrite) color, size or linetype
#' autoplot(rsim_data) +
#'   scale_color_manual(values = c("black", "black")) +
#'   scale_size_manual(values = c(0.9, 1)) +
#'   scale_linetype_manual(values = c("solid", "solid"))
#'
#' # Change names through labeller (first way)
#' custom_labels <- c("psy1" = "new_name_for_psy1", "psy2" = "new_name_for_psy2")
#' autoplot(rsim_data, labeller = labeller(.default = label_value, id = as_labeller(custom_labels)))
#'
#' # Change names through labeller (second way)
#' custom_labels2 <- series_names(rsim_data)
#' names(custom_labels2) <- custom_labels2
#' custom_labels2[c(3,5)] <- c("Evans", "Blanchard")
#' autoplot(rsim_data, labeller = labeller(id = custom_labels2))
#'
#' # Or change names before plotting
#' series_names(rsim_data) <- LETTERS[1:5]
#' autoplot(rsim_data)
#'
#' # Change Theme options
#' autoplot(rsim_data) +
#'   theme(legend.position = "right")
#'  }
autoplot.radf_obj <- function(object, cv = NULL,
                          option = c("gsadf", "sadf"),
                          min_duration = 0L,
                          select_series = NULL,
                          include_negative = FALSE,
                          shade_opt = shade(),
                          include = "DEPRECATED", select = "DEPRECATED", ...) {

  deprecate_arg_warn(include, include_negative)
  deprecate_arg_warn(select, select_series)
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  snames <- series_names(object)

  option <- match.arg(option)
  if (is_sb(cv)) {
    if (!is.null(select_series)) {
      stop_glue("argument 'select_series' have to be set to NULL ",
                "when cv is of class 'sb_cv'")
    }
    filter_option <- "bsadf_panel" # overwrite option
    select_series <- "panel"
  }else{
    filter_option <- if (option == "gsadf") "bsadf" else "badf"
  }

  pos_series <- if (include_negative) {
    if (is_sb(cv)) "panel" else snames
  } else {
    diagnostics_internal(object, cv)$positive # internal to make the check here
  }

  if (is.numeric(select_series)) {
    select_series <- snames[select_series]
  }
  sel_series <- select_series %||% snames
  not_exist <- sel_series %ni% c(snames, "panel")
  if (any(not_exist)) {
    stop_glue("The series '{sel_series[not_exist][1]}' doesn't exist.") # only the first
  }
  series <- intersect(pos_series, sel_series)
  if (rlang::is_bare_character(pos_series, n = 0)) {
    stop_glue("available series are not acceptable for plotting")
  }

  dots <- rlang::dots_list(...)
  plot_data <- augment_join(object, cv) %>%
    filter(id %in% series, sig == 95, name == filter_option) %>%
    droplevels() %>%
    pivot_longer(data = ., cols = c("tstat", "crit"), names_to = "stat")
  gg <-  plot_data %>%
    ggplot(aes(index, value, col = stat, size = stat, linetype = stat)) +
    geom_line() +
    scale_exuber_manual() +
    theme_exuber()

  all_negative <- all(series %in% diagnostics(object, cv)$negative)
  if (!is.null(shade_opt) && !all_negative) {
    ds_data <- tidy(datestamp(object, cv, option = option)) %>%
      filter(id %in% series) %>%
      droplevels()
    gg <- gg + shade_opt(ds_data, min_duration)
  }

  if (length(series) > 1) {
    if (is.null(dots$scales)) {
      gg <- gg + facet_wrap( ~ id, scales = "free", ...)
    }else{
      gg <- gg + facet_wrap( ~ id, ...)
    }
  }else{
    gg <- gg + ggtitle(series) # = 1 for ggtitle to work in single plot
  }
  gg
}

#' @rdname autoplot.radf_obj
#' @inheritParams datestamp
#' @param fill The shade color that indicates the exuberance periods.
#' @param opacity The opacity of the shade color aka alpha.
#' @export
shade <- function(fill = "grey70", opacity = 0.5, ...) {
  function(ds_data, min_duration) {
  filter(ds_data, Duration >= min_duration) %>%
    geom_rect(
      data = ., inherit.aes = FALSE, fill = fill, alpha = opacity,
      aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf), ...
    )
  }
}

#' Exuber scale and theme functions
#'
#' `scale_exuber_manual` allows specifying the color, size and linetype in
#' `autoplot.radf_obj` mappings. `theme_exuber` is a complete theme which control all non-data display.
#'
#' @param color_values a set of color values to map data values to.
#' @param linetype_values a set of linetype values to map data values to.
#' @param size_values a set of size values to map data values to.
#'
#' @importFrom ggplot2 scale_color_manual scale_size_manual scale_linetype_manual
#' @export
scale_exuber_manual <- function(
  color_values = c("red", "blue"), linetype_values = c(2,1),
  size_values = c(0.8, 0.7)) {
  list(
    scale_color_manual(values = color_values),
    scale_size_manual(values = size_values),
    scale_linetype_manual(values = linetype_values)
  )
}

#' @rdname scale_exuber_manual
#' @inheritParams ggplot2::theme_bw
#' @importFrom ggplot2 `%+replace%`
#' @export
theme_exuber <- function(
  base_size = 11, base_family = "", base_line_size = base_size/22,
  base_rect_size = base_size/22) {
  half_line <- base_size/2
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.title = element_blank(),
      legend.title = element_blank(),
      legend.position = "none"
    ) +
    theme(
      legend.key = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "grey20"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.major = element_line(linetype = "dashed", size = 0.7),
      strip.text.x = element_text(size = rel(1.5), hjust = 0,
          vjust = 1, margin = margin(b = half_line)),
    )
}


