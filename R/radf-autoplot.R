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
#' @param trunc Whether to remove the period of the minimum window from the plot (default = TRUE).
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
#' # Modify the shading options
#' autoplot(rsim_data, shade_opt = shade(fill = "pink", opacity = 0.5))
#'
#' # Or remove the shading completely
#' autoplot(rsim_data, shade_opt = shade(opacity = 0))
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
                          trunc = TRUE,
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
  plot_data <- augment_join(object, cv, trunc = trunc) %>%
    filter(id %in% series, sig == 95, stat == filter_option) %>%
    pivot_longer(data = ., cols = c("tstat", "crit"), names_to = "tstat_crit")
  gg <-  plot_data %>%
    ggplot(aes(index, value, col = tstat_crit, size = tstat_crit, linetype = tstat_crit)) +
    geom_line() +
    scale_exuber_manual() +
    theme_exuber()

  all_negative <- all(series %in% diagnostics(object, cv)$negative)
  idx <- index(object)
  if (!is.null(shade_opt) && !all_negative) {
    ds_data <- datestamp(object, cv, option = option, nonrejected = include_negative)
    gg <- gg + shade_opt(ds_data, min_duration)
  }

  if (length(series) > 1) {
    if (is.null(dots$scales)) {
      gg <- gg + facet_wrap( ~ id, scales = "free", ...)
    }else{
      gg <- gg + facet_wrap( ~ id, ...)
    }
  }else{
    gg <- gg + ggtitle(series)
  }
  gg
}


# autolayer.ds_radf <- function(ds_radf, min_duration = 0L, ...) {
#   shade(...)(tidy(ds_radf), min_duration)
# }

#' @rdname autoplot.radf_obj
#' @export
autoplot2.radf_obj <- function(object, cv = NULL,
                      option = c("gsadf", "sadf"),
                      min_duration = 0L,
                      select_series = NULL,
                      include_negative = FALSE,
                      trunc = TRUE,
                      shade_opt = shade(), ...) {

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
  plot_data <- augment_join(object, cv, trunc = trunc) %>%
    filter(id %in% series, sig == 95, stat == filter_option)
  gg <-  plot_data %>%
    ggplot(aes(index, data)) +
    geom_line() +
    scale_exuber_manual() +
    theme_exuber()

  all_negative <- all(series %in% diagnostics(object, cv)$negative)
  idx <- index(object)
  if (!is.null(shade_opt) && !all_negative) {
    ds_data <- datestamp(object, cv, option = option, nonrejected = include_negative)
    gg <- gg + shade_opt(ds_data, min_duration)
  }

  if (length(series) > 1) {
    if (is.null(dots$scales)) {
      gg <- gg + facet_wrap( ~ id, scales = "free", ...)
    }else{
      gg <- gg + facet_wrap( ~ id, ...)
    }
  }else{
    # = 1 for ggtitle to work in single plot
    gg <- gg + ggtitle(series)
  }
  gg
}

#' @rdname autoplot.radf_obj
#' @inheritParams datestamp
#' @param fill The shade color that indicates the exuberance periods with positive signal
#' @param fill_negative The shade color that indicates the exuberance periods with positive signal
#' @param fill_ongoing The shade color that indicates the exuberance periods that are ongoing
#' the null hypothesis.
#'
#' @param opacity The opacity of the shade color aka alpha.
#' @export
shade <- function(fill = "grey55", fill_negative = fill,#"yellow2",
                  fill_ongoing = NULL, opacity = 0.3, ...) { #"pink2"
  function(ds_data, min_duration) {

    is_panel <- "panel" %in% names(ds_data)
    ds_data <- filter(tidy(ds_data), Duration >= min_duration)

    if(is_panel) {
      ds_pos <- ds_data
      ds_neg <- filter(ds_data, Duration == "notexist")
    }else{
      ds_pos <- filter(ds_data, Signal == "positive")
      ds_neg <- filter(ds_data, Signal == "negative")
    }

    if(!is.null(fill_ongoing)) {
      ds_pos <- filter(ds_data, Ongoing == FALSE)
      ds_neg <- filter(ds_data, Ongoing == FALSE)
    }

    any_pos <- nrow(ds_pos) > 0
    x1 <-  ds_pos %>%
      geom_rect(
        data = .,  inherit.aes = FALSE, fill = fill, alpha = opacity,
        aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf), ...
      )

    any_neg <- nrow(ds_neg) > 0
    x2 <-  ds_neg %>%
      geom_rect(
        data = ., inherit.aes = FALSE, fill = fill_negative, alpha = opacity,
        aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf), ...
      )

    if(!is.null(fill_ongoing)) {
      any_ongoing <- any(ds_data$Ongoing)
      x3 <- filter(ds_data, Ongoing == TRUE) %>%
        geom_rect(
          data = ., inherit.aes = FALSE, fill = fill_ongoing, alpha = opacity,
          aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf), ...
        )
    } else {
      any_ongoing <- x3 <- NULL
    }

    list(
      any_pos %NULL% x1,
      any_neg %NULL% x2,
      any_ongoing %NULL% x3
    )
  }
}

null_color <- function() {
  "#ffffff00"
}

#' Exuber scale and theme functions
#'
#' `scale_exuber_manual` allows specifying the color, size and linetype in
#' `autoplot.radf_obj` mappings. `theme_exuber` is a complete theme which control
#' all non-data display.
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

# datestamp ---------------------------------------------------------------


#' Plotting a `ds_radf` object
#'
#' Takes a `ds_radf` object and returns a ggplot2 object, with a
#' \link[=ggplot2]{geom_segment()} layer.
#'
#' @name autoplot.ds_radf
#'
#' @param object An object of class \code{ds_radf}. The output of \code{\link[=datestamp]{datestamp()}}
#' @param trunc Whether to remove the period of the minimum window from the plot (default = TRUE).
#' @param ... Further arguments passed to methods. Not used.
#' @export
#'
#' @importFrom stats reorder
#'
#' @return A [ggplot2::ggplot()]
#'
#' @examples
#' \donttest{
#'
#' sim_data_wdate %>%
#'   radf() %>%
#'   datestamp() %>%
#'   autoplot()
#'
#' # Change the colour manually
#' sim_data_wdate %>%
#'   radf() %>%
#'   datestamp() %>%
#'   autoplot() +
#'   ggplot2::scale_colour_manual(values = rep("black", 4))
#' }
autoplot.ds_radf <- function(object, trunc = TRUE, ...) {

  stopifnot(is.logical(trunc))

  ggplot() +
    geom_ds_segment(object, trunc = trunc, ...) +
    theme_bw() +
    labs(title = "", x = "", y = "") + #intentionally not in theme (for extra margin)
    theme(
      axis.text.y = element_text(face = "bold", size = 8, hjust = 0),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      plot.margin = margin(0.5, 1, 0, 0.5, "cm")
    )
}

geom_ds_segment <- function(object, trunc = TRUE, col = "grey75", col_negative = col,#"yellow2",
                            col_ongoing = NULL) {

  is_panel <- get_panel(object)
  idx <- index(object, trunc = trunc)
  scale_custom <- if (lubridate::is.Date(idx)) scale_x_date else scale_x_continuous

  ds_data <- filter(tidy(object)) %>%
    mutate(id = reorder(id, dplyr::desc(id)))

  if(is_panel) {
    ds_pos <- ds_data
    ds_neg <- filter(ds_data, Duration == "notexist")
  }else{
    ds_pos <- filter(ds_data, Signal == "positive")
    ds_neg <- filter(ds_data, Signal == "negative")
  }

  if(!is.null(col_ongoing)) {
    ds_pos <- filter(ds_data, Ongoing == FALSE)
    ds_neg <- filter(ds_data, Ongoing == FALSE)
  }

  any_pos <- any(ds_data$Signal == "positive")
  x1 <- filter(ds_data,  Signal == "positive") %>%
    geom_segment(
      data = ., size = 7, color = col,
      aes(x = Start, xend = End, y = id, yend = id)
    )

  any_neg <- any(ds_data$Signal == "negative")
  x2 <- filter(ds_data, Signal == "negative") %>%
    geom_segment(
      data = ., size = 7, color = col_negative,
      aes(x = Start, xend = End, y = id, yend = id)
    )

  if(!is.null(col_ongoing)) {
    any_ongoing <- any(ds_data$Ongoing)
    x3 <- filter(ds_data, Ongoing == TRUE) %>%
      geom_segment(
        data = ., color = col_ongoing, size = 7,
        aes(x = Start, xend = End, y = id, yend = id)
      )
  } else {
    any_ongoing <- x3 <- NULL
  }

  list(
    any_pos %NULL% x1,
    any_neg %NULL% x2,
    any_ongoing %NULL% x3,
    scale_custom(limits = c(idx[1L], idx[length(idx)]))
  )
}




