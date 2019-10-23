
#'
#' gg <- autoplpot_(radf_dta)
#' style_custom(gg, color = c("blue", "red))
#'
#'
#' custom_labels <- c("psy1" = "new_name_for_psy1", "psy2" = "new_name_for_psy2")
#' autoplot_(radf_dta, labeller = labeller(id = as_labeller(custom_labels)))
#'
#'
autoplot_ <- function(
  object, cv = NULL, include_all = FALSE, select_series = NULL,
  option = c("bsadf", "badf"), shade_opt = shade(), facet = TRUE, ...) {

  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "cv")
  option <- match.arg(option)

  acc_series <- if (include_all)
    col_names(object)
  else diagnostics(object, cv)$accepted
  sel_series <- select_series %||% col_names(object)
  series <- intersect(acc_series, sel_series)

  gg <- augment_join(object, cv) %>%
    filter(id %in% series, sig == 0.95, name == option) %>%
    ggplot() +
    geom_line(aes( index, tstat), col = "blue", size = 0.7) +
    geom_line(aes(index, crit), col = "red", size = 0.8, linetype = "dashed") +
    theme_exuber()
  if (!is.null(shade_opt)) {
    ds_data <- datestamp(object, cv) %>% fortify()
    gg <- gg + shade_opt(ds_data)
    gg$ds_data <- ds_data
  }
  if (facet) {
    h <- gg +
      facet_wrap(~id, ...)
  }else{
    h <- list()
    for (i in 1:length(series)) {
      h[[i]] <- gg +
        ggforce::facet_wrap_paginate(~id, nrow = 1, ncol = 1, page = i, ...)
    }
    names(h) <- series
  }
  h
}

shade <- function(min_duration = NULL, shade_color = "grey70", opacity = 0.5, ...) {
  function(ds_data) {
  filter(ds_data, Duration >= min_duration %||% 0) %>%
    geom_rect(
      data = ., inherit.aes = FALSE, fill = shade_color, alpha = opacity,
      aes_string(xmin = "Start", xmax = "End", ymin = -Inf, ymax = +Inf), ...
    )
  }
}

#' @importFrom ggplot2 `%+replace%`
theme_exuber <- function(
  base_size = 11, base_family = "", base_line_size = base_size/22,
  base_rect_size = base_size/22) {
    theme_grey(
      base_size = base_size,
      base_family = base_family,
      base_line_size = base_line_size,
      base_rect_size = base_rect_size) %+replace%
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "grey20"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = "dashed", size = 0.7),
      strip.background = element_blank(),
      strip.text.x = element_text(size = rel(1.4)),
      axis.title = element_blank()
    )
}

style_custom <- function(gg, color = NULL, linetype = NULL, size = NULL) {
  gg$layers[[1]]$aes_params$colour <- color[1]
  gg$layers[[1]]$aes_params$linetype <- linetype[1]
  gg$layers[[1]]$aes_params$size <- size[1]
  gg$layers[[2]]$aes_params$colour <- color[2]
  gg$layers[[2]]$aes_params$linetype <- linetype[2]
  gg$layers[[2]]$aes_params$size <- size[2]
  gg
}

style_black <- function(gg) {
  gg$layers[[1]]$aes_params$colour <- "black"
  gg$layers[[2]]$aes_params$colour <- "black"
  gg
}

style_exuber <- function(gg) {
  gg$layers[[1]]$aes_params$colour <- "blue"
  gg$layers[[2]]$aes_params$colour <- "red"
  gg
}


