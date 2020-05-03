
#' Date-stamping periods of mildly explosive behavior
#'
#' Computes the origination, termination and duration of
#' episodes during which the time series display explosive dynamics.
#'
#' @inheritParams diagnostics
#' @param min_duration The minimum duration of an explosive period for it to be
#' reported (default = 0).
#' @param ... further arguments passed to methods.
#'
#' @return Returns a list containing the estimated origination and termination
#' dates of  episodes of explosive behaviour and the corresponding duration.
#' @details
#' Datestamp also stores a vector whose elements take the value of 1 when there is
#' a period of explosive behaviour and 0 otherwise. This output can serve as a
#' dummy variable for the occurrence of exuberance.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @export
datestamp <- function(object, cv = NULL, min_duration = 0L, ...) {
  UseMethod("datestamp")
}

#' @rdname datestamp
#' @inheritParams diagnostics.radf_obj
#' @importFrom rlang sym !! %||%
#' @importFrom dplyr filter pull
#' @importFrom purrr map map_lgl
#' @export
#'
#' @examples
#'
#' rsim_data <- radf(sim_data)
#'
#' ds_data <- datestamp(rsim_data)
#' ds_data
#'
#' # Choose minimum window
#' datestamp(rsim_data, min_duration = psy_ds(nrow(sim_data)))
#'
#' autoplot(ds_data)
datestamp.radf_obj <- function(object, cv = NULL, min_duration = 0L,
                           option = c("gsadf", "sadf"), ...) {

  # assert_class(object, "radf")
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  option <- match.arg(option)
  assert_positive_int(min_duration, strictly = FALSE)
  assert_match(object, cv)

  idx <- index(object)
  snames <- series_names(object)
  pos <- diagnostics_internal(object, cv, option = option)$positive

  filter_option <- if (option == "gsadf") "bsadf" else  "badf"
  ds_tbl <- augment_join(object, cv) %>%
    filter(sig == 95, name %in% c(filter_option, "bsadf_panel")) %>% # either {bsadf, badf} or bsadf_panel
    mutate(ds_lgl = tstat > crit)

  ds <- list()
  for (nm in pos) {
    ds[[nm]] <- filter(ds_tbl, id == nm) %>%
      pull(ds_lgl) %>%
      which()
  }
  ds_stamp <- map(ds, ~ stamp(.x) %>% filter(Duration >= min_duration) %>% as.matrix())
  idx_trunc <- if (is_sb(cv)) index(cv, trunc = TRUE) else index(object, trunc = TRUE)
  ds_stamp_index <- map(ds_stamp, stamp_to_index, idx_trunc) # index has to from cv to iclude sb_cv(+2)

  # min_duration may cause to exclude periods or the whole sample
  min_reject <- map_lgl(ds_stamp, ~ length(.x) == 0)
  res <- ds_stamp_index[!min_reject]
  names(res) <- pos[!min_reject]
  if (length(res) == 0) {
    stop_glue("Argument 'min_duration' excludes all explosive periods")
  }
  # store to dummy {0, 1}
  reps <- if (is_sb(cv)) 1 else match(pos, series_names(object))
  dms <- list(seq_along(idx), if (is_sb(cv)) "panel" else snames[reps])
  dummy <- matrix(0, nrow = length(idx), ncol = length(pos), dimnames = dms)
  zadj <- get_minw(object) + get_lag(object)
  for (z in seq_along(pos)) {
    dummy[ds[[z]] + zadj, z] <- 1
  }

  structure(
    res,
    dummy = dummy,
    index = idx,
    series_names = snames,
    panel = is_sb(cv),
    minw = get_minw(object),
    lag = get_lag(object),
    min_duration = min_duration,
    option = option,
    method = get_method(cv),
    class = c("ds_radf", "ds", "list")
  )
}

#' @export
print.ds_radf <- function(x, ...) {
  cli::cat_line()
  cli::cat_rule(
    left = glue("Datestamp (min_duration = {get_min_dur(x)})"),
    right = get_method(x))
  cli::cat_line()
  print.listof(x)
}

# identification of periods
stamp <- function(x) {
  start <- x[c(TRUE, diff(x) != 1)] # diff reduces length by 1
  end <- x[c(diff(x) != 1, TRUE)] + 1
  end[end - start == 0] <- end[end - start == 0]
  duration <- end - start
  tibble("Start" = start, "End" = end, "Duration" = duration)
}

stamp_to_index <- function(x, idx) {
  data.frame(
    "Start" = idx[x[, 1]],
    "End" = idx[x[, 2]],
    "Duration" = x[, 3],
    row.names = NULL
  )
}

# TODO: Include Peak
# peak <- numeric()
# for(i in length(start)) {
#   interval <- y[start[i]:end[i]]
#   peak <- start[i] + which.max(interval)
# }

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

  idx <- index(object, trunc = trunc)
  scale_custom <- if (lubridate::is.Date(idx)) scale_x_date else scale_x_continuous

  tidy(object) %>%
    mutate(id = reorder(id, dplyr::desc(id))) %>%
    ggplot(aes(color = id)) +
    geom_segment(
      aes_string(x = "Start", xend = "End", y = "id", yend = "id"), size = 7, ...) +
    scale_custom(limits = c(idx[1L], idx[length(idx)])) +
    theme_bw() +
    scale_color_grey() +
    labs(title = "", x = "", y = "") + #intentionally not in theme (for extra margin)
    theme(
      axis.text.y = element_text(face = "bold", size = 8, hjust = 0),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      plot.margin = margin(0.5, 1, 0, 0.5, "cm")
    )
}

#' Tidy a `ds_radf` object
#'
#' Summarizes information about `ds_radf` object.
#'
#' @param x An object of class `ds_radf`.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#'
#' @export
tidy.ds_radf <- function(x, ...) {
  fct_lvls <- if (attr(x, "panel")) "panel" else series_names(x)
  bind_rows(x, .id = "id") %>%
    as_tibble() %>%
    mutate(id = factor(id, levels = fct_lvls))
}

# TODO https://plotnine.readthedocs.io/en/stable/generated/plotnine.geoms.geom_segment.html


# additional style for autoplot.ds ----------------------------------------


# tidy(datestamp(radf(sim_data))) %>%
#   ggplot() +
#   geom_segment(aes_string(x = "Start", xend = "End", y = "id", yend = "id"), color = "grey", size = 1) +
#   geom_point(aes(x = Start, y = id), size = 2, color = "red") +
#   geom_point(aes(x = Start + 5, y = id), size = 2, color = "green") +
#   geom_point(aes(x = End, y = id), size = 2, color = "blue") +
#   geom_text(aes(x = Start + 5, y = id, label = Duration), size = 4, color = "white") +
#   labs(title = "", x = "", y = "") + #intentionally not in theme (for extra margin)
#   theme_bw() +
#   scale_y_discrete(drop = FALSE) +
#   theme(
#     axis.text.y = element_text(face = "bold", size = 8, hjust = 0),
#     legend.position = "none",
#     panel.grid.major.y = element_blank(),
#     plot.margin = margin(0.5, 1, 0, 0.5, "cm")
#   )
