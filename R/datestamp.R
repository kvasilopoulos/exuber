#' Date-stamping periods of mildly explosive behavior
#'
#' Computes the origination, termination and duration of
#' episodes during which the time series display explosive dynamics.
#'
#' @inheritParams diagnostics
#' @param min_duration The minimum duration of an explosive period for it to be
#' reported. Default is 0.
#' @param ... further arguments passed to methods.
#'
#' @return Returns a list of values for each explosive sub-period, giving the origin
#' and termination dates as well as the number of periods explosive behavior lasts.
#' @details
#' Datestamp also stores a vector in {0,1} that corresponds to {reject, accept}
#' respectively, for all series in the time period. This output can be used as
#' a dummy that indicates the occurrence of a bubble.
#'
#' Setting \code{min_duration} removes very short episode of exuberance.
#' Phillips et al. (2015) propose two simple rules of thumb to remove short
#' periods of explosive dynamics, "log(T)/T", where T is the number of observations.
#'
#' @references Phillips, P. C. B., Shi, S., & Yu, J. (2015). Testing for
#' Multiple Bubbles: Historical Episodes of Exuberance and Collapse in the
#' S&P 500. International Economic Review, 56(4), 1043-1078.
#'
#' @export
datestamp <- function(object, cv = NULL, min_duration = 0L, ...) {
  UseMethod("datestamp")
}

#' @export
datestamp.default <- function(object, cv, ...) {
  stop_glue(
    "method 'datestamp' is not available for objects of class '{class(object)}'.")
}

#' @rdname datestamp
#' @inheritParams diagnostics.radf
#' @importFrom rlang sym !! %||%
#' @importFrom dplyr filter pull
#' @importFrom purrr map map_lgl
#' @export
datestamp.radf <- function(object, cv = NULL, min_duration = 0L,
                           option = c("gsadf", "sadf"), ...) {

  assert_class(object, "radf")
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "cv")
  option <- match.arg(option)
  assert_positive_int(min_duration, strictly = FALSE)
  assert_match(object, cv)

  idx <- index(object)
  snames <- series_names(object)
  ds <-  diagnostics_internal(object, cv, option = option)
  acc <- ds[["accepted"]]

  option <- if (option == "gsadf") "bsadf" else  "badf"
  ds_tbl <- augment_join(object, cv) %>%
    filter(sig == 95, name %in% c(option, "bsadf_panel")) %>%
    mutate(ds_lgl = tstat > crit)

  ds <- list()
  for (nm in acc) {
    ds[[nm]] <- filter(ds_tbl, id == nm) %>%
      pull(ds_lgl) %>%
      which()
  }
  ds_stamp <- map(ds, ~ stamp(.x) %>% filter(Duration >= min_duration) %>% as.matrix())
  ds_stamp_index <- map(ds_stamp, stamp_to_index, index(object, trunc = TRUE))

  # min_duration may cause to exclude periods or the whole sample
  min_reject <- map_lgl(ds_stamp, ~ length(.x) == 0)
  res <- ds_stamp_index[!min_reject]
  names(res) <- acc[!min_reject]
  if (length(res) == 0) {
    stop_glue("Argument 'min_duration' excludes all explosive periods")
  }
  # store to dummy {0, 1}
  reps <- if (is_sb(cv)) 1 else match(acc, series_names(object))
  dms <- list(seq_along(idx), if (is_sb(cv)) "panel" else snames[reps])
  dummy <- matrix(0, nrow = length(idx), ncol = length(acc), dimnames = dms)
  for (z in seq_along(acc)) {
    dummy[ds[[z]], z] <- 1
  }

  structure(
    res,
    dummy = dummy,
    index = idx,
    panel = is_sb(cv),
    minw = get_minw(object),
    lag = get_lag(object),
    min_duration = min_duration,
    option = option,
    method = get_method(cv),
    class = c("datestamp", "list")
  )
}

#' @export
print.datestamp <- function(x, ...) {
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
  end <- x[c(diff(x) != 1, TRUE)]
  end[end - start == 0] <- end[end - start == 0] + 1
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

#' Plotting and tidying datestamp objects
#'
#' Plotting datestamp with \link[=ggplot2]{geom_segment()}
#'
#' @name autoplot.datestamp
#'
#' @param object An object of class \code{\link[=datestamp]{datestamp()}}
#' @param trunc default FALSE. If TRUE the index formed by truncating the value
#' in the minimum window.
#' @param ... further arguments passed to methods.
#' @export
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
autoplot.datestamp <- function(object, trunc = TRUE, ...) {

  dating <- index(object, trunc = trunc)
  scale_custom <- if (lubridate::is.Date(dating)) scale_x_date else scale_x_continuous

  ggplot(tidy(object), aes_string(colour = "id")) +
    geom_segment(
      aes_string(x = "Start", xend = "End", y = "id", yend = "id"), size = 7) +
    scale_custom(limits = c(dating[1L], dating[length(dating)])) +
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

#' @rdname autoplot.datestamp
#' @param x An object of class \code{\link[=datestamp]{datestamp()}}
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @export
tidy.datestamp <- function(x, ...) {
  bind_rows(x, .id = "id") %>%
    as_tibble() %>%
    mutate(id = as.factor(id))
}

