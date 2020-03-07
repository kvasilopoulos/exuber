#' Date-stamping periods of mildly explosive behavior
#'
#' Computes the origination, termination and duration of
#' episodes during which the time series display explosive dynamics.
#'
#' @inheritParams diagnostics
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
datestamp <- function(object, cv = NULL, ...) {
  UseMethod("datestamp")
}

datestamp.default <- function(object, cv, ...) {
  stop_glue("method `datestamp` is not available for objects of class {class(object)}.")
}

# object <- radf_dta
# cv <- crit[[100]]
# option <- "gsadf"
# min_duration = 0

#' This is a topic
#'
#' This is a description
#'
#' @inheritParams datestamp
#' @inheritParams diagnostics.radf
#'
#' @param min_duration The minimum duration of an explosive period for it to be
#' reported. Default is 0.
#' @importFrom rlang sym !!
#' @importFrom dplyr filter
#' @importFrom purrr map map_lgl
#' @export
datestamp.radf <- function(object, cv = NULL, option = c("gsadf", "sadf"),
                           min_duration = 0, ...) {

  assert_class(object, "radf")
  if (is.null(cv)) {
    cv <- retrieve_crit(object)
  }
  assert_class(cv, "cv")
  option <- match.arg(option)
  assert_positive_int(min_duration, strictly = FALSE)
  assert_match(object, cv)

  x <- object
  y <- cv
  dating <- index(x)

  ds <-  diagnostics(x, cv = y, option = option)
  if (all(ds$dummy == 0)) {
    return(message_glue("Cannot reject H0 for significance level 95%"))
  }
  acc <- ds[["accepted"]]
  if (is_bare_character(acc, n = 0)) {
    stop_glue("Cannot reject the H0")
  }


  ds <- vector("list", length(acc))
  if (is_sb(y)) {
    if (get_lag(y) != 0) {
      tstat <- x$bsadf_panel[-c(1:2)] #remove 2 cause of the differnce in the regression
      dating <- dating[-c(1:2)]
    } else if (get_lag(y) > 0) {
      tstat <- x$bsadf_panel
    }
    ds <- list(which(tstat > y$bsadf_panel_cv[, 2]) + get_minw(x) + get_lag(x))
  }

  reps <- if (is_sb(y)) 1 else match(acc, series_names(x))
  for (i in seq_along(acc)) {
    if (is_mc(y)) {
      if (option == "gsadf") {
        # cv <- extract_cv(y, "bsadf_cv", get_lag(x))
        cv <- if (get_lag(x) == 0) {
          y$bsadf_cv[, 2]
        } else {
          y$bsadf_cv[-c(1:get_lag(x)), 2]
        }
        ds[[i]] <- which(x$bsadf[, reps[i]] > cv) + get_minw(x) + get_lag(x)
      } else if (option == "sadf") {
        cv <- if (get_lag(x) == 0) {
          y$badf_cv[, 2]
        } else {
          y$badf_cv[-c(1:get_lag(x)), 2]
        }
        ds[[i]] <- which(x$badf[, i] > cv) + get_minw(x) + get_lag(x)
      }
    } else if (is_wb(y)) {
      cv <- if (get_lag(x) == 0) {
        y$bsadf_cv[, 2, i]
      } else {
        y$bsadf_cv[-c(1:get_lag(x)), 2, i]
      }
      ds[[i]] <- which(x$bsadf[, reps[i]] > cv) + get_minw(x) + get_lag(x)
    }
  }

  ds_stamp <- map(ds, ~ stamp(.x) %>% filter(Duration >= min_duration) %>% as.matrix())
  ds_stamp_index <- lapply(ds_stamp, function(t)
    data.frame(
      "Start" = dating[t[, 1]],
      "End" = dating[t[, 2]],
      "Duration" = t[, 3], row.names = NULL))

  # min_duration may cause to exclude periods or the whole sample
  min_reject <- map_lgl(ds_stamp, ~ length(.x) == 0)

  res <- ds_stamp_index[!min_reject]
  names(res) <- acc[!min_reject]
  if (length(res) == 0) {
    stop_glue("Argument 'min_duration' excludes all explosive periods")
  }
  dms <- list(seq_along(index(x)), if (is_sb(y)) "Panel" else series_names(x)[reps])
  dummy <- matrix(0, nrow = length(index(x)), ncol = length(acc), dimnames = dms)
  for (z in seq_along(acc)) {
    dummy[ds[[z]], z] <- 1
  }

  structure(
    res,
    dummy = dummy,
    index = index(x),
    panel = is_sb(y),
    minw = get_minw(x),
    lag = get_lag(x),
    min_duration = min_duration,
    option = option,
    method = get_method(y),
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
  if (get_panel(x)) {
    print(x[[1]])
    cli::cat_line()
  } else{
    print.listof(x)
  }
}

# identification of periods
stamp <- function(ds) {
  start <- ds[c(TRUE, diff(ds) != 1)]
  end <- ds[c(diff(ds) != 1, TRUE)]
  end[end - start == 0] <- end[end - start == 0] + 1
  duration <- end - start + 1
  foo <- data.frame("Start" = start, "End" = end, "Duration" = duration)
  foo
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
#' dta <- cbind(sim_psy1(n = 100), sim_psy2(n = 100))
#'
#' dta %>%
#'   radf() %>%
#'   datestamp() %>%
#'   autoplot()
#'
#' # Change the colour manually
#' dta %>%
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
    labs(title = "", x = "", y = "") + #intentionally not in theme
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
tidy.datestamp <- function(x, ...) {
  bind_rows(x, .id = "id") %>%
    as_tibble() %>%
    mutate(id = as.factor(id))
}

