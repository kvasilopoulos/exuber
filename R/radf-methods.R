# summary -----------------------------------------------------------------

#' Summarizing `radf` models
#'
#' \code{summary} method for radf models that consist of `radf_obj` and `radf_cv`.
#'
#' @param object An object of class `radf_obj`. The output of \code{\link[=radf]{radf()}}.
#' @param cv An object of class `radf_cv`. The output of \code{\link[=radf_mc_cv]{radf_mc_cv()}},
#'   \code{\link[=radf_wb_cv]{radf_wb_cv()}} or \code{\link[=radf_sb_cv]{radf_sb_cv()}}.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return Returns a list of summary statistics, which include the estimated ADF,
#' SADF, and GSADF test statistics and the corresponding critical values
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter select
#' @importFrom rlang is_logical
#' @name summary.radf_obj
#' @examples
#' \donttest{
#' # Simulate bubble processes, compute the test statistics and critical values
#' rsim_data <- radf(sim_data)
#'
#' # Summary, diagnostics and datestamp (default)
#' summary(rsim_data)
#'
#' # Summary, diagnostics and datestamp (wild bootstrap critical values)
#'
#' wb <- radf_wb_cv(sim_data)
#'
#' summary(rsim_data, cv = wb)
#' }
#' @export
summary.radf_obj <- function(object, cv = NULL, ...) {
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  assert_match(object, cv)
  ret <- summary_radf(cv, object, ...)

  ret %>%
    add_attr(
      minw = get_minw(object),
      lag = get_lag(object),
      method = get_method(cv),
      iter = get_iter(cv)
    ) %>%
    add_class("sm_radf")
}

summary_radf <- function(cv, ...) {
  UseMethod("summary_radf")
}

#' @exportS3method summary_radf mc_cv
summary_radf.mc_cv <- summary_radf.wb_cv <- function(cv, object, ...) {
  ret <- list()
  snames <- series_names(object)
  sm <- tidy_join(object, cv) %>%
    pivot_wider(names_from = sig, values_from = crit)
  for (nms in snames) {
    ret[[nms]] <- filter(sm, id == nms) %>%
      select(-id)
  }
  ret
}

#' @exportS3method summary_radf sb_cv
summary_radf.sb_cv <- function(cv, object, ...) {
  ret <- list()
  ret[["panel"]] <- tidy_join(object, cv, panel = TRUE) %>%
    pivot_wider(names_from = sig, values_from = crit) %>%
    select(-id)
  ret
}

#' @importFrom glue glue
#' @export
print.sm_radf <- function(x, ...) {
  iter_char <- if (is_mc(x)) "nrep" else "nboot"
  cat_line()
  cat_rule(
    left = glue("Summary (minw = {get_minw(x)}, lag = {get_lag(x)})"),
    right = glue("{get_method(x)} ({iter_char} = {get_iter(x)})")
  )
  cat_line()
  print.listof(x, ...)
}

# diagnostics -------------------------------------------------------------

#' Diagnostics on hypothesis testing
#'
#' Provides information on whether the null hypothesis of a unit root is rejected
#' against the alternative of explosive behaviour for each series in a dataset.
#'
#' @param object  An object of class `obj`.
#' @param cv An object of class `cv`.
#' @param ... Further arguments passed to methods.
#'
#' @return Returns a list with the series that reject (positive) and the series
#' that do not reject (negative) the null hypothesis, and at what significance level.
#'
#' @details
#' Diagnostics also stores a vector whose elements take the value of 1 when
#' there is a period of explosive behaviour and 0 otherwise.
#'
#' @export
diagnostics <- function(object, cv = NULL, ...) {
  UseMethod("diagnostics", object)
}

#' @rdname diagnostics
#' @importFrom dplyr case_when
#' @param option Whether to apply the "gsadf" or "sadf" methodology (default = "gsadf").
#' @export
#' @examples
#'
#' rsim_data <- radf(sim_data)
#' diagnostics(rsim_data)
#'
#' diagnostics(rsim_data, option = "sadf")
diagnostics.radf_obj <- function(object, cv = NULL,
                                 option = c("gsadf", "sadf"), ...) {
  # assert_class(object, "radf")
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  assert_match(object, cv)
  option <- match.arg(option)

  if (option == "sadf" && is_sb(cv)) {
    stop_glue("argument 'option' cannot  be be set to 'sadf' when cv is of class 'sb_cv'")
  }
  snames <- series_names(object)
  if (is_sb(cv)) {
    option <- "gsadf_panel"
  }
  out <- tidy_join(object, cv) %>%
    pivot_wider(names_from = sig, values_from = crit, names_prefix = "cv") %>%
    filter(stat == option)
  # in case of simulation exercises
  dummy <- case_when(
    out$tstat < out$cv95 ~ 0,
    out$tstat >= out$cv95 ~ 1
  )
  sig <- case_when(
    out$tstat < out$cv90 ~ "Reject",
    out$tstat >= out$cv90 & out$tstat < out$cv95 ~ "10%",
    out$tstat >= out$cv95 & out$tstat < out$cv99 ~ "5%",
    out$tstat >= out$cv99 ~ "1%"
  )
  dummy_lgl <- as.logical(dummy)
  if (is_sb(cv)) {
    positive <- ifelse(dummy_lgl, "panel", NA)
    negative <- ifelse(dummy_lgl, NA, "panel")
  } else {
    positive <- snames[as.logical(dummy_lgl)]
    negative <- snames[!as.logical(dummy_lgl)]
  }
  list(
    positive = positive,
    negative = negative,
    sig = sig,
    dummy = dummy
  ) %>%
    add_attr(
      panel = is_sb(cv),
      series_names = if (!is_sb(cv)) snames,
      method = get_method(cv),
      option = option,
    ) %>%
    add_class("dg_radf")
}

#' @export
tidy.dg_radf <- function(x, ...) {
  snames <- series_names(x)
  sig <- gsub("%", "", x$sig)
  tibble(
    "series" = snames,
    "positive" = ifelse(snames %in% x$positive, TRUE, FALSE),
    "negative" = ifelse(snames %in% x$negative, TRUE, FALSE),
    "sig" = as.factor(ifelse(sig == "Reject", NA, sig))
  )
}


diagnostics_internal <- function(...) {
  dg <- diagnostics(...)
  if (all(dg$dummy == 0)) {
    stop_glue("Cannot reject H0 at the 5% significance level")
  }
  if (purrr::is_bare_character(dg$positive, n = 0)) {
    stop_glue("Cannot reject H0")
  }
  unclass(dg)
}


#' @importFrom cli cat_line cat_rule
#' @importFrom glue glue
#' @importFrom rlang is_bare_character
#' @export
print.dg_radf <- function(x, ...) {
  cli::cat_line()
  cli::cat_rule(
    left = glue('Diagnostics (option = {attr(x, "option")})'),
    right = get_method(x)
  )
  cli::cat_line()
  if (attr(x, "panel")) {
    if (x$sig == "Reject") {
      cat(" Cannot reject H0 \n")
    } else {
      cat(" Rejects H0 at the", cli::col_red(x$sig), "significance level\n")
    }
  } else {
    width <- nchar(series_names(x))
    ngaps <- max(8, width) - width
    for (i in seq_along(series_names(x))) {
      cat(series_names(x)[i], ":", rep(" ", ngaps[i]), sep = "")
      if (x$sig[i] == "Reject") {
        cat(" Cannot reject H0 \n")
      } else {
        cat(" Rejects H0 at the", cli::col_red(x$sig[i]), "significance level\n")
      }
    }
  }
  cli::cat_line()
}

# datestamp ---------------------------------------------------------------



#' Date-stamping periods of mildly explosive behavior
#'
#' Computes the origination, termination and duration of
#' episodes during which the time series display explosive dynamics.
#'
#' @inheritParams diagnostics
#' @param min_duration The minimum duration of an explosive period for it to be
#' reported (default = 0).
#' @param nonrejected logical. Whether to apply datestamping technique to the series
#' that were not able to reject the Null hypothesis.
#' @param sig_lvl logical. Significance level, one of 90, 95 or 99.
#' @param ... further arguments passed to methods.
#'
#' @return Return a table with the following columns:
#'
#' \itemize{
#'  \item Start:
#'  \item Peak:
#'  \item End:
#'  \item Duration:
#'  \item Signal:
#'  \item Ongoing:
#' }
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
#' @importFrom purrr map map_lgl possibly
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
datestamp.radf_obj <- function(object, cv = NULL, min_duration = 0L, sig_lvl = 95,
                               option = c("gsadf", "sadf"), nonrejected = FALSE, ...) {
  # assert_class(object, "radf")
  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  option <- match.arg(option)
  stopifnot(sig_lvl %in% c(90, 95, 99))
  assert_positive_int(min_duration, strictly = FALSE)
  assert_match(object, cv)

  is_panel <- is_sb(cv)

  snames <- series_names(object)
  pos <- if (isTRUE(nonrejected)) {
    if (is_panel) "panel" else snames
  } else {
    diagnostics_internal(object, cv)$positive # internal to make the check here
  }

  filter_option <- if (option == "gsadf") c("bsadf_panel", "bsadf") else c("bsadf_panel", "badf")
  ds_tbl <- augment_join(object, cv) %>%
    filter(sig == sig_lvl, stat %in% filter_option) %>% # either {bsadf, badf} or bsadf_panel
    mutate(ds_lgl = tstat > crit)

  ds_basic <- map(pos, ~ filter(ds_tbl, id == .x) %>%
    pull(ds_lgl) %>%
    which())
  ds_stamp <- map(ds_basic, ~ stamp(.x) %>% as.matrix())

  if (!is_panel) {
    tstat <- map2(pos, ds_stamp, ~ filter(ds_tbl, id == .x) %>% pull(tstat))
    mat <- map(pos, ~ mat(object)[, .x])
    possibly_add_peak <- possibly(add_peak, otherwise = NULL)
    ds_stamp <- purrr::pmap(list(ds_stamp, tstat, mat, get_trunc(object)), possibly_add_peak)
  }

  idx <- if (is_panel) index(cv) else index(object)
  idx_trunc <- if (is_panel) index(cv, trunc = TRUE) else index(object, trunc = TRUE)
  ds_stamp_index <- map(ds_stamp, stamp_to_index, idx_trunc, cv) # index has to from cv to iclude sb_cv(+2)
  ds_full <- purrr::map(ds_stamp_index, add_ongoing, idx, cv)

  if (isTRUE(nonrejected)) {
    dg <- diagnostics(object, cv)$negative
    ds_full <- map2(ds_full, pos %in% dg, ~ mutate(.x, Nonrejected = .y))
  }

  ds <- map(ds_full, ~ filter(.x, Duration >= min_duration))
  min_reject <- map_lgl(ds, ~ nrow(.x) == 0)
  res <- ds[!min_reject]
  names(res) <- pos[!min_reject]
  if (length(res) == 0) {
    warning_glue("Argument 'min_duration' excludes all explosive periods")
  }

  # store to dummy {0, 1}
  idx <- index(object)
  reps <- if (is_panel) 1 else match(pos, series_names(object))
  dms <- list(seq_along(idx), if (is_panel) "panel" else snames[reps])
  dummy <- matrix(0, nrow = length(idx), ncol = length(pos), dimnames = dms)
  zadj <- get_minw(object) + get_lag(object)
  for (z in seq_along(pos)) {
    dummy[ds_basic[[z]] + zadj, z] <- 1
  }

  structure(
    res,
    dummy = dummy,
    index = idx,
    series_names = snames,
    minw = get_minw(object),
    lag = get_lag(object),
    n = get_n(object),
    panel = is_panel,
    min_duration = min_duration,
    option = option,
    method = get_method(cv),
    class = c("ds_radf", "list")
  )
}


stamp <- function(x) {
  start <- x[c(TRUE, diff(x) != 1)] # diff reduces length by 1
  end <- x[c(diff(x) != 1, TRUE)] + 1
  end[end - start == 0] <- end[end - start == 0]
  duration <- end - start
  tibble("Start" = start, "End" = end, "Duration" = duration)
}

stamp_to_index <- function(x, idx, cv) {
  if (is_sb(cv)) {
    if (is.null(x)) {
      na_df <- data.frame(
        "Start" = NA,
        "End" = NA,
        "Duration" = NA,
        row.names = NULL
      )
      na_df <- na_df[-1, ]
      return(na_df)
    }

    data.frame(
      "Start" = idx[x[, "Start"]],
      "End" = idx[x[, "End"]],
      "Duration" = x[, "Duration"],
      row.names = NULL
    )
  } else {
    if (is.null(x)) {
      na_df <- data.frame(
        "Start" = NA,
        "Peak" = NA,
        "End" = NA,
        "Duration" = NA,
        "Signal" = NA,
        row.names = NULL
      )
      na_df <- na_df[-1, ]
      return(na_df)
    }

    data.frame(
      "Start" = idx[x[, "Start"]],
      "Peak" = idx[x[, "Peak"]],
      "End" = idx[x[, "End"]],
      "Duration" = x[, "Duration"],
      "Signal" = x[, "Signal"],
      row.names = NULL
    )
  }
}

add_peak <- function(ds, tstat, mat, minw) {
  start <- ds[, "Start"]
  end <- ds[, "End"]
  np <- length(start)
  signal <- peak <- numeric(np)
  for (i in 1:np) {
    ival <- start[i]:(end[i] - 1)
    tstat_ival <- tstat[ival]
    peak[i] <- start[i] + which.max(tstat_ival) - 1
    diff_peak <- mat[minw + peak[i]] - mat[minw + start[i]]
    signal[i] <- ifelse(diff_peak >= 0, "positive", "negative")
  }
  data.frame(
    Start = start,
    Peak = peak,
    End = end,
    Duration = ds[, "Duration"],
    Signal = signal
  )
}

# TODO ongoing cannot work in panel
add_ongoing <- function(ds, idx, cv) {
  n <- get_n(cv)
  end <- ds[, "End"]
  if (is_logical(end, 0)) {
    return(data.frame(ds, Ongoing = character(0)))
  } else {
    ongoing <- ifelse(is.na(end), TRUE, FALSE)
  }
  np <- length(end)
  for (i in 1:np) {
    if (ongoing[i]) {
      end[i] <- idx[n]
    }
  }
  ds[, "End"] <- end
  data.frame(ds, Ongoing = ongoing)
}


#' @export
print.ds_radf <- function(x, ...) {
  if (length(x) == 0) {
    return(invisible(NULL))
  }
  cli::cat_line()
  cli::cat_rule(
    left = glue("Datestamp (min_duration = {get_min_dur(x)})"),
    right = get_method(x)
  )
  cli::cat_line()
  print.listof(x)
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
#' @importFrom rlang !!!
#'
#' @export
tidy.ds_radf <- function(x, ...) {
  fct_lvls <- if (attr(x, "panel")) "panel" else series_names(x)
  nlevels <- length(fct_lvls)
  ds <- bind_rows(!!!x, .id = "id") %>%
    as_tibble() %>%
    mutate(id = factor(id, levels = fct_lvls))
  ds
}
