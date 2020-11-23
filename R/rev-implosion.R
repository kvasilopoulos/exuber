

#' Reverse Recursive Augmented Dickey-Fuller Test
#'
#' `rev_radf` returns the reverse recursive univariate and panel Augmented
#' Dickey-Fuller test statistics.
#'
#' @inheritParams radf
#'
#' @details
#'
#' @return A list that contains the unit root test statistics (sequence):
#'   \item{adf}{Augmented Dickey-Fuller}
#'   \item{sadf}{Supremum Augmented Dickey-Fuller}
#'   \item{gsadf}{Generalized Supremum Augmented Dickey-Fuller}
#'   \item{gsadf_panel}{Panel Generalized Supremum Augmented Dickey-Fuller}
#'   \item{rev_badf}{Backward Augmented Dickey-Fuller}
#'   \item{rev_bsadf}{Backward Supremum Augmented Dickey-Fuller}
#'   \item{rev_bsadf_panel}{Panel Backward Supremum Augmented Dickey-Fuller}
#'
#' @references Phillips, Peter CB, and Shu Ping Shi. "Financial bubble implosion
#' and reverse regression." Econometric Theory 34.4 (2018): 705-753.
#'
#' @export
rev_radf <- function(data, minw = NULL, lag = 0L) {

  x <- parse_data(data)
  minw <- minw %||% psy_minw(data)
  nc <- ncol(x)

  assert_na(x)
  assert_positive_int(minw, greater_than = 2)
  assert_positive_int(lag, strictly = FALSE)

  pointer <- nrow(x) - minw - lag
  snames <- colnames(x)
  adf <- sadf <- gsadf <- drop(matrix(0, 1, nc, dimnames = list(NULL, snames)))
  rev_badf <- rev_bsadf <- matrix(0, pointer, nc, dimnames = list(NULL, snames))

  for (i in 1:nc) {
    yxmat <- unroot(x[, i], lag = lag)
    results <- rls_gsadf(yxmat, min_win = minw, lag = lag)
    adf[i]     <- results[pointer + 1]
    sadf[i]    <- results[pointer + 2]
    gsadf[i]   <- results[pointer + 3]

    rev_yxmat <- unroot(rev(x[, i]), lag = lag) # add rev here
    rev_results <- rls_gsadf(rev_yxmat, min_win = minw, lag = lag)
    rev_badf[, i]  <- rev(rev_results[1:pointer]) # and rev here agin to restore order
    rev_bsadf[, i] <- rev(rev_results[-c(1:(pointer + 3))])
  }
  rev_bsadf_panel <- apply(rev_bsadf, 1, mean)

  list(
    rev_badf = rev_badf,
    rev_bsadf = rev_bsadf,
    rev_bsadf_panel = rev_bsadf_panel,
    adf = adf,
    sadf = sadf,
    gsadf = gsadf
  ) %>%
    add_attr(
      index = attr(x, "index"),
      lag = lag,
      n = nrow(x),
      minw = minw,
      lag = lag,
      series_names = snames,
    ) %>%
    add_class("rev_radf_obj")
}

#' @export
print.rev_radf_obj <- function(x, ...) {
  cat_line()
  cat_rule(
    left = glue("rev_radf (minw = {get_minw(x)}, lag = {get_lag(x)})")
  )
  cat_line()
  cli::cli_alert_success("Proceed to datestamp.")
  cat_line()
}

#' @export
index.rev_radf_obj <- function(x, trunc = FALSE, ...) {
  idx <- attr(x, "index")
  len <- attr(x, "n")
  if (trunc) idx <- idx[c(1:(len - get_minw(x) + get_lag(x)))]
  idx
}


# Same methods as radf_obj ------------------------------------------------


#' @export
tidy.rev_radf_obj <- function(...) {
  tidy.radf_obj(...)
}

#' @export
tidy_join.rev_radf_obj <- function(...) {
  tidy_join.radf_obj(...)
}

#' @export
diagnostics.rev_radf_obj <- function(...) {
  diagnostics.radf_obj(...)
}


# Augment method are different due to the change of name ------------------


#' @export
augment.rev_radf_obj <- function(x, format = c("wide", "long"), panel = FALSE, ...) {

  format <- match.arg(format)
  stopifnot(is.logical(panel))

  if (panel) {
    tbl_radf <- tibble(
      index = index(x, trunc = TRUE),
      rev_bsadf_panel = pluck(x, "rev_bsadf_panel")
    ) %>%
      add_key(x) %>%
      select(key, index, rev_bsadf_panel)

    if (format == "long") {
      tbl_radf <-
        tbl_radf %>%
        gather(name, tstat, -index, -key, factor_key = TRUE) %>%
        mutate(id = factor("panel")) %>%
        select(key, index, id, name, tstat)
    }
  }else{
    tbl_radf <- x %>%
      pluck("rev_badf") %>%
      as_tibble() %>%
      add_key(x) %>%
      mutate(
        index = index(x, trunc = TRUE)
      ) %>%
      gather(id, rev_badf, -index, -key, factor_key = TRUE) %>%
      bind_cols(
        x %>%
          pluck("rev_bsadf") %>%
          as_tibble() %>%
          gather(name, rev_bsadf) %>%
          select(rev_bsadf)
      ) %>%
      select(key, index, id, everything())

    if (format == "long") {
      tbl_radf <-
        tbl_radf %>%
        gather(name, tstat, -index, -id, -key) %>%
        mutate(name = factor(name, levels = c("rev_badf", "rev_bsadf"))) %>%
        arrange(id, name)
    }
  }
  tbl_radf
}

#' @export
augment_join.rev_radf_obj <- function(x, y = NULL, ...) {
  y <- y %||% retrieve_crit(x)
  assert_class(y, "radf_cv")
  # if(isFALSE(is_mc(y))) {
  #   stop("Reverse datestapming is allowed only for `mc` critical values.")
  # }
  assert_match(x, y)

  is_panel <- is_sb(y)
  join_by <- if (!is_mc(y)) c("id") else NULL
  is_idx_date <- is.Date(index(x))

  if (!is_idx_date && !is_mc(y)) join_by <- c("index", join_by)
  idx_if_date <- if (is_idx_date && !is_mc(y)) "index"  else NULL
  key_if_date <- if (is_idx_date) "key"  else NULL
  id_lvls <- if (is_panel) "panel" else series_names(x)

  inner_join(
    augment(x, "long", panel = is_panel),
    augment(y, "long") %>%
      select_at(vars(-all_of(idx_if_date))) %>%
      group_by(name, sig) %>%
      mutate(
        crit = rev(crit),
        name = dplyr::recode(name, badf = "rev_badf", bsadf = "rev_bsadf")
        ),
    by = c("key", "name", join_by)) %>%
    mutate(id = factor(id, levels = id_lvls)) %>%
    arrange(sig, id, name) %>%
    select_at(vars(-all_of(key_if_date)))
}



#' @export
datestamp.rev_radf_obj <- function(object, cv = NULL, min_duration = 0L,
                                   option = c("gsadf", "sadf"), ...) {

    cv <- cv %||% retrieve_crit(object)
    assert_class(cv, "radf_cv")
    option <- match.arg(option)
    assert_positive_int(min_duration, strictly = FALSE)
    assert_match(object, cv)

    idx <- index(object)
    snames <- series_names(object)
    pos <- diagnostics_internal(object, cv, option = option)$positive

    filter_option <- if (option == "gsadf") "rev_bsadf" else  "rev_badf"
    ds_tbl <- augment_join(object, cv) %>%
      filter(sig == 95, name %in% c(filter_option, "rev_bsadf_panel")) %>% # either {bsadf, badf} or bsadf_panel
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
      class = c("ds_rev_radf", "list")
    )
}


# same methods again ------------------------------------------------------

#' @export
print.ds_rev_radf <- function(...) {
  print.ds_radf(...)
}

#' @export
tidy.ds_rev_radf <- function(...) {
  tidy.ds_radf(...)
}

# Plotting ----------------------------------------------------------------


#' @export
autoplot.rev_radf_obj <- function(object, cv = NULL, option = c("gsadf", "sadf"),
                                  min_duration = 0L, select_series = NULL,
                                  include_negative = FALSE, shade_opt = shade(), ...) {

  cv <- cv %||% retrieve_crit(object)
  assert_class(cv, "radf_cv")
  snames <- series_names(object)

  option <- match.arg(option)
  if (is_sb(cv)) {
    if (!is.null(select_series)) {
      stop_glue("argument 'select_series' have to be set to NULL ",
                "when cv is of class 'sb_cv'")
    }
    filter_option <- "rev_bsadf_panel" # overwrite option
    select_series <- "panel"
  }else{
    filter_option <- if (option == "gsadf") "rev_bsadf" else "rev_badf"
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

# autolayer.radf_obj <- function(ds_data) {}
