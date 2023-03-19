

# tidy --------------------------------------------------------------------

#' Tidy a `radf_obj` object
#'
#' Summarizes information about `radf_obj` object.
#'
#' @param x An object of class `radf_obj`.
#' @param format Long or wide format (default = "wide").
#' @param panel If TRUE then returns the panel statistics
#' @param ... Further arguments passed to methods. Not used.
#'
#' @importFrom purrr keep map reduce pluck
#' @importFrom dplyr full_join arrange
#' @importFrom rlang set_names
#' @importFrom tidyr gather
#' @importFrom tibble enframe
#'
#' @return A [tibble::tibble()]
#'
#' @export
#' @examples
#' \donttest{
#' dta <- data.frame(psy1 = sim_psy1(n = 100), psy2 = sim_psy2(n = 100))
#'
#' rfd <- radf(dta)
#'
#' # Get the test statistic
#' tidy(rfd)
#'
#' # Get the test statisticsequences
#' augment(rfd)
#'
#' # Get the panel test statistic
#' tidy(rfd, panel = TRUE)
#' }
tidy.radf_obj <- function(x, format = c("wide", "long"), panel = FALSE, ...) {

  format <- match.arg(format)

  if (panel) {
    tbl_radf <- pluck(x, "gsadf_panel") %>%
      enframe(name = NULL, value = "gsadf_panel")

    if (format == "long") {
      tbl_radf <- tbl_radf %>%
        gather(stat, tstat) %>%
        mutate(id = factor("panel"), stat = factor(stat)) %>%
        select(id, stat, tstat)
    }
  }else{
    tbl_radf <- x %>%
      keep(names(.) %in% c("adf", "sadf", "gsadf")) %>%
      map(enframe, name = "stat") %>%
      reduce(full_join, by = "stat") %>%
      set_names(c("id", "adf", "sadf", "gsadf")) %>%
      mutate(id = factor(id, series_names(x)))

    if (format == "long") {
      tbl_radf <- tbl_radf %>%
        gather(stat, tstat, -id) %>%
        mutate(stat = factor(stat, levels = c("adf", "sadf", "gsadf"))) %>%
        arrange(id)
    }
  }
  tbl_radf
}

# tidy-cv -----------------------------------------------------------------


#' Tidy a `radf_cv` object
#'
#' Summarizes information about `radf_cv` object.
#'
#' @param x An object of class `radf_cv`.
#' @inheritParams tidy.radf_obj
#'
#' @return A [tibble::tibble()]
#'
#' \itemize{
#' \item id: The series names.
#' \item sig: The significance level.
#' \item name: The name of the series (when format is "long").
#' \item crit: The critical value (when format is "long").
#' }
#'
#'
#' @importFrom purrr keep reduce
#' @importFrom rlang set_names
#' @importFrom dplyr full_join mutate
#' @importFrom tibble rownames_to_column enframe
#'
#' @export
#' @examples
#' \donttest{
#' mc <- radf_mc_cv(100)
#'
#' # Get the critical values
#' tidy(mc)
#'
#' # Get the critical value sequences
#' augment(mc)
#'
#' }
tidy.radf_cv <- function(x, format = c("wide", "long"), ...) {
  format <- match.arg(format)
  tidy_radf_cv(x, format)
}

tidy_radf_cv <- function(x, ...) {
  UseMethod("tidy_radf_cv")
}

tidy_radf_cv.mc_cv <- function(x, format = c("wide", "long"), ...) {

  tbl_cv <- x %>%
    keep(names(.) %in% c("adf_cv", "sadf_cv", "gsadf_cv")) %>%
    map(enframe, name = "stat") %>%
    reduce(full_join, by = "stat") %>%
    set_names(c("sig", "adf", "sadf", "gsadf")) %>%
    mutate(sig = sub("%$", "", sig) %>% as.factor())

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(stat, crit, -sig) %>%
      mutate(stat = factor(stat, levels = c("adf", "sadf", "gsadf"))) %>%
      select(stat, sig, crit)
  }

  tbl_cv
}

tidy_radf_cv.wb_cv <- function(x, format = c("wide", "long"), ...) {

  tbl_cv <- x %>%
    keep(names(.) %in% c("adf_cv", "sadf_cv", "gsadf_cv")) %>%
    map(~ .x %>%
          as.data.frame() %>%
          tibble::rownames_to_column() %>%
          as_tibble() %>%
          gather(stat, value, -rowname)) %>%
    reduce(full_join, by = c("rowname", "stat")) %>%
    set_names(c("id", "sig", "adf", "sadf", "gsadf")) %>%
    mutate(sig = gsub("%", "", sig) %>% as.factor()) %>%
    mutate(id = factor(id, levels = series_names(x)))

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(stat, crit, -id, -sig) %>%
      mutate(stat = factor(stat, levels = c("adf", "sadf", "gsadf"))) %>%
      select(id, stat, sig, crit)
  }

  tbl_cv
}

tidy_radf_cv.sb_cv <- function(x, format = c("wide", "long"), ...) {

  tbl_cv <- x %>%
    pluck("gsadf_panel_cv") %>%
    enframe(name = "sig", value = "gsadf_panel") %>%
    mutate(sig = sub("%$", "", sig) %>% as.factor()) %>%
    mutate(id = factor("panel")) %>%
    select(id, sig, gsadf_panel)

  if (format == "long") {

    tbl_cv <- tbl_cv %>%
      gather(stat, crit, -id, -sig) %>%
      mutate(stat = factor(stat)) %>%
      select(id, stat, sig, crit)
  }

  tbl_cv
}

# tidy-distr --------------------------------------------------------------


#' Tidy a `radf_distr` object
#'
#' Summarizes information about `radf_distr` object.
#'
#' @param x An object of class `radf_distr`.
#' @param ... Further arguments passed to methods. Not used.
#'
#' @return A [tibble::tibble()]
#'
#' @export
#' @examples
#' \dontrun{
#' mc <- mc_cv(n = 100)
#'
#' tidy(mc)
#' }
tidy.radf_distr <- function(x, ...) {
  tidy_radf_distr(x, ...)
}


tidy_radf_distr <- function(x, ...) {
  UseMethod("tidy_radf_distr")
}

#' @importFrom dplyr tibble
tidy_radf_distr.mc_distr <- function(x, ...) {
  tibble(
    adf = x$adf_distr,
    sadf = x$sadf_distr,
    gsadf = x$gsadf_distr
  )
}


#' @importFrom tidyr gather
#' @importFrom dplyr select bind_cols as_tibble
#' @importFrom purrr reduce pluck
tidy_radf_distr.wb_distr <- function(x, ...) {

  list(
    x %>%
      pluck("adf_distr") %>%
      as_tibble() %>%
      tidyr::gather(id, adf),
    x %>%
      pluck("sadf_distr") %>%
      as_tibble() %>%
      gather(id, sadf) %>%
      select(-id),
    x %>%
      pluck("gsadf_distr") %>%
      as_tibble() %>%
      gather(id, gsadf) %>%
      select(-id)
  ) %>%
    reduce(bind_cols)
}


tidy_radf_distr.sb_distr <- function(x, ...) {
  tibble(
    gsadf_panel = x
  )
}

# autoplot-distr ----------------------------------------------------------

#' Plotting a `radf_distr` object
#'
#' Takes a `radf_distr` object and returns a ggplot2 object.
#'
#' @param object An object of class `radf_distr`.
#' @param ... Further arguments passed to methods, used only in `wb_distr` facet options.
#'
#' @return A [ggplot2::ggplot()]
#'
#' @export
autoplot.radf_distr <- function(object, ...) {
  autoplot_radf_distr(object, ...)
}

autoplot_radf_distr <- function(object, ...) {
  UseMethod("autoplot_radf_distr")
}

#' @importFrom tidyr gather
#' @importFrom ggplot2 geom_density aes
autoplot_radf_distr.mc_distr <- function(object, ...) {

  object %>%
    tidy() %>%
    rename(ADF = adf, SADF = sadf, GSADF = gsadf) %>%
    tidyr::gather(Distribution, value, factor_key = TRUE) %>%
    ggplot(aes(value, fill = Distribution)) +
    geom_density(alpha = 0.2) +
    theme_bw() +
    labs(x = "", y = "",
         title = "Distributions of unit root test statistics")
}

#' @importFrom tidyr gather
#' @importFrom ggplot2 facet_wrap
autoplot_radf_distr.wb_distr <- function(object, ...) {

  object %>%
    tidy() %>%
    rename(ADF = adf, SADF = sadf, GSADF = gsadf) %>%
    tidyr::gather(Distribution, value, -id, factor_key = TRUE) %>%
    ggplot(aes(value, fill = Distribution)) +
    geom_density(alpha = 0.2) +
    theme_bw() +
    theme(strip.background = element_blank()) +
    facet_wrap(~ id, scales = "free", ...) +
    labs(x = "", y = "")
}

#' @export
autoplot_radf_distr.sb_distr <- function(object, ...) {

  object %>%
    tidy() %>%
    ggplot(aes(gsadf_panel, fill = gsadf_panel)) +
    scale_x_continuous() +
    geom_density(fill = "lightblue") +
    theme_bw() +
    labs(x = "", y = "", title = "Distribution of the panel GSADF statistic")
}




# tidy-join ---------------------------------------------------------------

#' Tidy into a joint model
#'
#' Tidy or augment and then join objects.
#'
#' @param x An object of class `obj`.
#' @param y An object of class `cv`.
#' @param ... Further arguments passed to methods.
#' @export
tidy_join <- function(x, y, ...) {
  UseMethod("tidy_join")
}

#' Tidy into a joint model
#'
#' Tidy or augment  and then join objects of class `radf_obj` and `radf_cv`. The
#' object of reference is the `radf_cv`. For example, if panel critical values
#' are provided the function will return the panel test statistic.
#'
#' @param x An object of class `radf_obj`.
#' @param y An object of class `radf_cv`. The output will depend on the type of
#' critical value.
#' @param  ... Further arguments passed to methods. Not used.
#'
#' @details `tidy_join` also calls `augment_join` when `cv` is of class `sb_cv`.
#'
#' @importFrom dplyr full_join case_when select_at
#' @export
tidy_join.radf_obj <- function(x, y = NULL, ...) {

  y <- y %||% retrieve_crit(x)
  assert_class(y, "radf_cv")
  assert_match(x, y)

  if (is_sb(y)) {
    if (utils::packageVersion("dplyr") >= "1.1.0.9000") {
      tbl <- inner_join(
        tidy(x, format = "long", panel = TRUE),
        tidy(y, format = "long"),
        by = c("id", "stat"),
        relationship = "many-to-many"
      )
    } else {
      # TODO: Remove this branch when dplyr 1.1.1 is on CRAN
      tbl <- inner_join(
        tidy(x, format = "long", panel = TRUE),
        tidy(y, format = "long"),
        by = c("id", "stat"),
        multiple = "all"
      )
    }

    tbl <- arrange(tbl, stat)
    return(tbl)
  }

  join_by <- if (!is_mc(y)) c("id") else NULL

  if (utils::packageVersion("dplyr") >= "1.1.0.9000") {
    tbl <- full_join(
      tidy(x, format = "long"),
      tidy(y, format = "long"),
      by = c("stat", join_by),
      relationship = "many-to-many"
    )
  } else {
    # TODO: Remove this branch when dplyr 1.1.1 is on CRAN
    tbl <- full_join(
      tidy(x, format = "long"),
      tidy(y, format = "long"),
      by = c("stat", join_by),
      multiple = "all"
    )
  }

  tbl %>%
    mutate(
      id = factor(id, levels = series_names(x)),
      stat = factor(stat, levels = c("adf", "sadf", "gsadf"))) %>%
    arrange(id, stat)
}


# augment -----------------------------------------------------------------

#' @param trunc Whether to remove the period of the minimum window from the plot (default = TRUE).
#' @rdname tidy.radf_obj
#'
#' @importFrom dplyr rename as_tibble everything
#' @importFrom tidyr gather pivot_longer drop_na
#' @export
augment.radf_obj <- function(x, format = c("wide", "long"), panel = FALSE, trunc = TRUE,  ...) {

  stopifnot(is.logical(panel))
  stopifnot(is.logical(trunc))
  format <- match.arg(format)

  if (panel) {
    tbl_radf <- extract_obj_stat(x, "bsadf_panel") %>%
      add_index(x) %>%
      select(key, index, bsadf_panel)

    if(format == "long") {
      tbl_radf <- tbl_radf %>%
        mutate(id = factor("panel")) %>%
        pivot_longer(
          bsadf_panel,
          names_to = "stat",
          names_ptypes = list(stat = factor()),
          values_to = "tstat"
        ) %>%
        select(key, index, id, stat, tstat)
    }

  }else{
    tbl_radf <- list(
      extract_obj_mat(x),
      extract_obj_stat(x, "badf"),
      extract_obj_stat(x, "bsadf")
    ) %>%
      reduce(full_join, by = c("key", "id"))

    if(format == "long") {
      tbl_radf <- tbl_radf %>%
        pivot_longer(
          cols = c(badf, bsadf),
          values_to = "tstat",
          names_to = "stat",
          names_ptypes = list(stat = factor())
        ) %>%
        mutate(stat = factor(stat, levels = c("badf", "bsadf")))
    }

  }
  if(trunc) {
    tbl_radf <- filter(tbl_radf, key > get_trunc(x))
  }
  tbl_radf
}

extract_obj_stat <- function(x, stat) {
  pluck(x, stat) %>%
    as_tibble() %>%
    na_pad_minw(x) %>%
    add_key(x, trunc = FALSE) %>%
    pivot_longer(
      cols = -key,
      names_to = "id",
      values_to = stat,
      values_ptypes = list(stat = factor())
    ) %>%
    select(key, everything())
}

extract_obj_mat <- function(x) {
  mat(x) %>%
    as_tibble %>%
    add_key(x, F) %>%
    mutate(index = index(x, trunc = FALSE)) %>%
    pivot_longer(
      cols = -c(key, index),
      names_to = "id",
      values_to = "data"
    ) %>%
    select(key, index, everything())
}


# augment-cv --------------------------------------------------------------

#' @param trunc Whether to remove the period of the minimum window from the plot (default = TRUE).
#' @rdname tidy.radf_cv
#' @inheritParams tidy.radf_cv
#'
#' @importFrom rlang as_double set_names
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble bind_cols mutate select bind_rows
#' @importFrom purrr pluck map2 reduce
#' @export
augment.radf_cv <- function(x, format = c("wide", "long"), trunc = TRUE, ...) {
  format <- match.arg(format)
  stopifnot(is.logical(trunc))
  augment_radf_cv(x, format, trunc = trunc)
}


augment_radf_cv <- function(x, ...) {
  UseMethod("augment_radf_cv")
}

augment_radf_cv.mc_cv <- function(x, format = c("wide", "long"), trunc = TRUE, ...) {

  stopifnot(is.logical(trunc))
  format <- match.arg(format)

  tbl_cv <- full_join(
    extract_cv_stat(x, "badf_cv"),
    extract_cv_stat(x, "bsadf_cv"),
    by = c("key", "sig")
  )

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(stat, crit, -sig, -key) %>%
      mutate(stat = factor(stat, levels = c("badf", "bsadf"))) %>%
      select(key, stat, sig, crit)
  }

  if(trunc) {
    tbl_cv <- filter(tbl_cv, key > get_trunc(x))
  }

  tbl_cv
}

extract_cv_stat <- function(x, stat = "bsadf_cv") {
  stat_name <-  gsub("_cv", "", stat)
  pluck(x, stat) %>%
    as_tibble() %>%
    na_pad_minw(x) %>%
    add_key(x, trunc = FALSE) %>%
    pivot_longer(-key, names_to = "sig", values_to = stat_name) %>%
    mutate(sig = as.factor(sub("%$", "", sig))) %>%
    select(key, everything())
}



augment_radf_cv.wb_cv <- function(x, format = c("wide", "long"), trunc = TRUE, ...) {

  tbl_cv <- full_join(
    extract_wb_stat(x, "badf_cv"),
    extract_wb_stat(x, "bsadf_cv"),
    by = c("key", "index", "id", "sig")
  ) %>%
    select(key, index, id, sig, everything())

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      pivot_longer(cols = c(badf, bsadf), values_to = "crit", names_to = "stat") %>%
      mutate(stat = factor(stat, levels = c("badf", "bsadf"))) %>%
      select(key, index, id, stat, sig, crit)
  }

  if(trunc) {
    tbl_cv <- filter(tbl_cv, key > get_trunc(x))
  }
  tbl_cv
}

extract_wb_stat <- function(x, stat = "badf_cv") {
  nms <- pluck(x, stat) %>% dimnames() %>% `[[`(3)
  stat_name <-  gsub("_cv", "", stat)
  array_to_list(x, stat) %>%
    set_names(nms) %>%
    map(as_tibble) %>%
    map(na_pad_minw, x) %>%
    map(add_key, x, FALSE) %>%
    map(add_index, x, FALSE) %>%
    map(pivot_longer, cols = c(-key, -index), names_to = "sig", values_to = stat_name) %>%
    bind_rows(.id = "id") %>%
    mutate(sig = as.factor(gsub("%", "", sig)))
}



augment_radf_cv.sb_cv <- function(x, format = c("wide", "long"), trunc = TRUE, ...) {

  tbl_cv <- extract_sb_stat(x)

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(stat, crit, -sig, -index, -key, factor_key = TRUE) %>%
      mutate(id = factor("panel"), stat = as.factor(stat)) %>%
      select(key, index, id, stat, sig, crit)
  }

  if(trunc) {
    tbl_cv <- filter(tbl_cv, key > get_trunc(x))
  }
  tbl_cv
}

extract_sb_stat <- function(x, stat = "bsadf_panel_cv") {
  stat_name <-  gsub("_cv", "", stat)
  pluck(x, stat) %>%
    as_tibble() %>%
    na_pad_minw(x) %>%
    add_key(x) %>%
    add_index(x) %>% # only difference with cv_stat
    pivot_longer(c(-key, -index), names_to = "sig", values_to = stat_name) %>%
    mutate(sig = as.factor(sub("%$", "", sig))) %>%
    select(key, everything())
}


# tidiers-join-radf -------------------------------------------------------



#' @rdname tidy_join
#' @export
augment_join <- function(x, y, ...) {
  UseMethod("augment_join")
}

#' @param trunc Whether to remove the period of the minimum window from the plot (default = TRUE).
#' @export
#' @rdname tidy_join.radf_obj
#' @importFrom dplyr inner_join select case_when all_of
augment_join.radf_obj <- function(x, y = NULL, trunc = TRUE, ...) {

  y <- y %||% retrieve_crit(x)
  assert_class(y, "radf_cv")
  assert_match(x, y)

  is_panel <- is_sb(y)
  join_by <- if (!is_mc(y)) c("id") else NULL
  is_idx_date <- is.Date(index(x))
  if (!is_idx_date && !is_mc(y)) join_by <- c("index", join_by)
  idx_if_date <- if (is_idx_date && !is_mc(y)) "index"  else NULL
  # key_if_date <- if (is_idx_date) "key"  else NULL
  id_lvls <- if (is_panel) "panel" else series_names(x)

  if (utils::packageVersion("dplyr") >= "1.1.0.9000") {
    tbl <- full_join(
      augment(x, "long", panel = is_panel, trunc = trunc),
      augment(y, "long", trunc = trunc) %>%
        select_at(vars(-all_of(idx_if_date))),
      by = c("key", "stat", join_by),
      relationship = "many-to-many"
    )
  } else {
    # TODO: Remove this branch when dplyr 1.1.1 is on CRAN
    tbl <- full_join(
      augment(x, "long", panel = is_panel, trunc = trunc),
      augment(y, "long", trunc = trunc) %>%
        select_at(vars(-all_of(idx_if_date))),
      by = c("key", "stat", join_by),
      multiple = "all"
    )
  }

  tbl %>%
    # drop_na(index) %>%
    mutate(id = factor(id, levels = id_lvls)) %>%
    arrange(id, stat, sig) #%>%
  # select_at(vars(-dplyr::all_of(key_if_date)))
}
# full_join(
#   augment(x, "long", panel = is_panel, trunc = trunc),
#   augment(y, "long", trunc = trunc),
#   by = c("key", "index", "id", "stat")
# )

