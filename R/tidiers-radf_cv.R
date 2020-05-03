
# tidy_radf_cv ---------------------------------------------------------------

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
    map(enframe) %>%
    reduce(full_join, by = "name") %>%
    set_names(c("sig", "adf", "sadf", "gsadf")) %>%
    mutate(sig = sub("%$", "", sig) %>% as.factor())

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(name, crit, -sig) %>%
      mutate(name = factor(name, levels = c("adf", "sadf", "gsadf"))) %>%
      select(name, sig, crit)
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
          gather(name, value, -rowname)) %>%
    reduce(full_join, by = c("rowname", "name")) %>%
    set_names(c("id", "sig", "adf", "sadf", "gsadf")) %>%
    mutate(sig = gsub("%", "", sig) %>% as.factor()) %>%
    mutate(id = factor(id, levels = series_names(x)))

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(name, crit, -id, -sig) %>%
      mutate(name = factor(name, levels = c("adf", "sadf", "gsadf"))) %>%
      select(id, name, sig, crit)
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
      gather(name, crit, -id, -sig) %>%
      mutate(name = factor(name)) %>%
      select(id, name, sig, crit)
  }

  tbl_cv
}


# augment_radf_cv ------------------------------------------------------------



#' @rdname tidy.radf_cv
#' @inheritParams tidy.radf_cv
#'
#' @importFrom rlang as_double set_names
#' @importFrom tidyr gather
#' @importFrom dplyr as_tibble bind_cols mutate select bind_rows
#' @importFrom purrr pluck map2 reduce
#' @export
augment.radf_cv <- function(x, format = c("wide", "long"), ...) {
  format <- match.arg(format)
  augment_radf_cv(x, format)
}


augment_radf_cv <- function(x, ...) {
  UseMethod("augment_radf_cv")
}

augment_radf_cv.mc_cv <- function(x, format = c("wide", "long"), ...) {

  tbl_cv <- inner_join(
    x %>%
      pluck("badf_cv") %>%
      as_tibble() %>%
      add_key(x) %>%
      gather(sig, badf, -key),
    x %>%
      pluck("bsadf_cv") %>%
      as_tibble() %>%
      add_key(x) %>%
      gather(sig, bsadf, -key),
    by = c("key", "sig")
  ) %>%
    mutate(sig = sub("%$", "", sig) %>% as.factor())

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(name, crit, -sig, -key) %>%
      mutate(name = factor(name, levels = c("badf", "bsadf"))) %>%
      select(key, name, sig, crit)
  }

  tbl_cv
}

augment_radf_cv.wb_cv <- function(x, format = c("wide", "long"), ...) {

  iternames <- x %>% pluck("badf_cv") %>% dimnames() %>% `[[`(3)

  tbl_cv <-
    bind_rows(
      x %>%
        array_to_list("badf_cv") %>%
        set_names(iternames) %>%
        map(as_tibble) %>%
        map2(iternames, ~ .x %>% gather(sig, !!(.y))) %>%
        reduce(bind_cols) %>%
        mutate(name = "badf") %>%
        select(sig, name, iternames),
      x %>%
        array_to_list("bsadf_cv") %>%
        set_names(iternames) %>%
        map(as_tibble) %>%
        map2(iternames, ~ .x %>% gather(sig, !!(.y))) %>%
        reduce(bind_cols) %>%
        mutate(name = "bsadf") %>%
        select(sig, name, iternames)
    ) %>%
    mutate(index = rep(index(x, trunc = TRUE), 6)) %>% # n_sig * n_name
    mutate(key = rep((get_minw(x) + 1):(nrow(.)/6 + get_minw(x)),6)) %>%
    mutate(sig = gsub("%", "", sig) %>% as.factor()) %>%
    select(key, index, sig, name , everything())

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(id, crit, -key, -index, -sig, -name, factor_key = TRUE) %>%
      mutate(name = factor(name, levels = c("badf", "bsadf"))) %>%
      select(key, index, id, name, sig, crit)
  }

  tbl_cv
}

augment_radf_cv.sb_cv <- function(x, format = c("wide", "long"), ...) {

  tbl_cv <- x %>%
    pluck("bsadf_panel_cv") %>%
    as_tibble() %>%
    add_column(index = index(x, trunc = TRUE)) %>%
    add_key(x) %>%
    gather(sig, bsadf_panel, -index, -key, factor_key = TRUE) %>%
    mutate(sig = sub("%$", "", sig) %>% as.factor()) %>%
    select(key, index, everything())

  if (format == "long") {
    tbl_cv <- tbl_cv %>%
      gather(name, crit, -sig, -index, -key, factor_key = TRUE) %>%
      mutate(id = factor("panel"), name = as.factor(name)) %>%
      select(key, index, id, name, sig, crit)
  }

  tbl_cv
}



# Tidying distr -----------------------------------------------------------


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



# Plotting distr ----------------------------------------------------------


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

autoplot_radf_distr <- function(x, ...) {
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
